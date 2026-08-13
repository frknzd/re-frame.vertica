(ns re-frame.vertica.react
  (:require [clojure.string :as str]
            [goog.object :as gobj]
            [react :as react]))

(defonce object-ids (js/WeakMap.))
(defonce next-id (atom 0))

(defn object-id [prefix object]
  (if-not object
    (str prefix "-unknown")
    (or (.get object-ids object)
        (let [id (str prefix "-" (swap! next-id inc))]
          (.set object-ids object id)
          id))))

(defn react-major []
  (let [version (or (gobj/get react "version")
                    (some-> js/globalThis (gobj/get "React") (gobj/get "version")))]
    (when-let [match (and version (re-find #"^(\d+)" version))]
      (js/parseInt (second match) 10))))

(defn supported-react? [] (contains? #{17 18} (react-major)))

(defn fiber-for-element [element]
  (when element
    (let [keys (js/Object.keys element)
          key (some #(when (or (str/starts-with? % "__reactFiber$")
                               (str/starts-with? % "__reactInternalInstance$")) %)
                    (array-seq keys))]
      (when key (gobj/get element key)))))

(defn- hook-reaction [fiber]
  (loop [hook (gobj/get fiber "memoizedState")]
    (when hook
      (let [state (gobj/get hook "memoizedState")
            current (when (and state (object? state)) (gobj/get state "current"))
            reaction (when current (gobj/get current "cljsRatom"))]
        (or reaction (recur (gobj/get hook "next")))))))

(defn render-reaction [fiber]
  (or (some-> (gobj/get fiber "stateNode") (gobj/get "cljsRatom"))
      (hook-reaction fiber)))

(defn- hook-render-state [fiber reaction]
  (loop [hook (gobj/get fiber "memoizedState")]
    (when hook
      (let [state (gobj/get hook "memoizedState")
            current (when (and state (object? state)) (gobj/get state "current"))]
        (if (and current (identical? reaction (gobj/get current "cljsRatom")))
          current
          (recur (gobj/get hook "next")))))))

(defn component-argv-slot [{:keys [fiber reaction]}]
  (let [state-node (gobj/get fiber "stateNode")
        class-props (some-> state-node (gobj/get "props"))
        functional-state (hook-render-state fiber reaction)
        target (cond
                 (and (object? class-props) (some? (gobj/get class-props "argv"))) class-props
                 (and (object? functional-state) (some? (gobj/get functional-state "argv"))) functional-state
                 :else nil)]
    (when target
      {:target target :value (gobj/get target "argv")})))

(defn- function-names [f]
  (when f
    (remove nil?
            [(gobj/get f "displayName")
             (gobj/get f "name")
             (try (some-> f meta :name str)
                  (catch :default _ nil))])))

(defn- generated-component-name? [value]
  (let [value (some-> value str str/trim)]
    (or (str/blank? value)
        (boolean (re-matches #"(?:G__|G_)[0-9]+" value))
        (contains? #{"Object" "Function" "Component" "ReactComponent" "cmp" "f"} value))))

(defn normalize-component-name [value]
  (when-not (generated-component-name? value)
    (let [raw (-> (str value)
                  str/trim
                  (str/replace #"\$cljs\$.*$" ""))
          qualified
          (cond
            (str/includes? raw "/") raw

            (str/includes? raw "$")
            (let [parts (vec (remove str/blank? (str/split raw #"\$")))]
              (if (> (count parts) 1)
                (str (str/join "." (pop parts)) "/" (peek parts))
                raw))

            (str/includes? raw ".")
            (let [parts (str/split raw #"\.")]
              (str (str/join "." (butlast parts)) "/" (last parts)))

            :else raw)]
      (str/replace qualified "_" "-"))))

(defn- first-useful-name [sources]
  (some normalize-component-name sources))

(defn component-name [fiber]
  (let [state (gobj/get fiber "stateNode")
        type (gobj/get fiber "type")
        props (gobj/get fiber "memoizedProps")
        argv (when props (gobj/get props "argv"))
        argv-component (when (seq argv) (first argv))
        reagent-render (some-> state (gobj/get "reagentRender"))
        constructor (some-> state (gobj/get "constructor"))
        type-render (some-> type (gobj/get "render"))]
    (or (first-useful-name
          (concat
            [(some-> constructor (gobj/get "displayName"))]
            (function-names argv-component)
            (function-names reagent-render)
            (function-names type)
            (function-names type-render)
            [(some-> constructor (gobj/get "name"))]))
        (str "Anonymous Reagent component (" (object-id "component" fiber) ")"))))

(defn component-arguments [fiber]
  (let [props (gobj/get fiber "memoizedProps")
        argv (when props (gobj/get props "argv"))
        values (cond
                 (array? argv) (vec (array-seq argv))
                 (sequential? argv) (vec argv)
                 :else [])]
    (if (seq values) (subvec values 1) [])))

(def component-tags #{0 1 2 9 10 11 14 15})
(def host-tags #{5 6})

(defn direct-reagent-owner
  "Return the nearest Reagent owner when element is a host root emitted by that
  component. Crossing another host fiber means the element is only a DOM
  descendant of the component, not one of its rendered roots."
  [element]
  (when-let [fiber (fiber-for-element element)]
    (loop [cursor (gobj/get fiber "return")]
      (when cursor
        (let [tag (gobj/get cursor "tag")
              reaction (when (contains? component-tags tag)
                         (render-reaction cursor))]
          (cond
            reaction {:id (object-id "component" cursor)
                      :fiber cursor
                      :name (component-name cursor)
                      :reaction reaction}
            (contains? host-tags tag) nil
            :else (recur (gobj/get cursor "return"))))))))

(defn selected-element-route [element component]
  (when (and element component)
    (loop [candidate element indices []]
      (when candidate
        (let [owner (direct-reagent-owner candidate)]
          (if (= (:id component) (:id owner))
            {:root {:tag (.-tagName candidate)
                    :id (or (.-id candidate) "")
                    :classes (if (string? (.-className candidate)) (.-className candidate) "")}
             :indices indices}
            (when-let [parent (.-parentElement candidate)]
              (let [siblings (array-seq (.-children parent))
                    index (first (keep-indexed #(when (identical? %2 candidate) %1) siblings))]
                (when (some? index)
                  (recur parent (into [index] indices)))))))))))

(defn reagent-root-element? [element]
  (boolean (direct-reagent-owner element)))

(defn- composed-parent-element [element]
  (or (.-parentElement element)
      (when-let [get-root (.-getRootNode element)]
        (some-> (.call get-root element) .-host))))

(defn nearest-reagent-root-element [element]
  (loop [candidate element]
    (cond
      (nil? candidate) nil
      (reagent-root-element? candidate) candidate
      :else (recur (composed-parent-element candidate)))))

(defn owning-components
  ([element] (owning-components element js/Number.MAX_SAFE_INTEGER))
  ([element limit]
   (if-not (supported-react?)
     {:components []
      :warning {:code :unsupported-react
                :message (str "React " (or (react-major) "unknown")
                              " is unsupported; install React 17 or 18.")}}
     (if-let [fiber (fiber-for-element element)]
       (loop [cursor fiber result []]
         (if-not cursor
           {:components (vec (reverse result))}
           (let [tag (gobj/get cursor "tag")]
             (if (contains? component-tags tag)
               (if (>= (count result) limit)
                 {:components (vec (reverse result)) :truncated? true}
                 (let [reaction (render-reaction cursor)]
                   (recur (gobj/get cursor "return")
                          (conj result {:id (object-id "component" cursor)
                                        :fiber cursor
                                        :name (component-name cursor)
                                        :arguments (component-arguments cursor)
                                        :reaction reaction
                                        :adapter (if (gobj/get cursor "stateNode") :class :functional)}))))
               (recur (gobj/get cursor "return") result)))))
       {:components []
        :warning {:code :missing-react-fiber
                  :message "The selected DOM node is not owned by a detectable React 17/18 fiber."}}))))

(defn element-label [element]
  (when element
    (str (str/lower-case (.-tagName element))
         (when-let [id (not-empty (.-id element))] (str "#" id))
         (when-let [classes (not-empty (.-className element))]
           (when (string? classes)
             (str "." (str/replace classes #"\s+" ".")))))))
