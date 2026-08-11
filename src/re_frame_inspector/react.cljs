(ns re-frame-inspector.react
  (:require [clojure.string :as str]
            [goog.object :as gobj]))

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
  (let [version (some-> js/globalThis (gobj/get "React") (gobj/get "version"))]
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

(defn- function-name [f]
  (or (when f (gobj/get f "displayName"))
      (when f (gobj/get f "name"))))

(defn component-name [fiber]
  (let [state (gobj/get fiber "stateNode")
        type (gobj/get fiber "type")
        props (gobj/get fiber "memoizedProps")
        argv (when props (gobj/get props "argv"))
        reagent-render (or (some-> state (gobj/get "reagentRender"))
                           (when (seq argv) (first argv)))]
    (or (function-name reagent-render)
        (function-name type)
        (some-> type (gobj/get "render") function-name)
        "Anonymous component")))

(def component-tags #{0 1 2 9 10 11 14 15})

(defn owning-components [element]
  (if-not (supported-react?)
    {:components []
     :warning {:code :unsupported-react
               :message (str "React " (or (react-major) "unknown")
                             " is unsupported; install React 17 or 18.")}}
    (if-let [fiber (fiber-for-element element)]
      (loop [cursor fiber result [] unsupported []]
        (if-not cursor
          {:components (vec (reverse result)) :unsupported unsupported}
          (let [tag (gobj/get cursor "tag")]
            (if (contains? component-tags tag)
              (let [reaction (render-reaction cursor)]
                (recur (gobj/get cursor "return")
                       (conj result {:id (object-id "component" cursor)
                                     :fiber cursor
                                     :name (component-name cursor)
                                     :reaction reaction
                                     :adapter (if (gobj/get cursor "stateNode") :class :functional)})
                       (cond-> unsupported
                         (nil? reaction)
                         (conj {:code :unsupported-fiber
                                :message (str (component-name cursor)
                                              " has no supported Reagent render reaction.")}))))
              (recur (gobj/get cursor "return") result unsupported)))))
      {:components []
       :warning {:code :missing-react-fiber
                 :message "The selected DOM node is not owned by a detectable React 17/18 fiber."}})))

(defn element-label [element]
  (when element
    (str (str/lower-case (.-tagName element))
         (when-let [id (not-empty (.-id element))] (str "#" id))
         (when-let [classes (not-empty (.-className element))]
           (when (string? classes)
             (str "." (str/replace classes #"\s+" ".")))))))
