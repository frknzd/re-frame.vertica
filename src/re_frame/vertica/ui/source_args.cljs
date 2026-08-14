(ns re-frame.vertica.ui.source-args
  (:require [clojure.string :as str]))

(def ^:private open-to-close {"(" ")" "[" "]" "{" "}"})
(def ^:private closing-delimiters (set (vals open-to-close)))

(defn- char-at [source index]
  (when (< index (count source)) (subs source index (inc index))))

(defn- tokenize [source]
  (let [length (count source)]
    (loop [index 0 tokens []]
      (if (>= index length)
        tokens
        (let [start index
              character (char-at source index)]
          (cond
            (re-find #"[\s,]" character)
            (recur (inc index) tokens)

            (= ";" character)
            (recur (loop [cursor (inc index)]
                     (if (and (< cursor length) (not= "\n" (char-at source cursor)))
                       (recur (inc cursor)) cursor))
                   tokens)

            (= "\"" character)
            (let [end (loop [cursor (inc index)]
                        (if (>= cursor length)
                          cursor
                          (let [current (char-at source cursor)]
                            (cond
                              (= "\\" current) (recur (min length (+ cursor 2)))
                              (= "\"" current) (inc cursor)
                              :else (recur (inc cursor))))))]
              (recur end (conj tokens {:type :string :start start :end end})))

            (or (contains? open-to-close character)
                (contains? closing-delimiters character))
            (recur (inc index)
                   (conj tokens {:type :delimiter :value character
                                 :start start :end (inc index)}))

            :else
            (let [end (loop [cursor (inc index)]
                        (if (and (< cursor length)
                                 (not (re-find #"[\s,;()\[\]{}\"]"
                                               (char-at source cursor))))
                          (recur (inc cursor)) cursor))]
              (recur end (conj tokens {:type :atom
                                       :value (subs source start end)
                                       :start start :end end})))))))))

(defn- parse-forms [source]
  (let [tree (atom {:type :root :children [] :start 0 :end (count source)})
        stack (atom [[]])]
    (doseq [token (tokenize source)]
      (let [current-path (peek @stack)
            value (:value token)]
        (cond
          (contains? open-to-close value)
          (let [form {:type (case value "(" :list "[" :vector "{" :map)
                      :open value :close (get open-to-close value)
                      :children [] :start (:start token) :end (:end token)}
                index (count (get-in @tree (conj current-path :children)))
                form-path (conj current-path :children index)]
            (swap! tree update-in (conj current-path :children) conj form)
            (swap! stack conj form-path))

          (= :delimiter (:type token))
          (when (and (> (count @stack) 1)
                     (= value (:close (get-in @tree current-path))))
            (swap! tree assoc-in (conj current-path :end) (:end token))
            (swap! stack pop))

          :else
          (swap! tree update-in (conj current-path :children) conj token))))
    @tree))

(defn- atom-value [form]
  (when (= :atom (:type form)) (:value form)))

(defn- form-text [source form]
  (-> (subs source (:start form) (:end form))
      (str/replace #"\s+" " ")
      str/trim))

(defn- source-position [source offset]
  (let [lines (str/split (subs source 0 offset) #"\n" -1)]
    {:line (count lines) :column (inc (count (last lines)))}))

(defn- definition-name-index
  ([children] (definition-name-index children 1))
  ([children start]
   (loop [index start metadata-pending? false]
     (when (< index (count children))
       (let [value (atom-value (nth children index))]
         (cond
           metadata-pending? (recur (inc index) false)
           (= "^" value) (recur (inc index) true)
           (str/starts-with? (or value "") "^") (recur (inc index) false)
           value index
           :else (recur (inc index) false)))))))

(defn- signature-vectors [definition name-index]
  (let [tail (subvec (vec (:children definition)) (inc name-index))]
    (if-let [direct (first (filter #(= :vector (:type %)) tail))]
      [direct]
      (->> tail
           (filter #(= :list (:type %)))
           (keep #(first (filter (fn [child] (= :vector (:type child))) (:children %))))
           vec))))

(defn- nested-fn-signatures [definition name-index]
  (loop [queue (vec (drop (inc name-index) (:children definition))) index 0]
    (when (< index (count queue))
      (let [form (nth queue index)
            children (:children form)]
        (if (and (= :list (:type form))
                 (contains? #{"fn" "fn*"} (atom-value (first children))))
          (let [initial-offset 1
                offset (if (and (atom-value (nth children initial-offset nil))
                                (not= :vector (:type (nth children (inc initial-offset) nil))))
                         (inc initial-offset)
                         initial-offset)
                tail (drop offset children)]
            (if-let [direct (first (filter #(= :vector (:type %)) tail))]
              [direct]
              (->> tail
                   (filter #(= :list (:type %)))
                   (keep #(first (filter (fn [child] (= :vector (:type child))) (:children %))))
                   vec)))
          (recur (into queue children) (inc index)))))))

(defn- argument-label [source form]
  (if-let [value (atom-value form)]
    (str/replace value #"^\^\S+\s*" "")
    (form-text source form)))

(defn- parameter-forms [signature]
  (loop [forms (:children signature) index 0 result []]
    (if (>= index (count forms))
      result
      (let [form (nth forms index)
            value (atom-value form)]
        (cond
          (= "^" value) (recur forms (+ index 2) result)
          (str/starts-with? (or value "") "^") (recur forms (inc index) result)
          :else (recur forms (inc index) (conj result form)))))))

(defn- expand-signature [source signature arity]
  (let [forms (parameter-forms signature)
        rest-index (first (keep-indexed #(when (= "&" (atom-value %2)) %1) forms))
        fixed (if (some? rest-index) (subvec (vec forms) 0 rest-index) (vec forms))
        rest-form (when (some? rest-index) (nth forms (inc rest-index) nil))]
    (when (or (and (nil? rest-form) (= (count fixed) arity))
              (and rest-form (>= arity (count fixed))))
      (let [labels (mapv #(argument-label source %) fixed)]
        (if rest-form
          (into labels
                (map #(str (argument-label source rest-form)
                           "[" (- % (count fixed)) "]")
                     (range (count fixed) arity)))
          labels)))))

(defn- namespace-name [root]
  (some (fn [form]
          (when (and (= :list (:type form))
                     (= "ns" (atom-value (first (:children form)))))
            (some->> (definition-name-index (:children form))
                     (nth (:children form))
                     atom-value)))
        (:children root)))

(defn index-clojurescript-source
  ([source] (index-clojurescript-source source {} ""))
  ([source index] (index-clojurescript-source source index ""))
  ([source index url]
   (let [root (parse-forms source)
         namespace (namespace-name root)]
     (if-not namespace
       index
       (reduce
         (fn [result form]
           (let [operator (when (= :list (:type form))
                            (atom-value (first (:children form))))]
             (if (contains? #{"defn" "defn-" "def" "defonce"} operator)
               (let [name-index (definition-name-index (:children form))
                     local-name (some->> name-index (nth (:children form)) atom-value)]
                 (if local-name
                   (assoc result (str namespace "/" local-name)
                          (merge {:source source
                                  :signatures (if (str/starts-with? operator "defn")
                                                (signature-vectors form name-index)
                                                (nested-fn-signatures form name-index))
                                  :url url}
                                 (source-position source (:start form))))
                   result))
               result)))
         index
         (tree-seq :children :children root))))))

(defn resolve-argument-names [index component-name arity]
  (when-let [definition (get index component-name)]
    (when (and (int? arity) (not (neg? arity)))
      (some #(expand-signature (:source definition) % arity)
            (:signatures definition)))))

(defn resolve-source-location [index component-name]
  (when-let [{:keys [url line column]} (get index component-name)]
    (when (seq url)
      {:component-name component-name :url url :line line :column column})))

(defn- absolute-source-url [source source-root map-url]
  (let [rooted (if (seq source-root)
                 (str (str/replace source-root #"/$" "") "/"
                      (str/replace source #"^/" ""))
                 source)]
    (try (.-href (js/URL. rooted map-url))
         (catch :default _ rooted))))

(declare collect-map)

(defn- collect-map [source-map result map-url]
  (let [result (reduce (fn [acc section]
                         (collect-map (:map section) acc map-url))
                       result
                       (or (:sections source-map) []))]
    (reduce-kv
      (fn [acc index content]
        (let [source (get (:sources source-map) index "")]
          (if (and (string? content) (re-find #"\.clj[sc]?(?:$|[?#])" source))
            (conj acc {:url (absolute-source-url source (:sourceRoot source-map) map-url)
                       :content content})
            acc)))
      result
      (vec (or (:sourcesContent source-map) [])))))

(defn embedded-sources
  ([source-map] (embedded-sources source-map ""))
  ([source-map map-url]
   (let [parsed (if (string? source-map)
                  (js->clj (js/JSON.parse source-map) :keywordize-keys true)
                  source-map)]
     (collect-map parsed [] map-url))))

(defn add-source-resource [index url content]
  (cond
    (re-find #"\.map(?:$|[?#])" url)
    (reduce (fn [result source]
              (if (re-find #"\.clj[sc]?(?:$|[?#])" (:url source))
                (index-clojurescript-source (:content source) result (:url source))
                result))
            index
            (embedded-sources content url))

    (re-find #"\.clj[sc]?(?:$|[?#])" url)
    (index-clojurescript-source content index url)

    :else index))
