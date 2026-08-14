(ns re-frame.vertica.ui.edn-tokenizer
  (:require [clojure.string :as str]))

(def ^:private openers #{\( \[ \{})
(def ^:private closers #{\) \] \}})
(def ^:private matching-opener {\) \( \] \[ \} \{})

(defn- delimiter? [character]
  (or (nil? character)
      (boolean (re-find #"[\s,()\[\]{}\";]" (str character)))))

(defn- token-type [value]
  (cond
    (str/starts-with? value ":") :keyword
    (str/starts-with? value "#") :tag
    (str/starts-with? value "\\") :character
    (re-matches #"(?:nil|true|false)" value) :literal
    (re-matches #"[+-]?(?:\d+r[0-9A-Za-z]+|0[xX][0-9A-Fa-f]+|\d+(?:\.\d+)?(?:[eE][+-]?\d+)?[MN]?|\d+/\d+)" value) :number
    :else :symbol))

(defn- append-token [tokens text type depth]
  (if (empty? text)
    tokens
    (let [previous (peek tokens)]
      (if (and (= :plain type) (= type (:type previous)) (= depth (:depth previous)))
        (update-in tokens [(dec (count tokens)) :text] str text)
        (conj tokens (cond-> {:text text :type type}
                       (some? depth) (assoc :depth depth)))))))

(defn edn-tokens [input]
  (let [text (str (or input ""))
        length (count text)]
    (loop [index 0 tokens [] stack []]
      (if (>= index length)
        tokens
        (let [character (nth text index)]
          (cond
            (re-find #"[\s,]" (str character))
            (let [end (loop [cursor (inc index)]
                        (if (and (< cursor length)
                                 (re-find #"[\s,]" (str (nth text cursor))))
                          (recur (inc cursor))
                          cursor))]
              (recur end (append-token tokens (subs text index end) :plain nil) stack))

            (= \; character)
            (let [end (loop [cursor (inc index)]
                        (if (and (< cursor length) (not= \newline (nth text cursor)))
                          (recur (inc cursor)) cursor))]
              (recur end (append-token tokens (subs text index end) :comment nil) stack))

            (= \" character)
            (let [end (loop [cursor (inc index) escaped? false]
                        (if (>= cursor length)
                          cursor
                          (let [current (nth text cursor)]
                            (cond
                              escaped? (recur (inc cursor) false)
                              (= \\ current) (recur (inc cursor) true)
                              (= \" current) (inc cursor)
                              :else (recur (inc cursor) false)))))]
              (recur end (append-token tokens (subs text index end) :string nil) stack))

            (= \\ character)
            (let [start index
                  cursor (min length (+ index 2))
                  end (loop [cursor cursor]
                        (if (and (< cursor length) (not (delimiter? (nth text cursor))))
                          (recur (inc cursor)) cursor))]
              (recur end (append-token tokens (subs text start end) :character nil) stack))

            (contains? openers character)
            (recur (inc index)
                   (append-token tokens (str character) :bracket (count stack))
                   (conj stack character))

            (contains? closers character)
            (let [depth (max 0 (dec (count stack)))
                  matching? (= (peek stack) (get matching-opener character))]
              (recur (inc index)
                     (append-token tokens (str character) :bracket
                                   (if matching? depth (count stack)))
                     (if matching? (pop stack) stack)))

            :else
            (let [end (loop [cursor (inc index)]
                        (if (and (< cursor length) (not (delimiter? (nth text cursor))))
                          (recur (inc cursor)) cursor))
                  value (subs text index end)]
              (recur end (append-token tokens value (token-type value) nil) stack))))))))
