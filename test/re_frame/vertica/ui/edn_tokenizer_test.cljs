(ns re-frame.vertica.ui.edn-tokenizer-test
  (:require [cljs.test :refer-macros [deftest is]]
            [re-frame.vertica.ui.edn-tokenizer :as tokenizer]))

(deftest matching-brackets-share-depth
  (let [brackets (filter #(= :bracket (:type %))
                         (tokenizer/edn-tokens "[:a {:b [1 (2)]}]"))]
    (is (= [["[" 0] ["{" 1] ["[" 2] ["(" 3]
            [")" 3] ["]" 2] ["}" 1] ["]" 0]]
           (mapv (juxt :text :depth) brackets)))))

(deftest recognizes-edn-types
  (let [tokens (tokenizer/edn-tokens
                 "{:name \"Ada\" :age 37 :active true :missing nil :id #uuid \"abc\" :initial \\A}")
        by-text (into {} (map (juxt :text :type)) tokens)]
    (is (= :keyword (get by-text ":name")))
    (is (= :string (get by-text "\"Ada\"")))
    (is (= :number (get by-text "37")))
    (is (= :literal (get by-text "true")))
    (is (= :literal (get by-text "nil")))
    (is (= :tag (get by-text "#uuid")))
    (is (= :character (get by-text "\\A")))))

(deftest ignores-brackets-in-strings-and-character-literals
  (is (= ["{" "}"]
         (->> (tokenizer/edn-tokens "{:text \"[not brackets]\" :character \\[}")
              (filter #(= :bracket (:type %)))
              (mapv :text)))))

(deftest unbalanced-input-remains-renderable
  (is (= [{:text "]" :type :bracket :depth 0}]
         (tokenizer/edn-tokens "]"))))
