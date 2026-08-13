(ns re-frame.vertica.tracker-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [re-frame.vertica.shared :as shared]
            [re-frame.vertica.tracker :as tracker]))

(defrecord Person [name roles])

(def db
  {:user {:name "Ada" :address {:city "London"}}
   :items [{:id 1} {:id 2}]
   :history (list {:id 3} {:id 4})
   :flags #{:a :b}
   :person (->Person "Grace" #{:author})})

(defn paths [f & [query dyn]]
  (:paths (tracker/replay f db (or query [:test]) dyn)))

(deftest lookup-paths
  (testing "keyword calls and get/get-in"
    (is (= [[:user]] (paths (fn [db _] (:user db)))))
    (is (= [[:user :name]] (paths (fn [db _] (get-in db [:user :name])))))
    (is (= [[:user :address :city]]
           (paths (fn [db _] (-> db :user :address :city)))))
    (is (= [[:user :missing]]
           (paths (fn [db _] (get-in db [:user :missing]))))))
  (testing "destructuring"
    (is (= [[:user :name]]
           (paths (fn [db _] (let [{{:keys [name]} :user} db] name))))))
  (testing "indexed access"
    (is (= [[:items 1 :id]]
           (paths (fn [db _] (get-in db [:items 1 :id])))))
    (is (= [[:items 99]]
           (paths (fn [db _] (get (:items db) 99)))))
    (is (= #{[:history] [:history 0 :id]}
           (set (paths (fn [db _] (:id (first (:history db))))))))))

(deftest membership-and-traversal
  (is (= [[:flags :a]]
         (paths (fn [db _] (contains? (:flags db) :a)))))
  (is (= [[:user :missing]]
         (paths (fn [db _] (contains? (:user db) :missing)))))
  (is (= #{[:user] [:user :name]}
         (set (paths (fn [db _] (keys (:user db)))))))
  (is (= [[:items 0] [:items 1]]
         (paths (fn [db _]
                  (reduce (fn [n _] (inc n)) 0 (:items db))))))
  (is (= #{[:person] [:person :name]}
         (set (paths (fn [db _] (vals (:person db))))))))

(deftest traversal-is-concrete-and-lazy
  (testing "short-circuiting records only consumed indexes"
    (is (= #{[:items] [:items 0]}
           (set (paths (fn [db _] (first (:items db)))))))
    (is (= #{[:items] [:items 0 :id]}
           (set (paths (fn [db _] (:id (first (:items db)))))))))
  (testing "count and empty collections depend on collection structure"
    (is (= [[:items]]
           (paths (fn [db _] (count (:items db))))))
    (is (= [[:empty]]
           (:paths (tracker/replay (fn [db _] (seq (:empty db)))
                                   {:empty []} [:test] nil)))))
  (testing "kv-reduce records each entry it reaches"
    (is (= #{[:user] [:user :name]}
           (set (paths (fn [db _]
                         (reduce-kv (fn [_ key _] (reduced key)) nil (:user db)))))))
    (is (= #{[:user :address] [:user :name]}
           (set (paths (fn [db _]
                         (reduce-kv (fn [result key _] (conj result key))
                                    [] (:user db))))))))
  (testing "fully realized lazy traversal drops its aggregate marker"
    (is (= #{[:user :address] [:user :name]}
           (set (paths (fn [db _]
                         (doall (keys (:user db))))))))))

(deftest query-and-dynamic-arguments
  (is (= [[:items 1 :id]]
         (paths (fn [db [_ index]] (get-in db [:items index :id])) [:test 1])))
  (is (= [[:items 0 :id]]
         (paths (fn [db _ [index]] (get-in db [:items index :id])) [:test] [0]))))

(deftest partial-provenance
  (let [result (tracker/replay
                 (fn [db _]
                   (:name (:user db))
                   (assoc (:user db) :x 1))
                 db [:test] nil)]
    (is (false? (:complete? result)))
    (is (= :assoc (:operation result)))
    (is (= [[:user :name]] (:paths result))))
  (let [result (tracker/replay
                 (fn [db _]
                   (:name (:user db))
                   (throw (js/Error. "boom")))
                 db [:test] nil)]
    (is (= [[:user :name]] (:paths result)))
    (is (re-find #"boom" (:reason result)))))

(deftest collection-heavy-replays-have-no-path-count-limit
  (let [large-db {:rows (mapv #(hash-map :id %) (range 1600))}
        result (tracker/replay
                 (fn [db _] (mapv :id (:rows db)))
                 large-db [:large] nil)]
    (is (:complete? result))
    (is (= 1600 (count (:paths result))))))
