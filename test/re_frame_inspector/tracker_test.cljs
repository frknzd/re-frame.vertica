(ns re-frame-inspector.tracker-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [re-frame-inspector.shared :as shared]
            [re-frame-inspector.tracker :as tracker]))

(defrecord Person [name roles])

(def db
  {:user {:name "Ada" :address {:city "London"}}
   :items [{:id 1} {:id 2}]
   :flags #{:a :b}
   :person (->Person "Grace" #{:author})})

(defn paths [f & [query dyn]]
  (:paths (tracker/replay f db (or query [:test]) dyn)))

(deftest lookup-paths
  (testing "keyword calls and get/get-in"
    (is (= [[:user]] (paths (fn [db _] (:user db)))))
    (is (= [[:user :name]] (paths (fn [db _] (get-in db [:user :name])))))
    (is (= [[:user :address :city]]
           (paths (fn [db _] (-> db :user :address :city))))))
  (testing "destructuring"
    (is (= [[:user :name]]
           (paths (fn [db _] (let [{{:keys [name]} :user} db] name))))))
  (testing "indexed access"
    (is (= [[:items 1 :id]]
           (paths (fn [db _] (get-in db [:items 1 :id])))))))

(deftest membership-and-traversal
  (is (= [[:flags :a]]
         (paths (fn [db _] (contains? (:flags db) :a)))))
  (is (= [[:user :missing]]
         (paths (fn [db _] (contains? (:user db) :missing)))))
  (is (= [[:user shared/wildcard]]
         (paths (fn [db _] (keys (:user db))))))
  (is (= [[:items shared/wildcard]]
         (paths (fn [db _]
                  (reduce (fn [n _] (inc n)) 0 (:items db))))))
  (is (= [[:person shared/wildcard]]
         (paths (fn [db _] (vals (:person db)))))))

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
