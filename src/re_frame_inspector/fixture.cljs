(ns re-frame-inspector.fixture
  (:require [re-frame.core :as rf]
            [reagent.core :as r]
            [reagent.dom :as rdom]
            [re-frame-inspector.preload]))

(rf/reg-event-db ::initialize
  (fn [_ _]
    {:people {1 {:name "Ada" :roles #{:admin :author}}
              2 {:name "Grace" :roles #{:author}}}
     :selected-id 1
     :theme :dark}))

(rf/reg-event-db ::select
  (fn [db [_ id]] (assoc db :selected-id id)))

(rf/reg-sub ::selected-id (fn [db _] (:selected-id db)))
(rf/reg-sub ::people (fn [db _] (:people db)))
(rf/reg-sub ::selected-person
  :<- [::selected-id]
  :<- [::people]
  (fn [[id people] _] (get people id)))
(rf/reg-sub ::name
  :<- [::selected-person]
  (fn [person _] (:name person)))
(rf/reg-sub ::is-author?
  :<- [::selected-person]
  (fn [person _] (contains? (:roles person) :author)))

(defn person-name []
  [:strong.person-name @(rf/subscribe [::name])])

(defn selected-card []
  (let [author? @(rf/subscribe [::is-author?])]
    [:article#selected-person.card
     [:h2 [person-name]]
     [:p (if author? "Author" "Reader")]]))

(defn app []
  (let [selected @(rf/subscribe [::selected-id])]
    [:main
     [:h1 "re-frame Inspector fixture"]
     [:nav
      (for [id [1 2]]
        ^{:key id}
        [:button {:class (when (= selected id) "selected")
                  :on-click #(rf/dispatch [::select id])}
         (str "Person " id)])]
     [selected-card]]))

(defn ^:export init []
  (rf/dispatch-sync [::initialize])
  (rdom/render [app] (.getElementById js/document "app")))
