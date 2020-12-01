(ns seven-guis.core
    (:require
      [reagent.core :as r]
      [reagent.dom :as d]))

;; -------------------------
;; Counter

(defn counter []
  (let [count (r/atom 0)]
    (fn []
      [:div
       [:h2 "Counter"]
       [:input {:type "text"
                :value (str @count)
                :readOnly true}]
       [:button {:on-click #(swap! count inc)} "Count"]])))

;; -------------------------
;; Views

(defn home-page []
  [:div [:h1 "The Seven GUIs"]
   [counter]])

;; -------------------------
;; Initialize app

(defn mount-root []
  (d/render [home-page] (.getElementById js/document "app")))

(defn ^:export init! []
  (mount-root))
