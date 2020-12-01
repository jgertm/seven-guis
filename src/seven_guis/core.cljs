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
;; Temperature converter

(defn- to-fahrenheit [celsius]
  ;; F = C * (9/5) + 32.
  (+ 32 (* celsius (/ 9 5))))

(defn- to-celsius [fahrenheit]
  ;; C = (F - 32) * (5/9) 
  (* (- fahrenheit 32) (/ 5 9)))

(defn temperature-converter []
  (let [celsius    (r/atom 0)
        fahrenheit (r/atom 32)
        update-temperature
        (fn [unit change-event]
          (let [value (-> change-event .-target .-value)]
            (reset! (case unit :celsius celsius :fahrenheit fahrenheit) value)
            (when (re-matches #"[0-9]+" value)
              (case unit
                :celsius    (reset! fahrenheit (-> value (int) (to-fahrenheit)))
                :fahrenheit (reset! celsius (-> value (int) (to-celsius)))))))]
    (fn []
      [:div
       [:h2 "Temperature Converter"]
       [:input
        {:type      "text"
         :value     (str @celsius)
         :on-change #(update-temperature :celsius %)}]
       " Celsius = "
       [:input {:type      "text"
                :value     (str @fahrenheit)
                :on-change #(update-temperature :fahrenheit %)}]
       " Fahrenheit"])))

;; -------------------------
;; Views

(defn home-page []
  [:div [:h1 "The Seven GUIs"]
   [counter]
   [temperature-converter]])

;; -------------------------
;; Initialize app

(defn mount-root []
  (d/render [home-page] (.getElementById js/document "app")))

(defn ^:export init! []
  (mount-root))
