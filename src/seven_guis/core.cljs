(ns seven-guis.core
  (:require [goog.string :as gstring]
            [goog.string.format]
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
;; Flight booker

(defn flight-booker []
  (let [today          (-> (js/Date.now) js/Date. .toLocaleDateString)
        mode           (r/atom :one-way)
        departure-date (r/atom today)
        return-date    (r/atom today)
        message        (r/atom nil)]
    (letfn [(date? [date] (-> date js/Date. .valueOf js/Number.isNaN (not)))
            (update-date [date event]
              (let [value (-> event .-target .-value)]
                (reset! date value)))
            (date-field
              ([atom] (date-field {} atom))
              ([properties atom]
               [:input (apply merge
                         {:type      "text"
                          :value     @atom
                          :on-change #(update-date atom %)}
                         (when-not (date? @atom)
                           {:style {:color :red}})
                         properties)]))]
      (fn []
        (let [button-active?
              (and
                (date? @departure-date)
                (if (= :return @mode) 
                  (and (date? @return-date)
                       (<= (js/Date. (js/Date.parse @departure-date))
                         (js/Date. (js/Date.parse @return-date))))
                  true))]
          [:div {:style {:display        :flex
                         :flex-direction :column
                         :width          200}}
           [:h2 "Flight Booker"]
           [:select {:on-change #(reset! mode (-> % .-target .-value (keyword)))}
            [:option {:value :one-way} "one-way flight"]
            [:option {:value :return} "return flight"]]
           [date-field departure-date]
           [date-field {:disabled (not= :return @mode)} return-date]
           [:button {:disabled (not button-active?)
                     :on-click
                     #(reset! message
                        (case @mode
                          :one-way (gstring/format
                                     "You have booked a one-way flight on %s."
                                     (.toLocaleDateString (js/Date. (js/Date.parse @departure-date))))
                          :return  (gstring/format "You have booked a return flight departing on %s and returning on %s."
                                     (.toLocaleDateString (js/Date. (js/Date.parse @departure-date)))
                                     (.toLocaleDateString (js/Date. (js/Date.parse @departure-date))))))}
            "Book"]
           [:div @message]])))))

;; -------------------------
;; Timer

(defn timer []
  (let [elapsed  (r/atom 0)
        max      (r/atom 100)
        interval (js/setInterval #(when (< @elapsed @max) (swap! elapsed + 0.1)) 100)]
    (r/create-class
      {:component-will-unmount
       #(js/clearInterval interval)
       :reagent-render
       (fn []
         [:div {:style {:display :flex :flex-direction :column :width 200}}
          [:h2 "Timer"]
          [:progress {:value @elapsed
                      :max   @max}]
          [:div (gstring/format "%.2fs" @elapsed)]
          [:input {:type      "range"
                   :value     @max
                   :min       0
                   :max       200
                   :on-change #(reset! max (-> % .-target .-value))}]
          [:button {:on-click #(reset! elapsed 0)} "Reset"]])})))

;; -------------------------
;; Views

(defn home-page []
  [:div [:h1 "The Seven GUIs"]
   [counter]
   [temperature-converter]
   [flight-booker]
   [timer]])

;; -------------------------
;; Initialize app

(defn mount-root []
  (d/render [home-page] (.getElementById js/document "app")))

(defn ^:export init! []
  (mount-root))
