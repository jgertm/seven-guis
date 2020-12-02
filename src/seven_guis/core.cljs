(ns seven-guis.core
  (:require [clojure.string :as str]
            [goog.string :as gstring]
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
;; CRUD

(defn- random-int []
  ;; LOLJS
  (js/Math.floor (* 1e6 (js/Math.random))))

(def initial-db
  (->> [{:surname "McCarthy" :name "John"}
        {:surname "Hickey" :name "Rich"}
        {:surname "Hamilton" :name "Margaret"}
        {:surname "Mickens" :name "James"}]
    (map #(let [id (random-int)]
            (vector id (assoc % :id id))))
    (into {})
    (hash-map :data)))

(defn crud []
  (letfn [(create [person db]
            (swap! db assoc-in [:data (random-int)] person))
          (update [person db]
            (swap! db assoc-in [:data (:id person)] person))
          (delete [person db]
            (swap! db clojure.core/update :data dissoc (:id person)))
          (select [event db entry]
            (let [value  (-> event .-target .-value (int))
                  person (get (:data @db) value)]
              (swap! db assoc :selection value)
              (reset! entry person)))]
    (let [db            (r/atom initial-db)
          entry         (r/atom {})
          filter-prefix (r/atom "")]
      (fn []
        (let [valid-selection? (contains? (:data @db) (:selection @db))]
          [:div
           [:h2 "CRUD"]
           [:div "Filter prefix"
            [:input {:type      :text
                     :value     @filter-prefix
                     :on-change #(reset! filter-prefix (-> % .-target .-value))}]]
           [:div {:style {:display :flex}}
            [:div
             (->> @db
               :data
               (sort-by (comp :surname val))
               (filter (fn [[_ {:keys [surname]}]] (str/starts-with? (str/lower-case surname) (str/lower-case @filter-prefix))))
               (map (fn [[id {:keys [surname name]}]]
                      [:option {:value (str id)} (gstring/format "%s, %s" surname name)]))
               (into [:select {:size     8
                               :style    {:width 200}
                               :on-click #(select % db entry)}]))]
            [:div  {:style {:display :flex :flex-direction :column}}
             [:div
              [:div "Name"]
              [:input {:type      :text
                       :value     (:name @entry)
                       :on-change #(swap! entry assoc :name (-> % .-target .-value))}]]
             [:div 
              [:div "Surname"]
              [:input {:type      :text
                       :value     (:surname @entry)
                       :on-change #(swap! entry assoc :surname (-> % .-target .-value))}]]]]
           [:div {:style {:display :flex :flex-direction :row}}
            [:button {:on-click #(create @entry db)} "Create"]
            [:button {:disabled (not valid-selection?) :on-click #(update @entry db)} "Update"]
            [:button {:disabled (not valid-selection?) :on-click #(delete @entry db)} "Delete"]]])))))

(defn circle-drawer []
  (letfn [(undo [{:keys [sequence point] :as operations}]
            (assoc operations :point (min (inc point) (count sequence))))
          (redo [{:keys [sequence point] :as operations}]
            (assoc operations :point (max (dec point) 0)))]
    (let [operations (r/atom {:sequence '()
                              :point    0})]
      (r/create-class
        {:component-did-update
         (fn []
           (let [canvas (.getElementById js/document "circle-drawer")
                 ctx    (.getContext canvas "2d")
                 {:keys [sequence point]} @operations]
             (.clearRect ctx 0 0 (.-width canvas) (.-height canvas))
             (->> sequence
               (drop point)
               (reverse)
               (run!
                 (fn [operation]
                   (case (first operation)
                     :draw-circle
                     (let [[_ x y r] operation]
                       (doto ctx
                         (.beginPath)
                         (.arc
                           (- x (.-offsetLeft canvas))
                           (- y (.-offsetTop canvas))
                           r
                           0 (* 2 js/Math.PI) false)
                         (.stroke)))))))))
         :reagent-render
         (fn []
           (let [_ @operations]
             [:div
              [:h2 "Circle Drawer"]
              [:div {:style {:display :flex}}
               [:button {:on-click #(swap! operations undo)} "Undo"]
               [:button {:on-click #(swap! operations redo)} "Redo"]]
              [:canvas#circle-drawer
               {:width    800
                :height   640
                :style    {:border "1px black solid"}
                :on-click (fn [event]
                            (let [x (.-pageX event)
                                  y (.-pageY event)]
                              (swap! operations
                                #(-> %
                                   (update :sequence (partial drop (:point %)))
                                   (assoc :point 0)
                                   (update :sequence conj [:draw-circle x y 50])))))}]]))}))))

;; -------------------------
;; Views

(defn home-page []
  [:div [:h1 "The Seven GUIs"]
   [:p "Taken from "
    [:a {:href "https://eugenkiss.github.io/7guis/tasks/"}
     "https://eugenkiss.github.io/7guis/tasks/"] "."]
   [counter]
   [temperature-converter]
   [flight-booker]
   [timer]
   [crud]
   [circle-drawer]])

;; -------------------------
;; Initialize app

(defn mount-root []
  (d/render [home-page] (.getElementById js/document "app")))

(defn ^:export init! []
  (mount-root))
