(ns nordnet-magic.core
  (:require [etaoin.api :as api]
            [etaoin.keys :as k]
            [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [clojure.test :refer [is]])
  (:import java.util.Base64))


(defonce state-atom (atom nil))

(defonce driver-atom (atom nil))
(defn match [regxp] (partial re-matches regxp))
(defn str-and-match? [regxp] (s/and string? (match regxp)))

(s/def ::name string?)

(s/def ::SEK (fn [v] (re-matches #"SEK" v)))
(s/def ::EUR (fn [v] (re-matches #"EUR" v)))
(s/def ::currency (s/and string? (s/or ::SEK ::EUR)))

(s/def ::quantity double?)
(s/def ::GAV double?)
(s/def ::dev-today double?)
(s/def ::last-bid double?)
(s/def ::value double?)
(s/def ::value-sek double?)
(s/def ::revenue-% double?)
(s/def ::revenue-sek double?)
(s/def ::mean-price double?)
(s/def ::last-NAV double?)
(s/def ::purchase-value double?)

;; req-un, where -un makes it so the keys don't need to be namespaced
(s/def ::fund (s/keys :req-un [::name ::currency ::quantity ::mean-price ::dev-today ::purchase-value ::last-NAV ::value-sek ::revenue-% ::revenue-sek]))
(s/def ::stock (s/keys :req-un [::name ::currency ::quantity ::GAV ::dev-today ::last-bid ::value ::value-sek ::revenue-% ::revenue-sek]))

(s/def ::raw-name (str-and-match? #"Namn"))
(s/def ::raw-currency (str-and-match? #"Valuta"))
(s/def ::raw-quantitiy (str-and-match? #"Antal"))
(s/def ::raw-GAV (str-and-match? #"GAV"))
(s/def ::raw-dev-today (str-and-match? #"Idag %"))
(s/def ::raw-dev-one-day (str-and-match? #"1 dag %"))
(s/def ::raw-last-bid (str-and-match? #"Senaste kurs"))
(s/def ::raw-value (str-and-match? #"Värde"))
(s/def ::raw-value-sek (str-and-match? #"Värde&nbsp;SEK|Värde SEK"))
(s/def ::raw-revenue-sek (str-and-match? #"Avkast.&nbsp;SEK"))
(s/def ::raw-revenue-% (str-and-match? #"Avkast. %"))
(s/def ::raw-mean-price (str-and-match? #"Snittkurs"))
(s/def ::raw-last-NAV (str-and-match? #"Senaste NAV"))
(s/def ::raw-purchase-value (str-and-match? #"Inköpsv.&nbsp;SEK"))

(s/def ::exchange-headers (s/cat :name ::raw-name
                                 :currency ::raw-currency
                                 :quantitiy ::raw-quantitiy
                                 :GAV ::raw-GAV
                                 :dev-today ::raw-dev-today
                                 :last-bid ::raw-last-bid
                                 :value ::raw-value
                                 :value-sek ::raw-value-sek
                                 :revenue ::raw-revenue-%
                                 :revenue-sek ::raw-revenue-sek))

(s/def ::fund-headers (s/cat :name ::raw-name
                             :currency ::raw-currency
                             :quantity ::raw-quantitiy
                             :mean-price ::raw-mean-price
                             :dev-one-day ::raw-dev-one-day
                             :purchase-value ::raw-purchase-value
                             :last-NAV ::raw-last-NAV
                             :value-sek ::raw-value-sek
                             :revenue ::raw-revenue-%
                             :revenue-sek ::raw-revenue-sek))

(s/def ::exchange-headers->keyword (s/keys :req-un [::raw-name ::raw-currency ::raw-quantitiy ::raw-GAV ::raw-dev-today ::raw-last-bid ::raw-value ::raw-value-sek ::raw-revenue-% ::raw-revenue-sek]))
(s/def ::fund-headers->keyword (s/keys :req-un [::raw-name ::raw-currency ::raw-quantitiy ::raw-mean-price ::raw-dev-today ::raw-purchase-value ::raw-last-NAV ::raw-value-sek ::raw-revenue-% ::raw-revenue-sek]))

(s/def ::column (fn [v] (= (count v) 10)))
(def stock-title "Börshandlat")
(def fund-title "Fonder")

(def fund->keyword {"Namn"         :name
                    "Valuta"       :currency
                    "Antal"        :quantity
                    "GAV"          :GAV
                    "Idag %"       :dev-today
                    "Senaste kurs" :last-bid
                    "Värde"        :value
                    "Värde SEK"    :value-sek
                    "Avkast. %"    :revenue-%
                    "Avkast. SEK"  :revenue-sek})

(def stock->keyword {"Namn"         :name
                     "Valuta"       :currency
                     "Antal"        :quantity
                     "Snittkurs"    :mean-price
                     "1 dag %"      :dev-today
                     "Inköpsv. SEK" :purchase-value
                     "Senaste NAV"  :last-NAV
                     "Värde SEK"    :value-sek
                     "Avkast. %"    :revenue-%
                     "Avkast. SEK"  :revenue-sek})

(def select-values (comp vals select-keys))

(def stock-fund->keyword
  {:stock stock->keyword
   :fund  fund->keyword})

(defn asort [order amap]
  (conj {} (select-keys amap order)))

(defn in?
  [coll elm]
  (some #(= elm %) coll))

(defn encode
  [to-encode]
  (.encodeToString (Base64/getEncoder) (.getBytes to-encode)))

(defn decode
  [to-decode]
  (String. (.decode (Base64/getDecoder) to-decode)))

(defn wait-and-click
  [driver path]
  (doto driver
    (api/wait-visible path)
    (api/click path)))

(defmulti login! (fn [_ type _] type))

(defmethod login! :username [driver _ {:keys [username password url account-name]}]
  (doto driver
    (api/go url)
    (wait-and-click {:tag :button :fn/text "användarnamn och lösenord"})
    (api/wait-visible [{:tag :input :id :username}])
    (api/fill {:tag :input :id :username} username)
    (api/fill {:tag :input :id :password} password)
    (api/click {:tag :button :fn/text "Logga in"})
    (api/wait-visible {:tag :span :fn/has-text account-name})))

(defmethod login! :bankid [driver _ {:keys [personal-number url]}]
  (doto driver
    (api/go url)
    (api/wait-visible {:tag :a :href "/login"})
    (api/click {:tag :a :href "/login"})
    (api/wait-visible {:tag :div :fn/text "Mobilt BankID"})
    (api/click {:tag :div :fn/text "Mobilt BankID"})
    (api/wait-visible {:tag :input :placeholder "ååååmmddnnnn"})
    (do
      (dotimes [_ 13]
        (api/fill driver {:tag :input :placeholder "ååååmmddnnnn"} k/backspace))
      driver)
    (api/fill {:tag :input :placeholder "ååååmmddnnnn"} personal-number)
    (api/click {:tag :button :data-l10n-id "nordnet-bankid-continue"})))

(defn get-table-headers!
  "Get the 10 table headers from the stock column."
  [{:keys [driver table-header]}]
  {:pre  [(or (= table-header fund-title) (= table-header stock-title))]
   :post [(= (count %) 10)]}
  (let [query [{:tag :h2 :fn/has-text table-header}
               {:xpath "ancestor::header//.."}
               {:xpath ".//*[local-name() = 'svg']//../..//span"}]]
    (->> (api/query-all driver query)
         (mapv (partial api/get-element-inner-html-el driver))
         (apply #(subvec (into [] %&) 0 10)))))

(defn get-table-values!
  "Get table values, they should be ten!"
  [{:keys [driver table-header]}]
  {:pre  [(s/or :funds (s/and (s/valid? string? table-header) (s/valid? (fn [v] (re-matches (re-pattern fund-title) v)) table-header))
                :stock (s/and (s/valid? string? table-header) (s/valid? (fn [v] (re-matches (re-pattern stock-title) v)) table-header)))]
   :post [(every? #(= (count %) 10) %)]}
  (let [query [{:tag :h2 :fn/has-text table-header}
               {:xpath "ancestor::header//.."}
               {:tag :div :fn/has-class "kzvPKu"}
               {:tag :span :fn/has-class "lgGULA"}]]
    (->>
      (api/query-all driver {:tag :div :role "row"})
      (mapv (partial api/get-element-text-el driver))
      (partition 10)
      (mapv vec))))

(s/def ::headers (fn [v]
                   (or
                     (s/valid? ::exchange-headers v)
                     (s/valid? ::fund-headers v))))
(s/def ::columns (s/coll-of string?))
;; strings as keys in a map is apparently not supported, so this spec is wasted?!
(s/def ::keyword-map (fn [v] (or (= v stock->keyword) (= v fund->keyword))))

(defn table->map
  "Convert a list of data to a map with {:header1 data1 :header2 data2}"
  [{:keys [headers columns keyword-map]}]
  {:pre  [(= (count headers) (count columns))]
   :post [(= (count (keys %)) (count headers))]}
  (reduce (fn [kvm [head col]]
            (assoc kvm (get keyword-map head) col)) {} (mapv vector headers columns)))
(s/fdef table->map :args (s/cat :arguments (s/keys :req-un [::headers ::columns ::keyword-map])))

(defn nordnet-fknn-dash->dash
  "that is some weird shit - just wasted 45 min of my life.."
  [str-with-fkn-nordnet-dash]
  {:pre  [(s/valid? string? str-with-fkn-nordnet-dash)]
   :post [(s/valid? string? %)]}
  (str/replace str-with-fkn-nordnet-dash #"−" "-"))

(defn sane-double
  [crazy-double]
  {:pre  [(s/valid? (fn [v] (not (re-find #"[a-zA-Z]|[åäö|ÅÄÖ]" v))) crazy-double)]
   :post [(s/valid? double? %)]}
  (->
    crazy-double
    (str/replace #" " "")
    (str/replace #"," ".")
    (str/replace #"%" "")
    nordnet-fknn-dash->dash
    Double/parseDouble))

(defn parse-data
  "Given a map with string values, parse it to it's real value"
  [data]
  {:pre  [(map? data)]
   :post [(= (count data) (count %)) (map? %) (= (keys data) (keys %))]}
  (reduce-kv (fn [acc k v]
               (if (boolean (re-find #"[a-zA-Z]|[åäö|ÅÄÖ]" v))
                 (assoc acc k v)
                 (assoc acc k (sane-double v)))) {} data))

(defn get-total-value
  [state]
  (let [stock-sum (->> (get-in state [:data :stock])
                       (map :value-sek)
                       (apply +))
        fund-sum (->> (get-in state [:data :fund])
                      (map :value-sek)
                      (apply +))]
    (+ stock-sum fund-sum)))

;(defn get-raw-data!
;  [{:keys [driver table-headers]}]
;  {:pre [(every? map? table-headers)
;         (every? (fn [{:keys [header type]}]
;                   (or (and #(re-matches (re-pattern fund-title) header) (= type :fund))
;                       (and #(re-matches (re-pattern stock-title) header) (= type :stock)))) table-headers)]}
;  (reduce (fn [data {:keys [header type]}]
;            (let [values (get-table-values! {:driver driver :table-header header})
;                  headers (get-table-headers! {:driver driver :table-header header})]
;              (-> (assoc-in data [:columns type] values)
;                  (assoc-in [:headers type] headers)))) {} table-headers))

(defn get-raw-data!
  [{:keys [driver]}]
  (->>
    (api/query-all @driver-atom {:tag :div :role "row"})
    (mapv (fn [el-id] (api/get-element-text-el driver el-id)))
    (reduce (fn [acc row]
              (if (re-find #"Namn" row)
                (conj acc [row])
                (let [l (dec (count acc))]
                  (assoc acc l (conj (last acc) row))))) [])
    (map (fn [group] (map (fn [row] (clojure.string/split row #"\n")) group)))
    (map (fn [group] (map (fn [row] (filter (fn [ent]
                                              (not
                                                (or (re-matches #"KöpSälj" ent)
                                                    (re-matches #"Byt" ent)
                                                    (re-matches #"Totalt" ent)))) row)) group)))
    ;; remove the 'total' data row
    (map (fn [group] (filter (fn [row] (> (count row) 7)) group)))
    (reduce (fn [data group]
              (cond
                (in? (first group) "GAV")
                (-> (assoc-in data [:headers :fund] (first group))
                    (assoc-in [:columns :fund] (rest group)))

                (in? (first group) "Snittkurs")
                (-> (assoc-in data [:headers :stock] (first group))
                    (assoc-in [:columns :stock] (rest group)))
                )

              ) {})
    )
  )

(defn parse-raw-data
  [state]
  (reduce (fn [data type]
            (->> (get-in state [:raw-data :columns type])
                 (mapv (fn [raw-data]
                         (table->map {:headers     (get-in state [:raw-data :headers type])
                                      :columns     raw-data
                                      :keyword-map (get stock-fund->keyword type)})))
                 (mapv parse-data)
                 (assoc data type))) {} (-> (get-in state [:raw-data :columns]) keys)))

(defn get-asset-names
  [state]
  (-> (get state :data)
      (select-values [:stock :fund])
      flatten
      (#(map :name %))))

(defn get-funds-and-stock
  [state]
  (concat (get-in state [:data :stock]) (get-in state [:data :fund])))

(defn get-asset-distribution
  [state]
  (let [total-value (get-total-value state)]
    (->>
      (get-funds-and-stock state)
      (mapv (fn [{:keys [name value-sek]}]
              {:name      name
               :value-sek value-sek
               :dist      (/ value-sek total-value)})))))

(defn get-split
  [state funds-stocks name]
  (let [n (get-in state [:split name])]
    (if (some? n)
      n
      (do
        (println "Unable to find the name - " name)
        (println "We found these names:")
        (doseq [n_ funds-stocks]
          (println "\t " (:name n_)))
        (println "And the split from config.edn has these names: ")
        (doseq [n_ (keys (get-in state [:split]))]
          (println "\t " n_))
        (System/exit 1)))))

(defn sek-to-buy
  "Amount to buy to keep the balance correct for the given stock
  percent-of-stock * (total-new-investment + current-total-value) - current-stock-value = sek-to-buy"
  {:test (fn []
           (let [state {:investment 1000.0
                        :split      {"a" 0.4
                                     "b" 0.6}
                        :data       {:stock [{:name "a" :value-sek 100}]
                                     :fund  [{:name "b" :value-sek 100}]}}]
             (is (= (->> (sek-to-buy state)
                         (mapv :buy)
                         (apply +)) (:investment state)))))}
  [state]
  (let [total-value (get-total-value state)
        funds-stocks (get-funds-and-stock state)]
    (->>
      funds-stocks
      (mapv (fn [{:keys [name value-sek]}]
              (let [split (get-split state funds-stocks name)
                    to-buy (if (:keep-split state)
                             (- (* split
                                   (+ (:investment state) total-value))
                                value-sek)
                             (* split (:investment state)))]
                {:name          name
                 :value-sek     value-sek
                 :new-value-sek (+ value-sek to-buy)
                 :buy           to-buy
                 :dist          (/ value-sek total-value)
                 :new-dist      (/ (+ value-sek to-buy) (+ (:investment state) total-value))}))))))

(defn round-map
  [m]
  (map (fn [v] (reduce-kv (fn [a k v]
                            (if (string? v)
                              (assoc a k v)
                              (assoc a k (format "%.2f" v)))) {} v)) m))
(defn pprint-info
  [state]
  (let [headers ["INSTRUMENT" "VALUE" "NEW-VALUE" "BUY-SEK" "DIST" "NEW-DIST"]
        to-buy (-> (sek-to-buy state) round-map)]
    (->>
      (mapv vals to-buy)
      (mapv (fn [v]
              (->> (mapv vector headers v)
                   (apply concat)
                   (apply hash-map)
                   (asort headers))))
      clojure.pprint/print-table)))

(defn init!
  [config]

  (swap! state-atom assoc :investment (:investment config))
  (swap! state-atom assoc :split (:split config))
  (swap! state-atom assoc :keep-split (:keep-split config))

  ;; open the browser
  (println "Starting browser...")
  (reset! driver-atom (api/chrome
                        (if (:headless config)
                          {:capabilities
                           {:chromeOptions
                            {:args ["--headless" "window-size=1408,1028"
                                    "--disable-3d-apis"
                                    "--disable-accelerated-video"
                                    "--disable-plugins-discovery"
                                    "--disable-translate"]}}
                           }
                          {:size [1408 1028]})))

  ;; login
  (println "Log in...")
  (if (= (:login-type config) :username)
    (doto (deref driver-atom)
      (login! :username
              {:username     (-> (get-in config [:login :username-base64]) decode)
               :password     (-> (get-in config [:login :password-base64]) decode)
               :url          (get-in config [:login :login-username-url])
               :account-name (get config :account-name)})
      (api/go (:account-url config))
      (api/wait-visible {:tag :h2 :fn/has-text stock-title})
      (api/wait-visible {:tag :h2 :fn/has-text fund-title})
      ;; I think the graph messes things up, hope it renders within 5 sec
      (api/wait 5))

    (doto (deref driver-atom)
      (do (println "Login in with bankid..."))
      (login! :bankid
              {:personal-number (-> (get-in config [:login :personal-number-base64]) decode)
               :url             (get-in config [:login :login-bankid-url])})
      (api/go (:account-url config))
      (api/wait-visible {:tag :h2 :fn/has-text stock-title})
      (api/wait 5)))

  ;;; retrieve the raw data from the webpage
  (println "Collecting raw data...")
  (->>
    (get-raw-data! {:driver        @driver-atom
                    :table-headers [{:header stock-title :type :stock}
                                    {:header fund-title :type :fund}]})
    (swap! state-atom assoc :raw-data))

  ;; parse it
  (println "Crunching data...")
  (swap! state-atom assoc :data (parse-raw-data @state-atom))

  ;; print asset info on what to buy
  (pprint-info @state-atom)

  ;; print total value
  (println "\nInvested value:\t\t" (:investment @state-atom) " kr\nTotal new value:\t\t" (get-total-value @state-atom) " Kr"))

(comment
  (get-asset-names @state-atom)
  (get-asset-distribution @state-atom)
  (get-total-value @state-atom)
  (sek-to-buy @state-atom)
  (pprint-info @state-atom)

  (let [config (-> (slurp "config.edn") clojure.edn/read-string)]
    (init! config)
    )

  (get-raw-data! {:driver        @driver-atom
                  :table-headers [{:header stock-title :type :stock}
                                  {:header fund-title :type :fund}]})

  (clojure.pprint/pprint @state-atom)

  )

