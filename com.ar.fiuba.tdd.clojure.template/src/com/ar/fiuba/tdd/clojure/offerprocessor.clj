(ns com.ar.fiuba.tdd.clojure.offerprocessor
  (:require [com.ar.fiuba.tdd.clojure.datadefinitions :refer :all]
    :require [com.ar.fiuba.tdd.clojure.ruleprocessor :refer :all]
    :require [clojure.data.json :as json]
    :require [com.ar.fiuba.tdd.clojure.validations :refer :all]))

;; Initialize state
;; Pre: Receives a json with offers & another with rules
;; Post: It returns a map >> {:state {:rules rules, :offers offers}}
;; To get the information inside the created map (ie. the rules) >> (get-in theMap [:state :rules])
(defn -initialize-offers [offers rules]
  (try
    (json/read-str offers)
    (catch Exception e (.getMessage e) (throw (Exception. "Offers exception. Invalid JSON."))))

  (try
    (json/read-str rules)
    (catch Exception e (.getMessage e) (throw (Exception. "Rules exception. Invalid JSON."))))

  (if (empty-field offers)
    (throw (Exception. "Empty Offers.")))

  (if (empty-field rules)
    (throw (Exception. "Empty Rules.")))

  (when
    (not (unique-value-list (get-codes offers)))
    (throw (Exception. "Unique offer codes - Duplicate offer codes exception."))
    )

  (when
    (not (unique-value-list (get-codes rules)))
    (throw (Exception. "Unique rules codes - Duplicated rules codes exception."))
    )

  (when
    (not (valid-rules (get-offer-rules offers) (get-rules rules)))
    (throw (Exception. "Existing rule exception."))
    )

  ;TODO - Check the following exception:
  ;(when
  ;	(not (unique-value-list (offers)))
  ;	(throw (Exception. "Rules not duplicated in a given offer."))
  ;)

  :else (map-offers-rules offers rules)
  )

(defn -process-sale [state sale]
  (let [payment-method (get-in (json/read-json sale) [:payment :method])]
    (if (and (not= payment-method "DEBIT")
             (not= payment-method "CREDIT")
             (not= payment-method "CASH")
             (not= payment-method "ETC"))
      (throw (Exception. "Bad Payment method exception.")))

    (if (or (= payment-method "DEBIT")
            (= payment-method "CREDIT"))
      (let [payment-bank (get-in (json/read-json sale) [:payment :bank])]
        (if (and (not= payment-bank "BBVA FRANCES")
                 (not= payment-bank "GALICIA")
                 (not= payment-bank "SANTANDER RIO")
                 (not= payment-bank "CAPRO")
                 (not= payment-bank "PARISIA")
                 (not= payment-bank "etc"))
        (throw (Exception. "Bad Payment card exception.")))))
    )

  (try
    (->> (get-in state [:state :offers])
         (map #(apply-offer? % (json/read-json sale)))
         (filter #(not= nil %))
         (flatten)
         (json/write-str))
    (catch Exception e (.getMessage e)
      (throw (Exception. "Process sale exception.")))
  )
)