(ns com.ar.fiuba.tdd.clojure.ruleprocessor
  (:require [com.ar.fiuba.tdd.clojure.datadefinitions :refer :all]
  :require [clojure.data.json :as json]))

(defn find-rule [rule-name rules]
  (first (filter #(= rule-name (:code %)) rules)))

(defn get-sale-price [sale]
  (reduce + (map #(get % :price) (:products sale)))
)

(defn get-discount [type discount value]
  (cond
    (= type "PERCENTAGE")
      (format "%.2f" (double (* value (/ discount 100))))
    :else 0
  )
)

(defn filter-product-rules [rules]
  (filter #(not= nil %) (map #(if (and (contains? % :field) (clojure.string/starts-with? (:field %) "PRODUCT") ) %) (map #(find-rule % (json/read-json common-rules)) rules)))
)

(defn get-nested-field-value [nested-field sale-field]
  (get-in sale-field (map keyword nested-field)))

(defn compare-values [value type rule-value]
  (cond
    (= type "EQUALS")
      (= value rule-value)
    (= type "DISTINCT")
      (not (= value rule-value))
    (= type "HIGHER")
      (> value rule-value)
    (= type "LOWER")
      (< value rule-value)    
    (= type "IN")
      (.contains rule-value value)
    :else (println "COMPUESTA")
  )
)

(defn apply-simple-rule? [rule sale-field]
  (let [sale-field-name (first (rest (clojure.string/split (:field rule) #"\.")))]
  (compare-values ((keyword sale-field-name) sale-field) (:type rule) (:value rule))
))

(defn apply-atomic-rule? [rule sale]
  (cond
    (= (:type rule) "NOT")
      (not= true (apply-atomic-rule? (find-rule (:rules rule) (json/read-json common-rules)) sale))
    (= (first (clojure.string/split (:field rule) #"\.")) "CALENDAR")
      (apply-simple-rule? rule (:purchase_date sale))
    (= (first (clojure.string/split (:field rule) #"\.")) "PAYMENT")
      (apply-simple-rule? rule (:payment sale))
    (= (first (clojure.string/split (:field rule) #"\.")) "PRODUCT")
      (if (= 1 (count (rest (clojure.string/split (:field rule) #"\.")))) 
        (.contains (map #(apply-simple-rule? rule %) (:products sale)) true)
        (.contains (map #(compare-values 
              (get-nested-field-value (rest (clojure.string/split (:field rule) #"\."))  %) 
              (:type rule) 
              (:value rule)
            ) (:products sale)) true)
      )
  )
)

(defn apply-composed-rule? [rule sale]
  (cond
        (or (= (:type rule) "AND") (= (:type rule) "EXCLUSIVE_AND"))
            (every? true? 
                (map 
                      #(apply-atomic-rule? (find-rule % (json/read-json common-rules)) sale) 
                      (:rules rule)
                  )   
            )
        (= (:type rule) "OR")  
            (.contains (map 
                          #(apply-atomic-rule? (find-rule % (json/read-json common-rules)) sale) 
                          (:rules rule)
                        ) true)
  )
)

(defn apply-product-discount [offer product]
  {
    :offer_code (:code offer),
    :description (:description offer),
    :discount (get-discount (get-in offer [:discount :type])
                            (Float/parseFloat (get-in offer [:discount :value]))
                            (:price product)
              )
  }
)

(defn apply-rule? [rule sale]
  (if (contains? rule :rules) 
    (apply-composed-rule? rule sale)
    (apply-atomic-rule? rule sale)
  )
)

(defn is-product-rule? [rule]
  (if (= false (contains? rule :rules))
    (= "PRODUCT" (first (clojure.string/split (:field rule) #"\.")))
    false
  )
)

(defn contains-product-rule? [rules]
  (.contains (map #(is-product-rule? %)  (map #(find-rule % (json/read-json common-rules)) rules)  ) true)
)

(defn apply-rule-in-product? [rule product]
  (if (= 1 (count (rest (clojure.string/split (:field rule) #"\.")))) 
        (= true (apply-simple-rule? rule product))
        (= true (compare-values 
              (get-nested-field-value (rest (clojure.string/split (:field rule) #"\.")) product) 
              (:type rule) 
              (:value rule)
            )) 
      )
)

(defn apply-offer-in-product? [offer product]
  (cond
      (= (get-in offer [:rule :type]) "AND")
      (if (every? true? (map 
                      #(apply-rule-in-product? % product) 
                      (filter-product-rules (get-in offer [:rule :rules])))
            )
        (apply-product-discount offer product))

      (= (get-in offer [:rule :type]) "OR")
        (if (.contains (map #(apply-rule-in-product? % product)
                        (filter-product-rules (get-in offer [:rule :rules]))) true)
        (apply-product-discount offer product))
  )
)

(defn count-times-rule-in-rules [rule rules]
  (count (filter identity (map #(= rule %) rules)))
)

(defn count-products-apply-in-rule [rule products]
  (count (filter identity (map #(apply-rule-in-product? rule %) products)))
)

(defn apply-exclusive-discount [offer products]
  (let [offers-applied []
        apply-offer-times (apply min (map 
              #(int (Math/floor (/ (count-products-apply-in-rule (find-rule % (json/read-json common-rules)) products) (count-times-rule-in-rules % (get-in offer [:rule :rules])))))
              (distinct (get-in offer [:rule :rules]))
          ))
        ]
      (repeat apply-offer-times
        {
          :offer_code (:code offer),
          :description (:description offer),
          :discount (get-in offer [:discount :value])
        }
      )
      
  )
)

(defn apply-offer? [offer sale]
  (if (= true (apply-rule? (:rule offer) sale))
    (cond
      (= (get-in offer [:rule :type]) "EXCLUSIVE_AND")
        (apply-exclusive-discount offer (:products sale))
      (= true (contains-product-rule? (get-in offer [:rule :rules])))
        (filter #(not= nil %) (map #(apply-offer-in-product? offer %) (:products sale)))
      :else
        (map #(apply-product-discount offer %) (:products sale))
    )
  )
)
