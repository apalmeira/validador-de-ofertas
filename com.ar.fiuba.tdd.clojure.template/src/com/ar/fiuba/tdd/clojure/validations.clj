(ns com.ar.fiuba.tdd.clojure.validations
(:require [clojure.data.json :as json])
)

;; -----------------------------------------------------------------

;; Function that receives an offer JSON and returns all the rules of all the offers in a list
(defn get-offer-rules [x] (flatten (map :rules (map :rule (json/read-json x)))))

;; Function that receives a rules JSON and returns all the existing rules in a list
(defn get-rules [x] (map :code (json/read-json x)))

;; Function that validates that all the offers rules exist. If they do, returns true, false in any other case.
(defn valid-rules [x y] (empty? (for [i x
			:when (not (some #(= i %) y))		
		] "ERROR")))

;; -----------------------------------------------------------------

;; Function that receives an array list and returns the amount of elements of each array
(defn count-each-seq [v]
  (map (fn [s] [(count s)]) v))

;; Function that receives an array list and returns the amount of elements that are different on each array.
(defn count-each-seq-u [v]
  (map (fn [s] [(count (distinct s))]) v))

;; Function that receives an array list with elements and returns true if there are no repeated, false in any other case.
(defn unique-value-list [x]
(=  (vec (flatten (count-each-seq x)))
 (vec (flatten (count-each-seq-u x))))	
)

;; -----------------------------------------------------------------

; get codes in vector
(defn get-codes [x] [(map :code (json/read-json x))])

;; -----------------------------------------------------------------

(defn empty-field [x]
  (or (= x "[]") (= x "()"))
	)

(defn map-offers-rules [offers rules]
	(hash-map :state (hash-map :offers (json/read-json offers), :rules (json/read-json rules)))
	)