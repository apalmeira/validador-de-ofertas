(ns com.ar.fiuba.tdd.clojure.template
  ( :gen-class
    :name com.ar.fiuba.tdd.clojure.template
    :methods [#^{:static true} [applyDiscounts [String String String] String]])
  (:require [com.ar.fiuba.tdd.clojure.offerprocessor :refer :all])
  )

(defn -main []
  (println "Hello, World!")
  )

(defn -applyDiscounts [offers rules sale]
  (process-sale (initialize-offers offers rules) sale))