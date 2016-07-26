(ns brukbart.dbg
  (:require [clojure.pprint :refer [pprint]]))

(defn log-fn
  "Create a function that applies f (which presumably has side effects)
  to its argument, then returns it."
  [f]
  (fn [x] (f x) x))

(def p< "Prints a value with prn and returns it."
  (log-fn prn))

(def pln< "Prints a value with println and returns it."
  (log-fn println))

(def pp< "Pretty prints a value and returns it."
  (log-fn pprint))
