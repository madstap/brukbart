(ns brukbart.dbg
  (:require [clojure.pprint :refer [pprint]]))

(defn log-fn
  "Create a function that applies f (which presumably has side effects) to it's argument,
  then returns it."
  [f]
  (fn [x] (f x) x))

(def p< (log-fn prn))
(def pp< (log-fn pprint))
