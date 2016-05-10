(ns brukbart.brasil
  (:require [clojure.edn :as edn]
            [brukbart.core :refer [vars->map one?]]))

(defn- str->digits
  "Takes a string and returns a seq of the digits it contains, ignores other characters"
  [s]
  (map edn/read-string (re-seq #"[0-9]" s)))

(defn parse
  "Parses a cpf or cnpj string. Ignores any character that are not digits.
  Returns a map of digits control and whole.
  Will parse malformed strings without complaining."
  [s]
  (let [whole (str->digits s)
        [digits control] (split-at (- (count whole) 2) whole)]
    (vars->map digits control whole)))

(defn control-digit
  [mask digits]
  {:pre [(= (count mask) (count digits))]}
  (let [sum (apply + (map * mask digits))
        x (- 11 (rem sum 11))]
    (if (> x 9) 0 x)))

(defn control-digits
  [mask1 mask2 digits]
  (let [c1 (control-digit mask1 digits)
        c2 (control-digit mask2 (conj (vec digits) c1))]
    [c1 c2]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CPF

(def cpf-length 11)

(def invalid-cpfs
  "A set of edge cases where the algorithm returns a false positive."
  (set (conj (for [i (range 10)]
               (repeat cpf-length i)) [0 1 2 3 4 5 6 7 8 9 0])))

(defn cpf-validator
  "Takes a string. Any characters that are not digits are ignored.
  Returns either :valid, :invalid or :malformed"
  [cpf]
  (let [{:keys [digits control whole]} (parse cpf)]
    
    (cond (not= cpf-length (count whole)) :malformed
          (invalid-cpfs whole) :invalid
          (= control (control-digits (range 10 1 -1) (range 11 1 -1) digits)) :valid
          :else :invalid)))


(defn valid-cpf?
  "Takes a string with a cpf and ignores any characters that are not digits.
  Returns true if valid, else false"
  [cpf]
  (= :valid (cpf-validator cpf)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CNPJ

(def cnpj-length 14)

(defn cnpj-validator
  "Takes a string. Any characters that are not digits are ignored.
  Returns either :valid, :invalid or :malformed"
  [cnpj]
  (let [{:keys [digits control whole]} (parse cnpj)

        mask1   [5 4 3 2 9 8 7 6 5 4 3 2]
        mask2 [6 5 4 3 2 9 8 7 6 5 4 3 2]]

    (cond (not= cnpj-length (count whole)) :malformed
          (= control (control-digits mask1 mask2 digits)) :valid
          :else :invalid)))

(defn valid-cnpj?
  "Takes a string with a cnpj and ignores any characters that are not digits.
  Returns true if valid, else false"
  [cnpj]
  (= :valid (cnpj-validator cnpj)))






(comment

  (cnpj-validator "11.444.777/0001-61")

  (map valid-cnpj? ["36.564.023/0001-76"
                    "46.303.783/0001-83"
                    "48.741.030/0001-85"
                    "76.887.562/0001-60"])

  (parse "48.741.030/0001-85")

  (valid-cpf? "70007828209")

  (map (comp cpf-validator (partial apply str)) invalid-cpfs)
  
  )
