(ns llm-databench.dataset
  (:require [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck.generators :as cgen]
            [clojure.string :as str]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]))

(def all-forenames
  (->> (io/reader "resources/common-forenames-by-country.csv")
    (csv/read-csv)
    (drop 1)
    (map #(nth % 11))
    (filter (complement empty?))
    (set)))


(def all-surnames
  (->> (io/reader "resources/common-surnames-by-country.csv")
    (csv/read-csv)
    (drop 1)
    (map #(nth % 5))
    (filter (complement empty?))
    (set)))

(def colors #{"red" "blue" "green" "purple"
              "orange" "yellow" "black" "white"})


(defrecord QName [ns name prefix]
  clojure.lang.Named
  (getNamespace [_] ns)
  (getName [_] name))

(def human
  "Generator for a map representing a human with the following
  attributes:

  - uuid
  - forename
  - surname
  - height (meters, str with 2 decimal points)
  - age (at death, int)
  - favorite color"
  (gen/fmap (fn [[uuid forename surname height age fav-color]]
              {(->QName "http://data.gov/population/schema#" "uuid" "pop")
               uuid
               (->QName "http://xmlns.com/foaf/0.1/" "givenName" "foaf")
               forename
               (->QName "http://xmlns.com/foaf/0.1/" "familyName" "foaf")
               surname
               (->QName "http://data.gov/population/schema#" "height" "pop")
               height
               (->QName "http://data.gov/population/schema#" "age" "pop")
               age
               (->QName "http://people.com/interests/1.0/" "favorite_color" "pi")
               (->QName "http://dbpedia.org/resource/" fav-color "dbp")})
    (gen/tuple
      gen/uuid
      (gen/elements all-forenames)
      (gen/elements all-surnames)
      (gen/fmap (fn [cm]
                  (double (/ cm 100)))
        (gen/choose 130 215))
      (gen/choose 18 120)
      (gen/elements colors))))

;; TODO: this is a very biased set since LLMs know about humans in general
;; Generating a dataset with random columns would be informative too
(defn humans
  "Generator for a sequence of N humans. Adds a simple
  numeric ID to each one. Returns the set in ID order."
  [n]
  (gen/fmap (fn [humans]
              (let [humans (sort-by #(str/reverse (str (:uuid %))) humans)]
                (map #(let [id (+ 1000 %2)]
                        (with-meta
                          (assoc %1 (->QName "http://data.gov/population/schema#" "id" "pop") id)
                          {:entity (->QName "http://data.gov/population/individual#"
                                     (str id) "ind")}))
                  humans (range))))
    (gen/set human {:num-elements n})))


;;; Test Cases

(defn lookup-x-by-y
  "Generator for test cases involving record lookup by selecting the
  value of a field, given a field."
  [dataset-gen answer-field query-field]
  (cgen/for [ds dataset-gen
             record (gen/elements ds)]
    (let [query-val (record query-field)
          answer-val (record answer-field)]
      {:tc/dataset ds
       :tc/query-field query-field
       :tc/answer-field answer-field
       :tc/query-val query-val
       :tc/expected-answer answer-val
       :tc/prompt []})))

(defn choose-record
  "Generator for test cases involving record lookup by selecting the
  value of a field, given a field."
  [dataset-gen]
  (cgen/for [ds dataset-gen
             record (gen/elements ds)]
    {:tc/dataset ds
     :tc/record record
     :tc/prompt []}))

(defn generate
  ([n generator]
   (generate n (rand-int Integer/MAX_VALUE) generator))
  ([n seed generator]
   (gen/generate
     (gen/set generator {:num-elements n})
     30
     seed)))
