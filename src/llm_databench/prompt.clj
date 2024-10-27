(ns llm-databench.prompt
  (:require [clojure.data.csv :as csv]
            [selmer.parser :refer [<< render]])
  (:import [java.io StringWriter]))

(defn single-word
  [testcase]
  (update testcase :tc/prompt conj
    "Answer using a single word, number or proper noun."))


(defn bracket-result
  [testcase]
  (update testcase :tc/prompt conj
    "Put your final answer last and in duble brackets, e.g. {{\"1001\"}} or {{\"Rodger\"}}."))

(defn lookup-x-by-y
  [testcase x y]
  (let [record (:tc/record testcase)]
    (update testcase :tc/prompt conj
      (render "Find the value of {{x|safe}}, for the entity where {{y|safe}} is {{yv|safe}}?"
        {:x (name x)
         :y (name y)
         :yv (record )
         }

        (:tc/record testcase)))))


(defn answer
  [prompt keyfn testcase]
  (try
    (update testcase :tc/prompt conj
      (render prompt
        {:v (name (keyfn testcase))}))
    (catch Exception e
      (println "BAD prompt:" prompt)
      (println "BAD key:" key)
      (println "BAD data:")
      (clojure.pprint/pprint testcase))

    ))

(defn csv-val
  [val]
  (if (instance? clojure.lang.Named val)
    (name val)
    val))

(defn csv-data
  [testcase]
  (with-open [sw (StringWriter.)]
    (let [dataset (:tc/dataset testcase)
          keys (map name (keys (first dataset)))
          rows (map #(map csv-val (vals %)) dataset)]
      (csv/write-csv sw (cons keys rows))
      (let [data-str (str "CSV data with header row:\n\n" sw)]
        (-> testcase
          (update :tc/prompt conj data-str)
          (update :tc/query-field csv-val)
          (update :tc/answer-field csv-val)
          (update :tc/query-val csv-val)
          (update :tc/expected-answer csv-val))))))


(defn rdf-val-nt
  [val]
  (cond
    (instance? clojure.lang.Named val) (str "<" (namespace val) (name val) ">")
    (uuid? val) (str "<urn:uuid:" (str val) ">")
    (string? val) (str \" val \")
    :else (str val)))


(defn ntriples-data
  [testcase]
  (with-open [sw (StringWriter.)]
    (binding [*out* sw]
      (let [dataset (:tc/dataset testcase)]
        (doseq [row dataset]
          (let [subj (rdf-val-nt (:entity (meta row)))]
            (doseq [[col val] row]
              (let [pred (rdf-val-nt col)
                    obj (rdf-val-nt val)]
                (println subj pred obj " ."))))
          (println "\n"))
        (let [data-str (str  "RDF Data in N-Triples format:\n\n" sw)]
          (-> testcase
            (update :tc/prompt conj data-str)
            (update :tc/query-field rdf-val-nt)
            (update :tc/answer-field rdf-val-nt)
            (update :tc/query-val rdf-val-nt)
            (update :tc/expected-answer rdf-val-nt)))))))

(defn rdf-val-ttl
  [val]
  (cond
    (instance? clojure.lang.Named val) (if (:prefix val)
                                         (str (:prefix val) ":" (name val))
                                         (str "<" (namespace val) (name val) ">"))
    (uuid? val) (str "<urn:uuid:" (str val) ">")
    (string? val) (str \" val \")
    :else (str val)))

(defn turtle-data
  [testcase]
  (with-open [sw (StringWriter.)]
    (binding [*out* sw]
      (let [dataset (:tc/dataset testcase)]
        (doseq [row dataset]
          (let [subj (rdf-val-ttl (:entity (meta row)))]
            (doseq [[col val] row]
              (let [pred (rdf-val-ttl col)
                    obj (rdf-val-ttl val)]
                (println subj pred obj " ."))))
          (println ""))
        (let [data-str (str  "RDF Data in Turtle format:\n\n" sw)]
          (-> testcase
            (update :tc/prompt conj data-str)
            (update :tc/query-field rdf-val-ttl)
            (update :tc/answer-field rdf-val-ttl)
            (update :tc/query-val rdf-val-ttl)
            (update :tc/expected-answer rdf-val-ttl)))))))

