(ns llm-databench.bench
  (:require [llm-databench.dataset :as ds]
            [llm-databench.prompt :as prompt]
            [llm-databench.llm :as llm]
            [llm-databench.util :as util]
            [selmer.parser :refer [<< render]]
            [clojure.test.check.generators :as gen]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.edn :as edn]))

(defn persist
  "Persist the given data as EDN"
  [path data]
  (with-open [w (io/writer path)]
    (binding [*out* w]
      (pr data))))

(defn matrix
  "Construct a test execution matrix.

  Threads the identify function through a series of phases. If a phase
  has multiple variants, it is threaded through each variant, and both
  results passed to the next phase. Therefore, the number of outputs
  is equal to the cartesian product of all variants of all phases.

  Each phase is a [phase-id phase-map].

  Each phase map is a map of {variant-id variant-fn}.

  Returns a map representing all possible combinations of variant
  applications.

  The key of the result map is a map of {phase-id variant-id},
  containing values for each phase.

  The value is a function which is a composition of all previous
  functions."
  [& phases]
  (let [phases (partition 2 phases)]
    (reduce (fn [acc [phase-id variant-map]]
              (into {} (for [[key fn] acc
                             [vid vfn] variant-map]
                         [(assoc key phase-id vid) (util/pcomp vfn fn)])))
      {{} identity}
      phases)))

(def ^:dynamic *percent-complete* (atom 0))

(defn execute-matrix
  "Given a test set and a test execution matrix, execute all entries and
  return a seq of results. Results will be a map of the matrix keys
  with a :testset-id and :testcase-idx key added to the matrix
  function output applied to each test case."
  [testset-id testset matrix]
  (reset! *percent-complete* 0)
  (let [cases (mapcat (fn [[matrix-key matrix-fn]]
                        (map-indexed
                          (fn [i testcase]
                            [(assoc matrix-key
                               :testset-id testset-id
                               :testcase-idx i)
                             (partial matrix-fn testcase)])
                          testset))
                matrix)]
    (into {}
      (mapcat (fn [group]
                (swap! *percent-complete* + (count group))
                (->> group
                  (mapv (fn [[key exec-fn]]
                          [key (exec-fn)]))
                  (mapv (fn [[key result]]
                          (let [r (util/deref? result)]
                            [key r])
                          [key (util/deref? result)]))))
        (partition-all 4 cases)))))

  (defn eval-equals
    "Add a :eval/result to the testcase. Value may be :pass, :fail, or :error."
    [tc]
    (assoc tc :eval/result
      (cond
        (:llm/error tc) :error
        (= (str/lower-case (:llm/answer tc))
          (str/lower-case (:tc/expected-answer tc))) :pass
        :else :fail)))

(defn eval-bracket
  "Add a :eval/result to the testcase. Value may be :pass, :fail, or :error."
  [keyfn tc]
  (if (:llm/error tc)
    (assoc tc :eval/result :error)
    (let [[_ match] (re-find #"\{\{(.*)\}\}" (:llm/answer tc))
          match (str/lower-case (str/replace match "\"" ""))
          expected (keyfn (:tc/record tc))
          expected (str/lower-case (str/replace expected "\"" ""))]
      (assoc tc :eval/result
        (if (= match expected) :pass :fail)))))


(defn benchmark
  "Run a full benchmark. Takes a benchmark name (which will be used as
  the log directory), a map of testsets, and test phases to run (as
  per the args to `matrix`).

   Emits (& writes) all results, with the testset id and testcase
  index (position within the testset) added to the matrix keys."
  [benchmark-name testsets & phases]
  (let [dir (str "benchmarks/" benchmark-name)]
    (if-not (.mkdirs (io/file dir))
      (throw (ex-info (<< "Benchmark {{benchmark-name}} already exists") {}))
      (do
        (doseq [[testset-id testset] testsets]
          (persist (str dir "/" (name testset-id) ".edn") testset))
        (let [matrix (apply matrix phases)
              results (apply merge (map
                                     (fn [[testset-id testset]]
                                       (execute-matrix testset-id testset matrix))
                                     testsets))]
          (persist (str dir "/result.edn") results)
          results)))))

(defn score
  "Given a result set, aggregate by the provided key(s) and return the
   number of successes and failures."
  [results & group-keys]
  (let [groups (group-by
                 (fn [k] (select-keys k group-keys))
                 (keys results))]
    (into {}
      (for [[group-id group] groups]
        (let [result-values (map #(get-in results [% :eval/result]) group)
              total (count result-values)
              counts (frequencies result-values)]
          [group-id {:counts counts
                     :success-rate (double (/ (:pass counts 0) total))
                     :failure-rate (double (/ (:fail counts 0) total))
                     :error-rate (double (/ (:error counts 0) total))
                     }])))))

(defn fullname
  [testcase]
  (str
    (get (:tc/record testcase) (ds/->QName "http://xmlns.com/foaf/0.1/" "givenName" "foaf"))
    " "
    (get (:tc/record testcase) (ds/->QName "http://xmlns.com/foaf/0.1/" "familyName" "foaf"))))

(comment

  ;; TODO: make sure turtle is actually rendering correctly


  ;; TODO: test stronger demarcationss in Clojure data

  (def data (ds/generate 1
              (ds/lookup-x-by-y (ds/humans 3)
                (ds/->QName "http://people.com/interests/1.0/" "favorite_color" "pi")
                (ds/->QName "http://xmlns.com/foaf/0.1/" "surname" "foaf"))))

  (clojure.pprint/pprint data)

    (clojure.pprint/pprint
      (:tc/dataset (first data)))

  (clojure.pprint/pprint
    (prompt/turtle-data (first data)))

  (clojure.pprint/pprint
    (prompt/ntriples-data (first data)))



  (time
    (do

      (def data {:data (ds/generate 100
                         (ds/choose-record (ds/humans 700)))})

      (def r (benchmark "cot-fullname-big-9" data
               :sys-prompt
               {:bracket prompt/bracket-result}
               :dataset-prompt
               {:csv prompt/csv-data
                :turtle prompt/turtle-data}
               :query-prompt
               {:nat-lang (partial prompt/answer "How tall is {{v}}?" fullname)
                :nat-lang-cot (partial prompt/answer "How tall is {{v}}? Figure out what you need to answer at each step before providing your final answer." fullname)
                }
               :exec
               {:openai (llm/openai :model "gpt-4o-mini")}
               :eval
               {:bracket (partial eval-bracket
                           #(get % (ds/->QName "http://data.gov/population/schema#"
                                     "height" "pop")))}))

      (clojure.pprint/pprint (score r :dataset-prompt :query-prompt))

      ))

  

  (def errs (filter #(= :error (:eval/result %)) (vals r)))

  (println (nth (:tc/prompt (nth (vals r) 3)) 1))


  (def ttl (filter #(= nil (:eval/result (second %))) r))

  (def ttl (filter #(= :turtle (:dataset-prompt (first %))) r))
  (def ttl-ntl (filter #(= :nat-lang-cot (:query-prompt (first %))) r))

  (def ttl-f (filter #(= :fail (:eval/result (second %))) ttl-ntl))
  


  )

(comment

  ; trials=100, n=500
  {{:dataset-prompt :ntriples}
   {:counts {:pass 64, :fail 36},
    :success-rate 0.64,
    :failure-rate 0.36,
    :error-rate 0.0},
   {:dataset-prompt :csv}
   {:counts {:pass 50, :fail 50},
    :success-rate 0.5,
    :failure-rate 0.5,
    :error-rate 0.0}}

  )
