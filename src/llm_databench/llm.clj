(ns llm-databench.llm
  (:require [llm-databench.util :as util]
            [clj-json.core :as json]
            [org.httpkit.client :as http]))

(defn- invoke-openai
  "Return a promise containing a result map with either an :error or
  an :answer key."
  [[sys-prompt & prompts] & {:keys [temp model]
                             :or {temp 0
                                  model "gpt-4o-mini"}}]
  (let [body {:model model
              :temperature temp
              :messages (cons {:role "system"
                               :content sys-prompt}
                          (map (fn [prompt]
                                 {:role "user"
                                  :content prompt}) prompts))}
        resp (http/request
               {:headers {"Content-Type" "application/json"
                          "Authorization" (str "Bearer " (util/env "OPENAI_API_KEY"))}
                :method :post
                :url "https://api.openai.com/v1/chat/completions"
                :body (json/generate-string body)})]
    (util/then [r resp]
      (let [ret {:openai/response r}]
        (if (:error r)
          (assoc ret :llm/error {:reason :http-error})
          (try
            (let [body (json/parse-string (:body r) true)
                  answer (-> body
                           (:choices)
                           (first)
                           (:message)
                           :content)]
              (if (empty? answer)
                (assoc ret :llm/error {:reason :missing-answer})
                (assoc ret :llm/answer answer)))
            (catch Exception e
              (assoc ret :llm/error {:reason :exception
                                     :exception e}))))))))

(defn openai
  "Return a function which invokes a testcase using OpenAI and the
  provided options."
  [& opts]
  (fn [testcase]
    (let [prompts (:tc/prompt testcase)
          p (invoke-openai prompts opts)]
      (util/then [r p]
        (merge testcase r)))))

(comment
  @(invoke-openai ["Answer using a single word, number or proper noun."
                   "The candidate answers are the `US of A`, `Portugul` and `Russia`."
                   "What has the largest population of spanish speakers?"])

  )
