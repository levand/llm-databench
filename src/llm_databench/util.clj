(ns llm-databench.util)

(defn env
  ([key]
   (or (System/getenv key)
     (throw (ex-info (str "issing env var " key) {:key key}))))
  ([key default]
   (or (System/getenv key) default)))

(defn promise? [v]
  (instance? clojure.lang.IPending v))

(defrecord Then [delegate f]
  clojure.lang.IDeref
  (deref [_] (f (deref delegate)))
  clojure.lang.IBlockingDeref
  (deref [_ timeout-ms timeout-val]
    (let [v (deref delegate timeout-ms timeout-val)]
      (if (= timeout-val v)
        v
        (f v))))
  clojure.lang.IPending
  (isRealized [this] (realized? delegate))
  clojure.lang.IFn
  (invoke [this val]
    (delegate val)))

(prefer-method print-method clojure.lang.IDeref clojure.lang.IRecord)
(prefer-method print-method clojure.lang.IDeref clojure.lang.IPersistentMap)

(defmacro then
  [[binding promise] & body]
  `(->Then ~promise
     (fn [~binding] ~@body)))

(defn pcomp
  "Compose two function, as in clojure.core/comp.

  If at runtime the value of `g` is a promise, the result will also be
  a promise delivering (f (deref g))."
  [f g]
  (fn [& args]
    (let [v (apply g args)]
      (if (promise? v)
        (then [v v] (f v))
        (f v)))))

(defn deref?
  "Deref, but returns the value if the value is not a promise"
  [v]
  (if (promise? v)
    @v
    v))


(comment

  (def p (promise))

  (def pp (then [v p] (inc v)))

  (deliver p 32)

  (deref pp 3000 :timeout)

  (macroexpand-1
    '(then [v p] (inc v)))

  



  )

