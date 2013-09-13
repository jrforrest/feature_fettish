(ns 'feat_fettish.parallel-fetch
  "Provides a pmap-bounded function")


(defn pmap-bounded
  "Returns a lazy sequence containing the results of applying
  f to each item in the given input-seq
  
  Results are returned out of order"
  [f input-seq & opts])

(defn- make-output-buffer
  [max-size]
  (atom clojure.lang.PersistentQueue/Empty
        :validator #(< (count %) max-size)))

(defn- ref-pop
  "Pops an item from the given reference to a sequence, leaving it
  set to its tail"
  [seq-ref]
  (dosync
    (let [head (first @seq-ref)]
      (alter seq-ref #(rest %))
      head)))

(defn- blocking-push
  "Repeatedly attempts to push the given value to the reference to
  a queue given in queue-ref.  A 200 ms sleep is performed in between
  attempts
  
  This method seems pretty shitty, as there's going to be a lot of contention
  overhead with a high number of threads going after the same output queue, as
  there most definitely will be with the intended application.

  Perhaps there's a lower-level block-on-mutex function or something like
  that in the clojure libs, or maybe this needs implemented with Java
  concurrency primitives"
  [queue-ref value]
  (loop []
    (try (swap! queue-rf conj value)
         (catch IllegalStateException e (Thread/sleep 200))
         (recur))))

(defn- make-pool
  [f input-seq max-workers buffer-size]
  (let [pool (for [x (range n-threads)] (agent (list)))
        ; The given sequence will get popped by each agent
        ; as it consumes work, so it needs to be a ref
        queue (ref input-seq)

        errors (atom (list))

        ; Pops an item off of queue, setting queue to rest
        q-pop %(ref-pop queue)
        output-buffer (make-output-buffer)
        push-output %(blocking-push output-buffer %)

        process-item (fn [item]
                       (try
                         (blocking-push (f item))
                         (catch Exception e)
                         (swap! errors conj e)))

        ; Pops from the queue till' it's dry, returning the results
        ; of the work in a map containing :output and :errors.  :output
        ; is a vector of the values resulting from the processing.
        ; :errors holds pairs of [given-value, Exception] for each failed item.
        do-work (fn []
                  (loop [[output errors] [(list) (list)]]
                    (if-let [next-item (q-pop)]
                      (recur (process-item output next-item errors))
                      {:output output, :errors errors})))

        ; Merges the results from all of the workers into one map
        combine-results (fn []
                          (reduce
                            (partial merge-with concat)
                            {:output [], :errors []}
                            (map deref pool)))
                          
        ; Concats the result of do-work with an agents current state
        run-worker (fn [b] (do-work))]
    ; Calls run-worker on each worker, utilizing 
    ; clojure's unbound thread-pool
    (doseq [w pool] (send-off w run-worker))
    ; Waits for all workers to complete
    (doseq [w pool] (await w))
    (doseq [w (map deref pool)]
      (prn w))
    (combine-results)))
