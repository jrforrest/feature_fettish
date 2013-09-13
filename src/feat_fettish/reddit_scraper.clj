(ns feat_fettish.reddit-scraper
  "Contains logic for scraping data from Reddit posts"
  (:require [ net.cgrand.enlive-html :as html ]
            [ boilerpipe-clj.core :as boilerpipe ]
            [ feat_fettish.word-count :as word-count]
            [ clojure.tools.logging :as log ]
            [ feat_fettish.post :as post ]))

(defn- subreddit-page-url
  "Builds a URL for the given subreddit for top posts of all time
  after the given id.  Defaults to the top of the list if no 
  after-id is given"
  ([subreddit]
    (subreddit-page-url subreddit nil))
  ([subreddit after-id]
  (let [after-opt (if after-id (str "&after=" after-id) nil)
        url (format "http://www.reddit.com/r/%s/top/?sort=top&t=all" subreddit)]
    (str url after-opt))))

(defn- http-posts-only
  "Returns only those of the given posts that contain an
  http url"
  [posts]
  (filter #(if (:url %)
             (re-matches #"^http\://.*$" (:url %)))
          posts))

(defn- subreddit-page
  "Loads the given page into an HTML object, raising a
  PageLoadError if it could not be loaded."
  [subreddit after-id]
  (let [url (subreddit-page-url subreddit after-id)
        body (html/html-resource (new java.net.URL url))]
    (if (not body)
      (throw (new Exception (format "Could not load: %s" url))))
    body))

(defn- last-post-id
  "Determines the id of the last post on the given page of posts"
  [body]
  (let [thing-divs (html/select body [:div.thing])]
    (->> thing-divs
         (map #(get-in % [:attrs :data-fullname]))
         last)))

(defn- subreddit-pages
  "A lazy sequence of urls for all pages in a given subreddit"
  ([subreddit]
   (subreddit-pages subreddit nil))
  ([subreddit after-id]
   (let [this-page (subreddit-page subreddit after-id)]
   (cons this-page
         (lazy-seq (subreddit-pages subreddit (last-post-id this-page)))))))

(defn- posts-on-page
  "Initializes posts for all of the links on a reddit page
  reachable at the given URL"
  [body]
  (let [links (html/select body [:a.title])]
    (map #(hash-map :url (:href (:attrs %))
                    :title (first (:content %)))
         links)))

(defn- scrape-post
  "Scrapes the web from the series of tubes from whence it came,
  creating word counts with the primary article content (as determined
  by boilerpipe) and returning nil if word-counts contains less than 400
  distinct words (probably means it was non-textual content)"
  [post]
  (let [body (->> (:url post) slurp boilerpipe/get-text)
             word-counts (word-count/make-wordcounts body)
             post* (assoc post :word-counts word-counts)]
    (if (> (count (:word-counts post*)) 200) 
      (do
        (post/save-post post*)
        post*)
      nil)))

(defmacro rescue 
  "Executes body, catching any exceptions of the types given in ex-classes.
  When an exception is caught, the given rescue-exprs are evaluated with
  the symbol given in ex-var bound to the exception object
  
  (rescue [java.io.IOException java.io.FileNotFoundException]
          [e (prn e)]
    (binding [*out* file-writer]
      (prn \"Hello werld!\")))

  todo: Extract this out to another ns
  "
  [ex-classes [ex-var & rescue-exprs] & body]
  `(try ~@body ~@(map (fn [ex] `(catch ~ex ~ex-var ~@rescue-exprs)) ex-classes)))

(defn- with-word-counts
  "Fetches the body for the given post, and returns the post
  with a the word counts from the fetched body.

  Uses boilerpipe to attempt to identify the content body,
  but will likely return garbage for non-article pages.  Also
  does not handle multi-page articles yet.
  
  Will fetch the wordcounts from the datastore instead of the
  web page it's hosted on if it's there already.
  
  If the post is fetched from the datastore, it persists it."
  [post]
  (let [stored-post (post/get-post (:url post))]
    (if (nil? stored-post)
      ;; Letting errors occur here now so they go upstream to the worker
      (scrape-post post)
      (assoc post :word-counts (:word-counts stored-post)))))

(defn- subreddit-posts
  "A lazy sequence of all posts from the given subreddit
  
  posts with empty bodies are not included."
  ([subreddit]
   (subreddit-posts subreddit (subreddit-pages subreddit)))
  ([subreddit subreddit-pages]
   (subreddit-posts subreddit subreddit-pages
                    (http-posts-only (posts-on-page (first subreddit-pages)))))
  ([subreddit subreddit-pages page-posts]
   (if (empty? page-posts)
     (subreddit-posts subreddit (next subreddit-pages))
     (cons (first page-posts)
           (lazy-seq (subreddit-posts subreddit subreddit-pages
                                      (rest page-posts)))))))

(defn- parallel-process-bounded
  "Initializes a set of n-threads agents to apply the given function f to
  each item of the given input-seq in parallel on the unbounded threadpool
  
  The default error handling strategy is to skip the element in the list
  and return it, mapped to the exception which occured in processing it with
  f to a map which is returned after the result set
  
  Note:  This is starting to get a little hairy.  Maybe this function
  should get busted out into its own namespace.  I suppose I'd want to
  re-use it elsewhere."
  [f input-seq n-threads]
  (let [err-log (ref (list))
        make-worker #(agent (list)
                           :error-mode :continue
                           :error-handler (fn [agent err]
                                            (dosync alter err-log conj err)))
        pool (for [x (range n-threads)] (make-worker))
        ; The given sequence will get popped by each agent
        ; as it consumes work, so it needs to be a ref
        queue (ref input-seq)
        ; Pops an item off of queue, setting queue to rest
        q-pop (fn [] (dosync (let [x (first @queue)]
                               (alter queue #(rest %))
                               x)))

        output (ref (list))
        ; Appends the given item to the output
        output-push (fn [item] 
                      (dosync (alter output conj item)))

        ; Pops from the queue till' it's dry, returning the results
        ; of the work in a map containing :output and :errors.  :output
        ; is a vector of the values resulting from the processing.
        ; :errors holds pairs of [given-value, Exception] for each failed item.
        do-work (fn []
                  (loop []
                    (if-let [next-item (q-pop)]
                      (do
                        (output-push (f next-item))
                        (recur)))))
        ; Concats the result of do-work with an agents current state
        run-worker (fn [b] (do-work))]
    ; Calls run-worker on each worker, utilizing 
    ; clojure's unbound thread-pool
    (doseq [w pool] (send-off w run-worker))
    ; Waits for all workers to complete
    (doseq [w pool] (await w))
    @output))

(defn top-posts 
  "Note: This doesn't actually gurantee it will return the 
  requested number of posts, as those that fail will be removed
  
  Note: I want to figure out how to do this with a lazy sequence rather
  than needing to keep all of these fetches in memory"
  [subreddit n & opts]
  (let [opts (apply hash-map opts)
        parallel-fetch (fn [posts]
                         (parallel-process-bounded with-word-counts posts
                                                   (get :n-threads opts 10)))
        prnit (fn [x] (prn x) x)]
    (->> subreddit
         subreddit-posts
         (take n)
         parallel-fetch
         (remove nil?))))
