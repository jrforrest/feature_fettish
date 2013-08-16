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
    (if (> (count (:word-counts post)) 200) 
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
  "
  [ex-classes [ex-var & rescue-exprs] & body]
  `(try ~@body ~@(map (fn [ex] `(catch ~ex ~ex-var ~@rescue-exprs)) ex-classes)))

(defn- with-body
  "Fetches the body for the given post as a string,
  uses boilerpipe to attempt to identify the content body,
  but will likely return garbage for non-article pages.  Also
  does not handle multi-page articles yet.
  
  Will fetch the wordcounts from the datastore instead of the
  web page it's hosted on if it's there already.
  
  If the post is fetched from the datastore, it persists it."
  [post]
  (let [stored-post (post/get-post (:url post))]
    (if (nil? stored-post)
      (do
        (rescue [java.io.FileNotFoundException
                 java.net.UnknownHostException
                 java.io.IOException]
                [e (log/warn (format "Fetching of %s failed: %s"
                                     (:url post)
                                     (.getMessage e)))]
                (log/info (str "Fetching from web: " (:url post)))
                (scrape-post post)))
      (do
        (log/info (str "Fetched from DS: " (:url post)))
        (assoc post :word-counts (:word-counts stored-post))))))

(defn subreddit-posts
  "A lazy sequence of all posts from the given subreddit"
  ([subreddit]
   (subreddit-posts subreddit (subreddit-pages subreddit)))
  ([subreddit subreddit-pages]
   (subreddit-posts subreddit subreddit-pages
                    (http-posts-only (posts-on-page (first subreddit-pages)))))
  ([subreddit subreddit-pages page-posts]
   (if (empty? page-posts)
     (subreddit-posts subreddit (next subreddit-pages))
     (cons (with-body (first (remove nil? page-posts)))
           (lazy-seq (subreddit-posts subreddit subreddit-pages
                                      (rest page-posts)))))))
