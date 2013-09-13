(ns feat_fettish.display-server
  "Runs an http server for rendering the progress and results of the
  computations performed by the applications other modules"
  (:require [org.httpkit.server :as server]
            [hiccup core page]
            [clojure.data.json :as json]))

(defn- layout [content]
  "Wraps the given content string in an HTML layout."
  (hiccup.page/html5
    [:head
     [:title "Shityea Feat Fettish!"]
     (hiccup.page/include-js "https://ajax.googleapis.com/ajax/libs/jquery/1.7.1/jquery.min.js")
     [:style 
      "table { border 2px solid black };"]]

    [:body
     [:div#header]
     [:div#wrapper content]]))

(defn- col [x] [:td x])
(defn- row [& x] (apply vector :tr x))

(defn- map-to-table
  "Maps the given sequence of items into a table of sections, created by
  applying the given section-fn to each each index, item pair."
  [section-fn items]
  (let [sections (reduce concat [] (map-indexed section-fn items))
        table (apply vector :table sections)]
    (hiccup.core/html table)))

(defn- entity-section
  "Returns a table section which maps the entity of the given name to
  a row for each of the given child entities with the columns yielded by
  the given cols-fn"
  [entity-name child-entities cols-fn]
  (let [first-row (apply row
                         [:td {:rowspan (count child-entities)} entity-name]
                         (cols-fn (first child-entities)))
        rest-rows (map #(apply row (cols-fn %)) (rest child-entities))]
    (apply vector first-row rest-rows)))

(defn- render-posts-table
  [posts]
  (map-to-table (fn [number post]
                  (let [features (:top-features post)
                        cols-fn #(list (col (str "Feature #" (:feature-number %)))
                                       (col (:score %)))]
                    (entity-section (:title post) features cols-fn)))
   posts))

(defn- render-features-table
  "Renders (generates html for) the given features list as a table."
  [features]
  (map-to-table (fn [number feature-words]
                  (let [feature-name (str "Feature#" number)
                        cols-fn #(list (col (:word %)) (col (:weight %)))]
                    (entity-section feature-name feature-words cols-fn)))
                features))

(defn- render-computation-status
  "Renders the given computation-status (post-set) as html.
  
  This displays a features table, which describes each feature
  in terms of its highest weighted words, and an posts table,
  which describes each article in terms of its highest weighted features"
  [computation-status]
  (hiccup.core/html
    [:h3.features]
    (render-features-table (:features computation-status))
    (render-posts-table (:posts computation-status))))

(defn- word-breakdown
  "Returns a map of the words that occur in the given post-set to the
  vectors of posts which contain them"
  [post-set]
  (let [all-words (keys (:article-words post-set))
        posts-with-word (fn [word]
                          (filter 
                            #(contains? word (:word-counts %))
                            (:posts post-set)))]
    (map posts-with-word all-words)))

(def computation-status
  "Holds the status of the computation currently being performed,
  for reporting back to the user via the http frontend
  
  This is flawed, in that the status of only one computation may be
  tracked at any time.  Perhaps this should be a dictionary of computation-ids
  to the state of the computation.  Each post-set then could track its ID."
  (agent {}))

(defn- make-body
  [request]
  (layout (render-computation-status @computation-status)))

(defn- http-frontend
  [request]
  (let [body (make-body request)]
    {:status 200, :headers {"Content-Type" "text/html"}, :body body}))

(def server-handle
  "htpt-kit server handle for starting, stopping this applications
  internal server.  This server threads internally so there should
  only ever be a need for one instance of it"
  (agent nil))

(def server-opts
  "Configuration options for the server.  
  See http://http-kit.org/server.html#options"
  {:port 8080})

(defn- init-server
  "Starts the server if it hasn't been started yet"
  [server-handle]
  (if-not server-handle
    (server/run-server http-frontend server-opts)))

(defn- server-status
  "Determines the current (as of the time the server-handle agent
  responds) status of the http server.
  
  Returns one of the status symbols [:running, :stopped]

  Blocks until the server agent responds"
  [])

(defn run-server
  []
  (send server-handle init-server))
