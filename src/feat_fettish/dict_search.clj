(ns feat_fettish.dict-search
  "Provides binary-search on the machine's words file"
  (:import [ java.io RandomAccessFile ] ))

(def dict-file
  "Reference to an open handle on the /usr/share/dict/words file"
  (ref (new RandomAccessFile "/usr/share/dict/words" "r")))

(defn- position
  "Returns the current position pointer in the given file"
  [^RandomAccessFile file]
  (.position (.getChannel file)))

(defn- midpoint
  "Finds the midpoint between the given window start and end points"
  [start end]
  (+ (/ (- end start)
        2)
     start))

(defn- seek-to-next-line
  "Seeks to the next line after the given position in the given file"
  [file position]
  (.seek file position)
  (.readLine file))

(defn- binary-search-file-window
  "Performs a binary search on the given denoted by the
  given start and end given file, searching for the given word."
  [^RandomAccessFile file word start end]
    (if (>= start end) 
      false
      (do
        (seek-to-next-line file (midpoint start end))
        (let [mid (midpoint start end)
              new-end (- mid 1)
              word-in-file (.readLine file)
              new-start (+ mid 1)
              word-comparison (compare word word-in-file)]
          (cond 
            (> new-start end) false
            (= word-comparison 0) true
            (< word-comparison 0) (recur file word start new-end)
            (> word-comparison 0) (recur file word new-start end))))))

(defn- binary-search-file
  "Searches for the given word within the given file"
  [file word]
  (let [start 0
        end (.length file)]
    ; I wasn't going to worry about this case, but the first word
    ; in the dict is A, kind of important I guess.
    (if (= (.readLine file) word)
      true
      (binary-search-file-window file word start end))))

(defn in-dict? 
  "Searches for the given word in the /usr/share/dict/words file using
  a binary search.  Returns true if the given word is present, false
  otherwise.  Case-sensitive."
  [word]
  (dosync (binary-search-file @dict-file word)))
