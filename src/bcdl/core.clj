(ns
  bcdl.core
  (:gen-class)
  (:require [clojure.java [io :as io] [shell :as shell]]
            [clojure.string :as string]
            [clojure.data.json :as json]
            [clojure.contrib.command-line :as cli])
  (:import [org.jaudiotagger audio.AudioFileIO tag.FieldKey tag.Tag]))

(def download-save-path "./downloads")

(defn save-url-content [url save-path]
  "Downloads and saves content at `url` to `save-path`."
  (println (format "Saving %s to %s" url save-path))
  (with-open [in (io/input-stream url)
              out (io/output-stream save-path)]
    (io/copy in out)))

(defn valid-filename [uri]
  "Converts a string to valid filename by replacing illegal characters with '-'."
  (string/replace uri
    #"[^\w\s\-.\,/\(\)\[\]']" "-"))

(defn track-save-name [track]
  "Returns a filename string that should be used to save the
  downloaded audio for `track` to."
  (apply format "%s - %s.mp3"
    (map valid-filename (map track [:number :title]))))

(defn album-save-path [{band :band, album :album}]
  "Returns a path to directory where audiofiles from an album
  should be saved to. The parameter is an album-data map."
  (apply format "%s/%s/%s - %s"
    (map valid-filename [download-save-path
                         (:name band)
                         (:year album)
                         (:title album)])))

(defn track-save-path [album-data track-info]
  "Returns save path for downloaded track like:
  <dl-path>/<artist>/<year> - <album>/<number> - <title>.mp3"
  (format "%s/%s"
    (album-save-path album-data)
    (track-save-name track-info)))

(defn get-track-info [html]
  "Returns track list from html string."
  (json/read-json
    (last
      (re-matches ;; Java can't do named groups.
        #"(?s)^.*var TralbumData = \{.*trackinfo : (\[.*?\]),.*?playing_from.*$"
        html))))

(defn get-band-info [html]
  "Returns band data from html string. For now just the band's name as I
  have no idea how to painlessly parse underlying Javascript."
  {:name
    (last (re-matches
            #"(?s)^.*var BandData = \{.*?name : \"(.*?)\".*$"
            html))
   })

(defn get-album-info [html]
  "Returns album data map from html string."
  (let
    [data (json/read-json
      (last (re-matches
              #"(?s)^.*var TralbumData = \{.*?current: (\{.*?\}),.*?is_preorder.*$"
              html)))]
    (assoc data
      ;; adding a release year as the last 4 chars in the 'release_date' key.
      :year (apply str (take-last 4 (:release_date data))))))

(defn get-album-data [album-url]
  "Returns a conglomerate of band, album and tracklist info, parsed
  from html at `album-url`."
  (let [html (slurp album-url)]
    {:band (get-band-info html)
     :album (get-album-info html)
     :tracks (map-indexed
               #(assoc %2 :number (str (inc %1))) ;; adding track numbers
               (get-track-info html))
     }))

(defn add-id3-tags
  [mp3-file & {:keys
                [artist album year title track-number]
                :or
                {artist "Unknown" album "Untitled" year ""
                 title "Untitled" track-number ""}}]
  "Adds ID3 tags to audiofile."
  (let [f (AudioFileIO/read (new java.io.File mp3-file))]
    (let [tag (.getTagOrCreateAndSetDefault f)]
      (.setField tag FieldKey/ARTIST artist)
      (.setField tag FieldKey/ALBUM album)
      (.setField tag FieldKey/YEAR year)
      (.setField tag FieldKey/TITLE title)
      (.setField tag FieldKey/TRACK track-number)
      (.commit f))))

(defn download-track [track-info album-data]
  "Downloads the track and saves is to appropriate location,
  adding ID3 tags afterwards."
  (let [save-path (track-save-path album-data track-info)
        {album :album band :band} album-data]
    (save-url-content (:file track-info) save-path)
    (add-id3-tags save-path
      :artist (:name band)
      :album (:title album)
      :year (:year album)
      :title (:title track-info)
      :track-number (:number track-info))))

(defn download-album [album-url]
  "Main function that does all the job."
  (println (format "Getting album info at %s" album-url))
  (let [album-data (get-album-data album-url)]
    (println (format "Downloading %s by %s"
               (:title (:album album-data))
               (:name (:band album-data))))

    (.mkdirs (new java.io.File (album-save-path album-data)))

    (doseq [track (:tracks album-data)]
      (println (apply format "Downloading %s. %s" (map track [:number :title])))

      (download-track track album-data))))

;;

(defn -main [& args]
  (println "A script for downloading Bandcamp albums. Use --help for help.")
  (cli/with-command-line args
    ""
    [[album-url "URL to Bandcamp album."]]
    (download-album album-url)))