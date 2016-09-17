(ns web-scraper.core
  (:require [hickory.core :refer [parse as-hiccup]]
            [hickory.zip :refer [hiccup-zip]]
            [clojure.zip :as zip]
            [clojure.string :as str]
            [cheshire.core :refer [generate-string]]))


(defn get-url-hiccup [url]
  (-> (slurp url)
      parse
      as-hiccup))

(defn extract-links
  ([hiccup] (extract-links (hiccup-zip hiccup) #{}))
  ([zip links]
   (let [[tag attrs] (zip/node zip)
         next (zip/next zip)]
     (cond
       (zip/end? zip) links
       (= :a tag) (recur next (conj links (:href attrs)))
       :else (recur next links)))))

(defn scrape
  ([url] (scrape url 2))
  ([url max-depth] (scrape url "/" max-depth))
  ([domain route depth]
   (if (zero? depth)
     {route {}}
     (let [hiccup (get-url-hiccup (str domain route))]
       {route (->> (extract-links hiccup)
                   (filter (complement nil?))
                   (filter #(str/starts-with? % "/"))
                   (map (fn [child-route] (scrape domain child-route (dec depth))))
                   (into {}))}))))

(defn save-as-json [map filename]
  (->> map
       generate-string
       (spit filename)))

(defn save-site-map [url & {:keys [max-depth] :or {max-depth 2}}]
  (save-as-json (scrape url max-depth) "site-map.json"))
