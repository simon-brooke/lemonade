(ns lemonade.utils
  (:require [clojure.java.io :refer [writer]]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as s]
            [lemonade.globals :refer :all]))



(defn prompt-for-string
  [& prompts]
  (doall (map #(println %) prompts))
  (print "> ")
  (flush)
  (read-line))


(defn prompt-for-tokens
  [& prompts]
  (let [p (if
            (< (count prompts) 2)
            (concat prompts ["(separate tokens with spaces or commas)"])
            prompts)]
    (remove
      nil?
      (map
        #(if-not (zero? (count %))
           (keyword %))
        (s/split (s/replace (apply prompt-for-string p) ", " " ") #"\s")))))


(defn prompt-for-token
  [& prompts]
  (let [p (if
            (< (count prompts) 2)
            (concat prompts ["(separate tokens with spaces or commas)"])
            prompts)]
    (first (apply prompt-for-tokens p))))


(defn prompt-for-boolean
  [& prompts]
  (=
    :y
    (apply prompt-for-token (concat prompts ["y/n"]))))


(defn write-to-file [path object]
  (with-open [scrivenor (writer path)]
      (pprint object scrivenor)))


(defn perhaps-save
  "If the global `*autosave*` is defined and is a string, treat it as a
  file name to autosave to."
  [envir]
  (if
    (string? *autosave*)
    (try
      (write-to-file *autosave* envir)
      (println (str "Saved to " *autosave* "!"))
      true
      (catch Exception any
        (println (str "Failed to save to " *autosave* " because " (.getMessage any)))
        false))
    false))
