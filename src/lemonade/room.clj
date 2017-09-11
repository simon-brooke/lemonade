(ns lemonade.room
  (:require [clojure.pprint :refer [pprint]]
            [clojure.string :as s]
            [lemonade.globals :refer :all]
            [lemonade.object :as object]
            [lemonade.utils :refer :all]))


(defn get-neighbours
  "Return a list of the ids of the neighbours of this `room` map,
  or of the room identified by this `id` in this `envir`."
  ([room]
   (remove nil? (map #(% room) all-dirs)))
  ([envir id]
   (get-neighbours (id (:rooms envir)))))


(defn get-undescribed-rooms
  "Return a list of the ids of rooms which are referenced in this
  `envir`, but not present as rooms in it.

  NOTE: if the envir was built using the environment builder, there
  ought not to be any."
  [envir]
  (remove
    (fn [id] ((:rooms envir) id))
    (set
      (mapcat
        #(get-neighbours envir %)
        (keys (:rooms envir))))))


(defn add
  "Create or add a room to an environment.
  * `envir` should be an environment;
  * `id` should be the keyword that will identify
  the new room in the rooms of the environment.

  Returns a modified environment."
  [envir id]
  (perhaps-save envir)
  (cond
    ;; no need to recreate a room if it already exists in the environment
    (and (keyword? id) (id (:rooms envir))) envir
    (keyword? id)
    (do
      (println (s/join " " ["Creating room" (name id)]))
      (let
        [description (prompt-for-string "Describe this room")
         objects (prompt-for-tokens "What objects are in this room?")
         room (reduce
                #(let
                   [neighbour (prompt-for-token
                                (str "What room lies " %2 " of " id "?"))]
                   (if
                     neighbour (assoc %1 %2 neighbour)
                     %1))
                {:id id
                 :objects objects
                 :description description
                 :container true
                 :room true
                 :type :room}
                all-dirs)
         with-room (assoc
            (reduce object/add envir objects)
            :rooms (assoc (:rooms envir) id room))]
        (pprint with-room)
        (reduce
          add
          with-room
          (get-neighbours room))))
    true
    (do
      (println (str "'" id "' is not a valid room key"))
      envir)))
