(ns lemonade.object
  (:require [clojure.pprint :refer [pprint]]
            [clojure.string :as s]
            [lemonade.globals :refer :all]
            [lemonade.utils :refer :all]))


(defn find-container
  "Find the id of the container that contains the object with this `id`,
   if it exists in this `envir`. Return the id of the container, or nil
   if not found."
  ([envir id]
   (first
     (remove
       nil?
       (mapcat
         #(find-container envir id %)
         (keys envir)))))
  ([envir id type-key]
   (let [containers (type-key envir)]
     (if
       (map? containers)
       (map
         #(if
            (some (fn [i] (= i id)) (:objects (% containers)))
            %)
         (keys containers))
       '()))))


(defn find-type
  "Find the id of the type of the item with this `id` in this `envir`, or
  nil if not found."
  ([envir id]
   (first
     (remove
       nil?
       (map
         #(find-type envir id %)
         (keys envir)))))
  ([envir id type-key]
   (if
     (id (type-key envir))
     type-key)))


(defn move-to
  "Return an environment like this `envir`, but with the object with the id
  obj-id removed from its current container and added to the container whose
  id is `target-id`."
  ;; TODO not working yet. The object id gets removed from the source but not
  ;; delivered to the target.
  [envir obj-id target-id]
  (let [source-id (find-container envir obj-id)
        source-type (find-type envir source-id)
        source-container (source-id (source-type envir))
        target-type (find-type envir target-id)
        target-container (target-id (target-type envir))]
    (println "source id: " source-id)
    (println "source type: " source-type)
    (pprint source-container)
    (println "target id: " target-id)
    (println "target-type: " target-type)
    (pprint target-container)
    (if
      (and source-container (:container target-container))
      (assoc
        envir
        target-type
        (assoc
          (target-type envir)
          target-id
          (assoc
            target-container
            :objects
            (cons obj-id (:objects target-container))))
        source-type
        (assoc
          (source-type envir)
          source-id
          (assoc
            source-container
            :objects
            (remove #(= % obj-id) (:objects source-container)))))
      (do
        (println (str target-id " is not a container"))
        envir))))


(defn add
  "Create or add an object to an environment.
  * `envir` should be an environment;
  * `id` should be the keyword that will identify
  the new object in the objects of the environment.

  Returns a modified environment."
  [envir id]
  (cond
    ;; no need to recreate an object if it already exists in the environment
    (and (keyword? id) (id (:objects envir))) envir
    (keyword? id)
    (do
      (println (s/join " " ["Creating object" (name id)]))
      (let
        [description (prompt-for-string "Describe this object")
         container (prompt-for-boolean "Is this object a container?")
         objects (if container (prompt-for-tokens "What objects are in this container?"))
         locked (if container (prompt-for-tokens "If this container is locked, what items can be used to open it?"))
         object {:id id
                 :description description
                 ;; an object can contain other objects only if it is a container
                 :objects objects
                 ;; only containers can be locked. The value of locked is a list of all the
                 ;; ids of objects that can open the container. Might later be optionally
                 ;; a function which could be invoked to open the container.
                 :locked locked
                 :container container
                 :object true
                 :type :object}]
        (pprint object)
        (reduce
          add
          (assoc
            envir
            :objects
            (assoc (:objects envir) id object))
          (concat objects locked))))
    true
    (do
      (println (str "'" id "' is not a valid object key"))
      envir)))

