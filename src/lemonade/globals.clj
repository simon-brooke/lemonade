(ns lemonade.globals)


(def all-dirs [:north :east :south :west :up :down])

(def empty-envir {:players {}
                  :rooms {}
                  :objects {}})

(def ^:dynamic *autosave* "autosave.edn")
