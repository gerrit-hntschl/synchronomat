(ns synchronomat.import-utils
    (:use [clojure.contrib.import-static])
      (:import (java.lang.reflect Modifier)) )


(defmacro import-static-fields
  "Imports all static final fields of the class as (private) symbols
  in the current namespace.

  Example: 
      user> (import-static-fields java.lang.Integer)
      #'user/TYPE
      user> MAX_VALUE
      2147483647

  Note: The class name must be fully qualified, even if it has already
  been imported."
  [class]
  (let [final-static-field? (fn [field]
                  (let [modifiers (.getModifiers field)]
                (and (Modifier/isStatic modifiers) (Modifier/isFinal modifiers))))
    static-fields (map #(.getName %)
               (filter
                final-static-field?
                (.. Class (forName (str class)) getFields)))]
    `(import-static ~class ~@static-fields)))
