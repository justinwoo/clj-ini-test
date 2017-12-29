(ns clj-ini-test.core
  (:use [blancas.kern.core])
  (:require [cats.monad.either :as either]
            [cats.core :as cats]
            [clojure.string :as string]))

(def skip-spaces
  "skip one or more spaces and return unit"
  (<|> (>> (skip-many1 white-space)
           skip-spaces)
       (return nil)))

(defn lexeme
  "define a lexeme for a given parser"
  [p]
  (<< p skip-spaces))

(def field
  "parse a key=value field into a [key, value] pair"
  (let [key (<+> (many-till any-char (sym* \=)))
        value (<+> (many-till any-char (<|> (sym* \newline) eof)))]
    (lexeme
     (>>=
      key
      (fn [k]
        (>>=
         value
         (fn [v]
           (return [(keyword k) v]))))))))

(def section-name
  "parse a section name from surrounding square brackets"
  (lexeme
   (>> (sym* \[)
       (<+> (many-till any-char (sym* \]))))))

(def section
  "parse a section into a [key, value] pair"
  (let [name section-name
        fields (<$> (fn [xs] (into (hash-map) xs))
                    (many-till field
                               (<|> (predict (sym* \[)) eof)))]
    (lexeme
     (>>=
      name
      (fn [n]
        (>>=
         fields
         (fn [fs]
           (return [(keyword n) fs]))))))))

(def document
  "parse a document of sections as a hashmap of hashmap of strings"
  (let [sections (<$> (fn [xs] (into (hash-map) xs))
                      (many section))]
    (>> skip-spaces
        (lexeme sections))))

; for now i'll just work with a "hashmap schema"
(def string-value "a symbol to represent that a string is desired" :string)
(def boolean-value "a symbol to represent that a boolean is desired" :boolean)
(def integer-value "a symbol to represent that a integer is desired" :integer)
(def string-array-value "a symbol to represent that a string array is desired" :string-array)

(defn read-field-from-field-spec
  "read field from field schema."
  [schema value]
  (case schema
    :string (either/right value)
    :boolean (case (string/lower-case value)
               "false" (either/right false)
               "true" (either/right true)
               (either/left (str "Unknown boolean field " value)))
    :integer (let [n (try (Integer/parseInt value) (catch Exception e nil))]
               (if (nil? n)
                 (either/left (str "Could not read integer from " value))
                 (either/right n)))
    :string-array (string/split value #",")
    (either/left (str "Unknown schema " schema " given"))))

(defn generic-inner-hashmap-operation
  "define generic hashmap operation to perform with document and section levels."
  [f level-name child-name]
  (fn [schema section]
    (let [f (fn [acc name inner-schema]
              (cats/>>=
               acc
               (fn [acc']
                 (let [value (name section)]
                   (if (nil? value)
                     (either/left (str "could not find " level-name " " name " in " child-name))
                     (cats/fmap
                      (fn [read] (assoc acc' name read))
                      (f inner-schema value)))))))]
      (reduce-kv f (either/right {}) schema))))

(def read-fields-from-section-spec
  "read fields from schema spec."
  (generic-inner-hashmap-operation
   read-field-from-field-spec
   "field"
   "section"))

(def read-sections-from-document-spec
  "read sections from document spec."
  (generic-inner-hashmap-operation
   read-fields-from-section-spec
   "section"
   "document"))

(defn parse-and-check-document
  "parse a string ini document and check it against a schema"
  [schema string]
  (let [parsed (parse document string)]
    (if (:ok parsed)
      (read-sections-from-document-spec schema (:value parsed))
      (either/left "failed to parse document"))))
