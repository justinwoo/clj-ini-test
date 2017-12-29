(ns clj-ini-test.core-test
  (:use [blancas.kern.core])
  (:require [clojure.test :refer :all]
            [clj-ini-test.core :refer :all]
            [cats.monad.either :as either]
            [cats.core :as cats]))

(deftest parser
  (testing "field"
    (is (= [:kiwi "grape"] (:value (parse field "kiwi=grape")))))
  (testing "section-name"
    (is (= "apple" (:value (parse section-name "[apple]")))))
  (testing "section"
    (is (= [:apple {:kiwi "grape"}] (:value (parse section "[apple]\nkiwi=grape")))))
  (testing "document"
    (is (= {:apple {:kiwi "grape"} :banana {:cat "dog"}} (:value (parse document "[apple]\nkiwi=grape\n[banana]\ncat=dog"))))))

(deftest schema-check
  (testing "read fields from section spec"
    (is (either/right? (read-fields-from-section-spec
                        {:apple :string}
                        {:apple "sdfds"})))
    (is (either/left? (read-fields-from-section-spec
                       {:banana :string}
                       {:apple "sdfds"})))
    (is (either/left? (read-fields-from-section-spec
                       {:apple :integer}
                       {:apple "sdfds"}))))
  (testing "read sections from document spec"
    (is (either/right?
         (read-sections-from-document-spec
          {:banana {:apple :integer}}
          {:banana {:apple "123"}})))
    (is (either/left?
         (read-sections-from-document-spec
          {:kiwi {:apple :integer}}
          {:banana {:apple "123"}})))))

(deftest schema-check
  (testing "read fields from section spec"
    (is (either/right? (read-fields-from-section-spec
                        {:apple :string}
                        {:apple "sdfds"})))
    (is (either/left? (read-fields-from-section-spec
                       {:banana :string}
                       {:apple "sdfds"})))
    (is (either/left? (read-fields-from-section-spec
                       {:apple :integer}
                       {:apple "sdfds"}))))
  (testing "read sections from document spec"
    (is (either/right?
         (read-sections-from-document-spec
          {:banana {:apple :integer}}
          {:banana {:apple "123"}})))
    (is (either/left?
         (read-sections-from-document-spec
          {:kiwi {:apple :integer}}
          {:banana {:apple "123"}})))))

(deftest toplevel-check
  (testing "works by putting things together"
    (is (either/right?
         (parse-and-check-document
          {:apple {:kiwi :string} :banana {:cat :string}}
          "[apple]\nkiwi=grape\n[banana]\ncat=dog")))))
