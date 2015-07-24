(require 'company-elm)
(require 'ert)

(defconst default-import-list "
import Basics exposing (..)
import List exposing ( List, (::) )
import Maybe exposing ( Maybe( Just, Nothing ) )
import Result exposing ( Result( Ok, Err ) )
import Signal exposing ( Signal )
")

(defconst example-import-list "
module Main where

import Color exposing (Color)
import Graphics.Element exposing (..)
import Graphics.Input as Input
import Signal exposing (Signal, Address)
import Text

")

(ert-deftest test-get-import-matches-in-buffer-should-return-all-imports ()
  (with-temp-buffer
    (insert example-import-list)
    (goto-char 0)
    (let ((matches (company-elm--parse-imports-in-buffer)))
      (should (= 5 (length matches))))))

(defun test-import (import name as exposing)
    (string-match company-elm--import-regex import)
    (let* ((import (company-elm--get-import-from-match-data import)))
      (should (equal (assoc 'name import) (cons 'name name)))
      (should (equal (assoc 'as import) (cons 'as as)))
      (should (equal (assoc 'exposing import) (cons 'exposing exposing)))))

(ert-deftest test-get-import-from-match-data-full ()
  (test-import "import Mod.SubMod as El exposing (foo, bar)"
               "Mod.SubMod"
               "El"
               '("bar" "foo")))

(ert-deftest test-match-import-single-module ()
  (test-import "import Foo" "Foo" nil ()))

(ert-deftest test-match-import-with-alias ()
  (test-import "import Foo as F" "Foo" "F" nil))

(ert-deftest test-match-import-with-single-expose ()
  (test-import "import Foo exposing (test)" "Foo" nil '("test")))

(ert-deftest test-match-import-with-multiple-exposings-tight ()
  (test-import "import Foo exposing (test1,test2)" "Foo" nil '("test2" "test1")))

(ert-deftest test-match-import-with-multiple-exposings-loose ()
  (test-import "import Foo exposing ( test1 , test2 )" "Foo" nil '("test2" "test1")))

(ert-deftest test-match-import-with-alias+multiple-exposings-tight ()
  (test-import "import Foo as F exposing (test1,test2)" "Foo" "F" '("test2" "test1")))

(ert-deftest test-match-import-with-alias+multiple-exposings-loose ()
  (test-import "import Foo as F exposing ( test1 , test2 )" "Foo" "F" '("test2" "test1")))

(ert-deftest test-match-import-with-pattern-match-loose ()
  (test-import "import Foo exposing ( Maybe ( Just, Nothing ) )"
               "Foo" nil '("Nothing" "Just")))

(ert-deftest test-match-import-with-pattern-match-tight ()
  (test-import "import Foo exposing (Maybe(Just,Nothing))"
               "Foo" nil '("Nothing" "Just")))

(ert-deftest test-elm-get-load-default-dependencies ()
  (let ((company-elm--retrieve-synchronous t)
        company-elm-documentation)
    (company-elm--load-dependencies "test" '(("elm-lang/core" . "2.1.0")))
    (should (equal (length company-elm-documentation) 31))))

(ert-deftest test-elm-get-candidates-no-prefix ()
  "Test that the correct completion candidates are returned for no prefix"
  (let ((company-elm--retrieve-synchronous t)
        company-elm-documentation)
    (company-elm--load-dependencies "test" '(("elm-lang/core" . "2.1.0")))
    (let ((candidate (company-elm--get-candidates "")))
      (should (equal (length candidates) 31)))))
