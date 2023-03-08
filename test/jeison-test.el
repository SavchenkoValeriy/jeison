;;; jeison-tests.el --- Tests for jeison -*- lexical-binding: t; -*-

;; Copyright (c) 2019 Valeriy Savchenko (GNU/GPL Licence)

;; This file is NOT part of GNU Emacs.

;; jeison is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; jeison is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with jeison.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides the tests for `ert'. Run interactively or
;; using "run-tests.sh" shell script.

;;; Code:
(require 'ert)
(require 'jeison)

(ert-deftest jeison:check-predicates ()
  (jeison-defclass jeison:jeison-class nil nil)
  (defclass jeison:usual-class nil nil)
  (should (jeison-class-p jeison:jeison-class))
  (should (not (jeison-class-p jeison:usual-class)))
  (should (jeison-object-p (jeison:jeison-class)))
  (should (not (jeison-object-p (jeison:usual-class))))
  (should (not (jeison-class-p 'number)))
  (should (not (jeison-class-p (type-of "string literal"))))
  (should (not (jeison-object-p 42))))

(ert-deftest jeison:check-paths ()
  (jeison-defclass jeison:jeison-class nil
    ((x :initarg :x :path (a b))
     (y :initarg :y :type list)))
  (let* ((slots (jeison--get-slots jeison:jeison-class))
         (x (car slots))
         (y (cadr slots)))
    (should (eq 'x (oref x name)))
    (should (eq :x (oref x initarg)))
    (should (eq t (oref x type)))
    (should (equal '(a b) (oref x path)))
    (should (eq 'y (oref y name)))
    (should (eq :y (oref y initarg)))
    (should (eq 'list (oref y type)))
    (should (equal 'y (oref y path)))))

(ert-deftest jeison:check-read-path-basic ()
  (should (equal 42 (jeison--read-path
                     (jeison--read-json-string "{\"a\": {\"b\": {\"c\": 42}}}")
                     '(a b c)))))

(ert-deftest jeison:check-read-path-index ()
  (let ((json "{\"a\": [{\"b\": 42}, {\"b\": 11}, {\"b\": 100}]}"))
    (should (equal 42 (jeison--read-path
                       (jeison--read-json-string json)
                       '(a 0 b))))
    (should (equal 100 (jeison--read-path
                        (jeison--read-json-string json)
                        '(a -1 b))))))

(ert-deftest jeison:check-read-path-string ()
  (should (equal 42 (jeison--read-path
                     (jeison--read-json-string "{\"a\": {\"b\": {\"c\": 42}}}")
                     (list "a" "b" "c")))))

(ert-deftest jeison:check-read-basic ()
  (jeison-defclass jeison:jeison-class nil ((x :initarg :x :path (a b))
                                            (y :initarg :y :path c)))
  (let ((parsed (jeison-read jeison:jeison-class
                             "{\"a\": {\"b\": 42}, \"c\": 36.6}")))
    (should (equal (oref parsed x) 42))))

(ert-deftest jeison:check-read-nested ()
  (jeison-defclass jeison:jeison-class-a nil
    ((i :initarg :i)
     (j :initarg :j)))
  (jeison-defclass jeison:jeison-class-b nil
    ((x :initarg :x :path (a b))
     (y :initarg :y :type jeison:jeison-class-a :path c)))
  (let* ((parsed (jeison-read
                  jeison:jeison-class-b
                  "{\"a\": {\"b\": 42}, \"c\": {\"i\": 1, \"j\": 2}}"))
         (parsed-nested (oref parsed y)))
    (should (equal (oref parsed x) 42))
    (should (equal (oref parsed-nested i) 1))
    (should (equal (oref parsed-nested j) 2))))

(ert-deftest jeison:check-read-lists ()
  (jeison-defclass jeison:jeison-class-a nil ((x :initarg :x)))
  (jeison-defclass jeison:jeison-class-b nil
    ((a :initarg :a :type (list-of jeison:jeison-class-a))
     (b :initarg :b :type (list-of string))))
  (let* ((parsed (jeison-read
                  jeison:jeison-class-b
                  "{
                  \"a\": [{\"x\": 1}, {\"x\": 15}, {\"x\": 30}],
                  \"b\": [\"hello\", \"jeison\", \"enthusiasts\"]
                  }")))
    (should (equal '(1 15 30)
                   (mapcar (lambda (element) (oref element x)) (oref parsed a))))
    (should (equal '("hello" "jeison" "enthusiasts")
                   (oref parsed b)))))

(ert-deftest jeison:check-read-vector ()
  (jeison-defclass jeison:jeison-class-a nil ((x :initarg :x)))
  (jeison-defclass jeison:jeison-class-b nil
    ((a :initarg :a :type (jeison-vector-of jeison:jeison-class-a))
     (b :initarg :b :type (jeison-vector-of string))))
  (let* ((parsed (jeison-read
                  jeison:jeison-class-b
                  "{
                  \"a\": [{\"x\": 1}, {\"x\": 15}, {\"x\": 30}],
                  \"b\": [\"hello\", \"jeison\", \"enthusiasts\"]
                  }")))
    (should (equal [1 15 30]
                   (apply #'vector (mapcar (lambda (element) (oref element x)) (oref parsed a)))))
    (should (equal ["hello" "jeison" "enthusiasts"]
                   (oref parsed b)))))

(ert-deftest jeison:check-read-wrong-type ()
  (jeison-defclass jeison:jeison-class nil ((x :initarg :x :type string)))
  (condition-case nil
      (progn
        (jeison-read jeison:jeison-class "{\"x\": 1}")
        (ert-fail "Unexpected success"))
    (jeison-wrong-parsed-type nil)))

(ert-deftest jeison:check-read-wrong-list ()
  (jeison-defclass jeison:jeison-class nil
    ((x :initarg :x :type (list-of number))))
  (condition-case nil
      (progn
        (jeison-read jeison:jeison-class "{\"x\": 1}")
        (ert-fail "Unexpected success"))
    (jeison-wrong-parsed-type nil)))

(ert-deftest jeison:check-jeisonify ()
  (defclass jeison:usual-class nil ((x :initarg :x)
                                    (y :initarg :y)))
  (jeisonify jeison:usual-class)
  (let ((parsed (jeison-read
                 jeison:usual-class
                 "{\"x\": 1, \"y\": 2}")))
    (should (equal 1 (oref parsed x)))
    (should (equal 2 (oref parsed y)))
    (should (jeison-class-p jeison:usual-class))
    (should (jeison-object-p parsed))))

(ert-deftest jeison:check-jeisonify-wrong-type ()
  (condition-case nil
      (progn
        (jeisonify 'number)
        (ert-fail "Unexpected success"))
    (wrong-type-argument nil))
  (jeison-defclass jeison:jeison-class nil
    ((x :initarg :x) (y :initarg :y)))
  (condition-case nil
      (progn
        (jeisonify jeison:jeison-class)
        (ert-fail "Unexpected success"))
    (error nil)))

(ert-deftest jeison:check-read-no-initarg ()
  (jeison-defclass jeison:jeison-class nil ((x :path (a b))
                                            (y :initarg :y :path c)))
  (let ((parsed (jeison-read jeison:jeison-class
                             "{\"a\": {\"b\": 42}, \"c\": 36.6}")))
    (should (equal (oref parsed x) 42))
    (should (equal (oref parsed y) 36.6))))

(ert-deftest jeison:check-read-function-simple ()
  (jeison-defclass jeison:jeison-class nil
    ((x :initarg :x :type number :path ((string-to-number (a b c))))))
  (should (equal
           42
           (oref (jeison-read jeison:jeison-class
                              "{\"a\": {\"b\": {\"c\": \"42\"}}}")
                 x))))

(ert-deftest jeison:check-read-function-two-arguments ()
  (jeison-defclass jeison:jeison-class nil
    ((full-name :path (name ((lambda (first-name last-name)
                               (format "%s %s" first-name last-name))
                             first last)))))
  (should (equal
           "John Johnson"
           (oref
            (jeison-read jeison:jeison-class
                         "{
                           \"name\": {
                             \"first\": \"John\",
                             \"last\": \"Johnson\"
                           }
                         }") full-name))))

(defun jeison:filter-candidates (candidates)
  (seq-find (lambda (x) (jeison-read 'boolean x 'awesome)) candidates))

(ert-deftest jeison:check-read-function-dynamic-choice ()
  (let ((jeison-false nil))
    (should (equal
             42
             (jeison-read 'integer "{
                                    \"a\": {
                                       \"b\": [
                                        {
                                          \"number\": 1,
                                          \"awesome\": false
                                        },
                                        {
                                          \"number\": 10,
                                          \"awesome\": false
                                        },
                                        {
                                          \"number\": 42,
                                          \"awesome\": true
                                        },
                                        {
                                          \"number\": 59,
                                          \"awesome\": false
                                        }
                                      ]
                                    }
                                  }"
                          '(a (jeison:filter-candidates b) number))))))

(ert-deftest jeison:check-cl-deftype-jeison-hash-table-of ()
  ;; NOTE: EIEIO checks the type constraint when it creates a new instance,
  ;; not when the code invokes 'defclass'.  Consequently, EIEIO emits the
  ;; 'invalid-slot-type' signal __only__ when the code invokes
  ;; 'make-instance'.
  ;;
  ;; This code exercises (exorcises?) the cl-deftype type constraint
  ;; for jeison-hash-table-of.
  (should (and (condition-case ()
                   (progn
                     (jeison-defclass jeison:treif-key-type-class ()
                       ((foo :initarg :foo :type (jeison-hash-table-of integer string))))
                     (make-instance jeison:treif-key-type-class :foo (make-hash-table))
                     ;; Should not make it here...
                     (ert-fail "jeison-defclass admits invalid hash table key type."))
                 (invalid-slot-type
                  ;; Success
                  t))

               ;; This succeeds:
               (jeison-defclass jeison:kosher-symbol-key-type-class ()
                 ((foo :initarg :foo :type (jeison-hash-table-of symbol string))))
               (make-instance jeison:kosher-symbol-key-type-class :foo (make-hash-table))

               ;; And so does this one:
               (jeison-defclass jeison:kosher-string-key-type-class ()
                       ((foo :initarg :foo :type (jeison-hash-table-of string string))))
               (make-instance jeison:kosher-string-key-type-class :foo (make-hash-table)))))

(ert-deftest jeison:check-hash-table-of-type ()
  (jeison-defclass jeison:jeison-htab-string nil
    ((reply :initarg :reply :type (jeison-hash-table-of string t))))
  (jeison-defclass jeison:jeison-htab-symbol nil
    ((reply :initarg :reply :type (jeison-hash-table-of symbol t))))
  (let ((answer-symbol (jeison-read jeison:jeison-htab-symbol
                                    "{ \"reply\": { \"client-1a4ff5b056e2b11\" : { \"foo\": 1, \"bar\": false } } }"))
        (answer-string (jeison-read jeison:jeison-htab-string
                                    "{ \"reply\": { \"client-1a4ff5b056e2b11\" : { \"foo\": 1, \"bar\": false } } }")))
    (should (and
             (with-slots ((client-table reply)) answer-symbol
               (let ((client-data (gethash 'client-1a4ff5b056e2b11 client-table)))
                 (and (not (null client-data))
                      (null (gethash 'non-existent client-table)))))
             (with-slots ((client-table reply)) answer-string
               (let ((client-data (gethash "client-1a4ff5b056e2b11" client-table)))
                 (and (not (null client-data))
                      (null (gethash "non-existent" client-table)))))))))

(ert-deftest jeison:check-hash-table-of-hash-table-of-type ()
  (jeison-defclass jeison:version-thing ()
    ((major :initarg :major :initform 0 :type integer)
     (minor :initarg :minor :initform 0 :type integer)))
  (jeison-defclass jeison:jeison-thing nil
    ((jsonFile :initarg :jsonFile :initform "invalid")
     (kind :initarg kind :initform "nothing")
     (version :initarg :version :type jeison:version-thing)))
  (jeison-defclass jeison:jeison-class nil
    ((reply :initarg :reply :type (jeison-hash-table-of symbol (jeison-hash-table-of symbol jeison:jeison-thing)))))
  (let ((answer (jeison-read jeison:jeison-class "{
        \"reply\" :
        {
            \"client-1aa66f801e65edda\" : {
                \"codemodel-v2\" : {
                    \"jsonFile\" : \"codemodel-v2-bd9606760e25788500f7.json\",
                    \"kind\" : \"codemodel\",
                    \"version\" : { \"major\" : 2,\"minor\" : 4 }
                }
            },
            \"client-1aa66f801e65beef\" : {
                \"codemodel-v2\" : {
                    \"jsonFile\" : \"codemodel-v2-bd9606760e292887f0f7.json\",
                    \"kind\" : \"codemodel\",
                    \"version\" : { \"major\" : 2,\"minor\" : 4 }
                }
            }
        }
}")))
    (should (with-slots ((client-table reply)) answer
              (let ((client-1 (gethash 'client-1aa66f801e65edda client-table))
                    (client-2 (gethash 'client-1aa66f801e65beef client-table)))
                (and (not (null client-1))
                     (not (null (gethash 'codemodel-v2 client-1)))
                     (not (null client-2))
                     (null (gethash 'non-existent client-table))))))))

(ert-deftest jeison:check-invalid-hash-key-spec ()
  ;; Ensure jeison-read's hash table key type checking doesn't admit something
  ;; other than string or symbol.
  (jeison-defclass jeison:integer-hash-key-class ()
    ((a :initarg :a :type (jeison-hash-table-of integer string))))
  (jeison-defclass jeison:vector-hash-key-class ()
    ((a :initarg :a :type (jeison-hash-table-of vector string))))
  (should (and
           (condition-case nil
              (progn
                (jeison-read jeison:integer-hash-key-class "{ \"key\": \"value\" }")
                (ert-fail "jeison-read admitted hash table with integer key."))
             (jeison-invalid-key-type t))
           (condition-case nil
              (progn
                (jeison-read jeison:vector-hash-key-class "{ \"key\": \"value\" }")
                (ert-fail "jeison-read admitted hash table with vector key."))
             (jeison-invalid-key-type t)))))

(ert-deftest jeison:wrong-hash-value-type ()
  ;; Exercise 'jeison-wrong-parsed-type' with hash tables.
  (jeison-defclass jeison:hash-value-entity ()
    ((a :initarg :a :initform "default" :type string)
     (b :initarg :b :initform 'undefined :type symbol)
     (c :initarg :c :initform [] :type (jeison-vector-of integer))))
  (let ((json-stream "{ \"key-1\": { \"a\" : \"a's value\", \"b\": \"b-symbol-value\", \"c\": [99, 100, 101] } }"))
    (should (and
             (jeison-read '(jeison-hash-table-of string jeison:hash-value-entity) json-stream)
             (jeison-read '(jeison-hash-table-of symbol jeison:hash-value-entity) json-stream)
             (condition-case ()
                 (jeison-read '(jeison-hash-table-of string (list-of integer)) json-stream)
               (jeison-wrong-parsed-type t))
             (condition-case ()
                 (jeison-read '(jeison-hash-table-of symbol (list-of integer)) json-stream)
               (jeison-wrong-parsed-type t))))))

(ert-deftest jeison:check-read-from-buffer ()
  (with-temp-buffer
    (insert "{
  \"a\":
  {
    \"b\":
    {
      \"c\": 42
    }
  }
}")
    (goto-char (point-min))
    (should (equal 42 (jeison-read t (current-buffer) '(a b c))))))

(ert-deftest jeison:check-optional-element ()
  (jeison-defclass jeison:version-opt-test-thing ()
    ((major :initarg :major :initform 0 :type integer)
     (minor :initarg :minor :initform nil :type (or null integer))
     (patch :initarg :patch :initform nil :type (or null integer))))
  (jeison-defclass jeison:jeison-opt-test-thing nil
    ((jsonFile :initarg :jsonFile :initform "invalid")
     (kind :initarg kind :initform "nothing")
     (version :initarg :version :type jeison:version-opt-test-thing)))
  (let ((answer (jeison-read jeison:jeison-opt-test-thing "{
  \"jsonFile\" : \"codemodel-v2-bd9606760e25788500f7.json\",
  \"kind\" : \"codemodel\",
  \"version\" :
  {
    \"major\" : 2,
    \"minor\" : 4
  }
}")))
    (should (with-slots (version) answer
              (and (not (null version))
                   (with-slots (minor patch) version
                     (and (not (null minor)) (null patch))))))))

;;; jeison-test.el ends here
