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
                     (json-read-from-string "{\"a\": {\"b\": {\"c\": 42}}}")
                     '(a b c)))))

(ert-deftest jeison:check-read-path-index ()
  (let ((json "{\"a\": [{\"b\": 42}, {\"b\": 11}, {\"b\": 100}]}"))
    (should (equal 42 (jeison--read-path
                       (json-read-from-string json)
                       '(a 0 b))))
    (should (equal 100 (jeison--read-path
                        (json-read-from-string json)
                        '(a -1 b))))))

(ert-deftest jeison:check-read-path-string ()
  (should (equal 42 (jeison--read-path
                     (json-read-from-string "{\"a\": {\"b\": {\"c\": 42}}}")
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
