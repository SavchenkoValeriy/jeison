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

(ert-deftest jeison:predicates ()
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

(ert-deftest jeison:check-read-basic ()
  (jeison-defclass jeison:jeison-class nil ((x :initarg :x :path (a b))
                                            (y :initarg :y :path c)))
  (let ((parsed (jeison-read jeison:jeison-class
                             "{\"a\": {\"b\": 42}, \"c\": 36.6}")))
    (should (equal (oref parsed x) 42))))
