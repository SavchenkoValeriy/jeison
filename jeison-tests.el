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
  (jeison-defclass jeison:jeison-class nil ((x :initarg :x :path (a b))
                                            (y :initarg :y)))
  (let* ((slots (jeison--get-slots jeison:jeison-class))
         (x (car slots))
         (y (cadr slots)))
    (should (eq 'x (car x)))
    (should (equal '(a b) (cdr x)))
    (should (eq 'y (car y)))
    (should (equal 'y (cdr y)))))
