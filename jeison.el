;;; jeison.el --- EIEIO JSON parser -*- lexical-binding: t; -*-

;; Copyright (c) 2019 Valeriy Savchenko (GNU/GPL Licence)

;; Authors: Valeriy Savchenko <sinmipt@gmail.com>
;; URL: http://github.com/SavchenkoValeriy/jeison
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: lisp json data-types

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

;; TODO

;;; Code:
(require 'cl-lib)
(require 'eieio)
(require 'json)

(defmacro jeison-defclass (name superclasses slots &rest options-and-doc)
  "TODO"
  (declare (doc-string 4))
  `(progn
     (defclass ,name ,superclasses ,slots :jeison t ,@options-and-doc)
     (jeison--set-paths ',name ',slots)))

(defun jeison--read-path (json path)
  "TODO"
  (pcase path
    (`(,head . ,tail) (jeison--read-path (assoc-default head json) tail))
    ('nil json)))

(defun jeison--set-path (cl-slot slot-description)
  "TODO"
  (let* ((slot-options (cdr slot-description))
         (path (or (plist-get slot-options :path)
                   (cl--slot-descriptor-name cl-slot)))
         (slot-props (cl--slot-descriptor-props cl-slot))
         (new-slot-props (append slot-props `((:path . ,path)))))
    (setf (cl--slot-descriptor-props cl-slot) new-slot-props)))

(defun jeison--set-paths (name slots)
  "TODO"
  (cl-mapcar #'jeison--set-path (jeison--class-slots name) slots))

(defun jeison-class-p (class-or-class-name)
  "TODO"
  (let ((class (jeison--find-class class-or-class-name)))
    (and (eieio--class-p class)
         (plist-get (eieio--class-options class) :jeison))))

(defun jeison-object-p (object)
  "TODO"
  (and (cl-typep object 'eieio-object)
       (jeison-class-p (eieio-object-class object))))

(defun jeison--find-class (class-or-class-name)
  "TODO"
  (if (symbolp class-or-class-name)
      (cl-find-class class-or-class-name)
    class-or-class-name))

(defun jeison--get-slots (class-or-class-name)
  "TODO"
  (mapcar #'jeison--get-slot (jeison--class-slots class-or-class-name)))

(defun jeison--get-slot (raw-slot)
  "TODO"
  (let ((props (cl--slot-descriptor-props raw-slot)))
    (cons (cl--slot-descriptor-name raw-slot)
          (assoc-default :path props))))

(defun jeison--class-slots (class-or-class-name)
  "TODO"
  (eieio--class-slots (jeison--find-class class-or-class-name)))

(provide 'jeison)
;;; jeison.el ends here
