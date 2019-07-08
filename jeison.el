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
     ;; create a usual EIEIO class and tag it as jeison class
     (defclass ,name ,superclasses ,slots :jeison t ,@options-and-doc)
     ;; inject :path property into the newly created class
     (jeison--set-paths ',name ',slots)))

(defun jeison-read (type alist-or-json &optional path)
  "TODO"
  (let* ((json (if (stringp alist-or-json)
                   (json-read-from-string alist-or-json)
                 alist-or-json)))
    (jeison--read-internal type json path)))

(defun jeison--read-internal (type json &optional path)
  "TODO"
  (let* ((json (jeison--read-path json path))
         (result
          (pcase type
            ((pred jeison-class-p) (jeison--read-class type json))
            (`(list-of ,element-type)
             (cl-check-type
              json sequence
              (format "\"%S\" was specified to be a sequence, but got \"%S\""
                      path json))
             (mapcar (lambda (element)
                       (jeison--read-internal element-type element)) json))
            (_ json))))
    ;; TODO: check that `RESULT' is of type `TYPE'
    ;; (cl-check-type result type) doesn't work as it uses type as symbol
    result))

(defun jeison--read-class (class json)
  "TODO"
  (let ((arguments (cl-mapcan (lambda (slot) (jeison--read-slot slot json))
                              (jeison--get-slots class))))
    (apply class arguments)))

(defun jeison--read-slot (slot json)
  "TODO"
  (list (oref slot initarg) (jeison--read-internal
                             (oref slot type) json (oref slot path))))

(defun jeison--read-path (json path)
  "TODO"
  (pcase path
    ;; unwrap one level of the path
    (`(,head . ,tail) (jeison--read-path (assoc-default head json) tail))
    ;; the path is empty - nothing left to do here
    ('nil json)
    ;; path is not a list - assume that it's a one-level path
    (_ (assoc-default path json))))

(defun jeison--set-path (cl-slot slot-description)
  "TODO"
  (let* ((slot-options (cdr slot-description))
         (path (or (plist-get slot-options :path)
                   (cl--slot-descriptor-name cl-slot)))
         (slot-props (cl--slot-descriptor-props cl-slot))
         (slot-props (append slot-props `((:path . ,path)))))
    (setf (cl--slot-descriptor-props cl-slot) slot-props)))

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

(defclass jeison--slot nil
  ((name :initarg :name
         :documentation "Name of the slot")
   (initarg :initarg :initarg
            :documentation "Constructor's argument of the slot")
   (path :initarg :path
         :documentation "JSON path to retrieve the slot")
   (type :initarg :type
         :documentation "Lisp type of the slot"))
  :documentation "TODO")

(defun jeison--get-slots (class-or-class-name)
  "TODO"
  (cl-mapcar #'jeison--get-slot (jeison--class-slots class-or-class-name)
             (jeison--initargs class-or-class-name)))

(defun jeison--get-slot (raw-slot initarg)
  "TODO"
  (let ((props (cl--slot-descriptor-props raw-slot)))
    (jeison--slot :name (cl--slot-descriptor-name raw-slot)
                  :initarg (car initarg)
                  :type (cl--slot-descriptor-type raw-slot)
                  :path (assoc-default :path props))))

(defun jeison--initargs (class-or-class-name)
  "TODO"
  (eieio--class-initarg-tuples (jeison--find-class class-or-class-name)))

(defun jeison--class-slots (class-or-class-name)
  "TODO"
  (eieio--class-slots (jeison--find-class class-or-class-name)))

(provide 'jeison)
;;; jeison.el ends here
