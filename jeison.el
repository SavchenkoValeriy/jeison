;;; jeison.el --- A library for declarative JSON parsing -*- lexical-binding: t; -*-

;; Copyright (c) 2023 B. Scott Michel (GNU/GPL Licence)
;; Copyright (c) 2019 Valeriy Savchenko (GNU/GPL Licence)

;; Authors: Valeriy Savchenko <sinmipt@gmail.com>
;;          B. Scott Michel (scooter.phd@gmail.com)
;; URL: http://github.com/SavchenkoValeriy/jeison
;; Version: 1.1.0
;; Package-Requires: ((emacs "25.1") (dash "2.16.0"))
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

;; 'jeison' is a library that transforms a JSON stream represented
;; as an 'alist' or a naked 'alist' on its own into EIEIO/CLOS
;; objects. New features since 1.0.0:
;;
;; - A 'vector-of' slot type constraint.  This is analogous to
;;   'list-of'.
;;
;; - '(jeison-hash-table-of key-type element-type)' type constraint
;;   maps JSON object member names to an 'element-type'. See the "Hash
;;   Tables" documentation.
;;
;; - Uses the Emacs-native JSON parsing, if detected.  'jeison' will
;;   fall back to the 'json.el' E-lisp package if native JSON support
;;   is not available.
;;
;;   NOTE: 'jeison-false' is the meta-syntactic variable for JSON
;;   'false' values -- native JSON defaults to ':false' whereas
;;   'json.el' defaults to ':json-false'.  'jeison-false' is set to
;;   the respective default, depending on the parser (either ':false'
;;   or ':json-false'.) Use a let-binding around 'jeison-read' to
;;   change 'jeison-false' to your preferred value when necessary.
;;
;; - A '(or null type)' type constraint is available, primarily to
;;   accommodate optional values in the JSON input stream. You can use
;;   ':initform nil' to initialize the slot; if the slot is not 'nil'
;;   after reading the JSON stream, a value was present in the stream.
;;
;; - ':initform' is respected as the slot's default value if the input
;;   JSON stream provides no replacement value. Alternatively, you can
;;   use the '(or null type)' type constraint.


;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'dash)
(require 'eieio)

;; Not used in the code, but may be useful in the future...
(defvar jeison-debug-mode 0
  "'jeison' debugging level.  0 = off, 5 = highest.")

(defvar jeison-use-json-el nil
  "t -> Strongly prefer 'json.el' to native JSON parser.")

(if (and (fboundp 'json-available-p)
         (json-available-p)
         (not jeison-use-json-el))
    ;; Native JSON available
    (progn
      (defvar jeison-false :false
        "jeison: Native JSON parser: Value interpolated for 'false' in
JSON. Consider let-binding this around your call to `json-read'
instead of `setq'ing it.")
      (defun jeison--read-json-string (inp)
        (json-parse-string inp :object-type 'alist :false-object jeison-false))
      (defun jeison--read-json-buffer (buf)
        (save-excursion
          (with-current-buffer buf
            (goto-char (point-min))
            (json-parse-buffer :object-type 'alist :false-object jeison-false)))))

  ;; Nope. Go with the E-lisp JSON support
  (progn
    (require 'json)
    (defvar jeison-false :json-false
      "jeison: Native JSON parser: Value interpolated for 'false' in
JSON. Consider let-binding this around your call to `json-read'
instead of `setq'ing it.")
    (defun jeison--read-json-string (inp)
      ;; Ensure that json-read-from-string only produces assoc lists.
      (let ((json-object-type 'alist)
            (json-false jeison-false))
        (json-read-from-string inp)))
    (defun jeison--read-json-buffer (buf)
      (save-excursion
        (let ((json-object-type 'alist)
              (json-false jeison-false))
          (json-read-from-string (with-current-buffer buf
                                   (goto-char (point-min))
                                   (buffer-string))))))))

(defmacro jeison-defclass (name superclasses slots &rest options-and-doc)
  "Define NAME as a new class derived from SUPERCLASS with SLOTS.

This macro is fully compatible with `defclass' macro and accepts the same
arguments.  The only difference is additional `:path' tag for slots.

OPTIONS-AND-DOC is used as the class' options and base documentation.
SUPERCLASSES is a list of superclasses to inherit from, with SLOTS
being the slots residing in that class definition.  Supported tags are:

  :initform   - Initializing form.
  :initarg    - Tag used during initialization.
  :accessor   - Tag used to create a function to access this slot.
  :allocation - Specify where the value is stored.
                Defaults to `:instance', but could also be `:class'.
  :writer     - A function symbol which will `write' an object's slot.
  :reader     - A function symbol which will `read' an object.
  :type       - The type of data allowed in this slot (see `typep').
  :documentation
              - A string documenting use of this slot.
  :path       - A path in JSON that is used to find the value for this slot
                during the `jeison-read' parsing process.

The following are extensions on CLOS:
  :custom     - When customizing an object, the custom :type.  Public only.
  :label      - A text string label used for a slot when customizing.
  :group      - Name of a customization group this slot belongs in.
  :printer    - A function to call to print the value of a slot.
                See `eieio-override-prin1' as an example.

A class can also have optional options.  These options happen in place
of documentation (including a :documentation tag), in addition to
documentation, or not at all.  Supported options are:

  :documentation - The doc-string used for this class.

Options added to EIEIO:

  :allow-nil-initform - Non-nil to skip typechecking of null initforms.
  :custom-groups      - List of custom group names.  Organizes slots into
                        reasonable groups for customizations.
  :abstract           - Non-nil to prevent instances of this class.
                        If a string, use as an error string if someone does
                        try to make an instance.
  :method-invocation-order
                      - Control the method invocation order if there is
                        multiple inheritance.  Valid values are:
                         :breadth-first - The default.
                         :depth-first

Options in CLOS not supported in EIEIO:

  :metaclass - Class to use in place of `standard-class'
  :default-initargs - Initargs to use when initializing new objects of
                      this class.

Due to the way class options are set up, you can add any tags you wish,
and reference them using the function `class-option'."
  (declare (doc-string 4) (indent 2))
  `(progn
     ;; create a usual EIEIO class and tag it as jeison class
     (defclass ,name ,superclasses ,slots :jeison t ,@options-and-doc)
     ;; inject :path property into the newly created class
     (jeison--set-paths ',name ',slots)))

(defun jeisonify (class-name)
  "Make a usual EIEIO CLASS-NAME jeison compatible.

CLASS-NAME is a symbol name of the class, it must be EIEIO class.

We use this function to store additional information about slots
inside of them (inside of slots props), if this behavior doesn't fit
your needs, please consider declaring a separate class."
  ;; passing jeison class would break custom `:path'es set in it
  (or (not (jeison-class-p class-name))
      (error "Given type is already a jeison class"))
  ;; check that the given type is EIEIO class indeed
  (cl-check-type (eieio--class-object class-name) eieio--class)
  ;; tag it as jeison class
  (plist-put (eieio--class-options
              (jeison--find-class class-name))
             :jeison t)
  ;; and put :path property into its slots
  (mapc (lambda (slot-descriptor) (jeison--set-path slot-descriptor nil))
        (jeison--class-slots class-name)))

(defun jeison-read (type input-source &optional path)
  "Read TYPE from INPUT-SOURCE by the given PATH.

TYPE is a CL-defined type (should work with `cl-typep'), that
means also that t is a valid type.
INPUT-SOURCE is a `buffer' or `string' with raw JSON, or an `alist'
representing a JSON object where we want information to be parsed
from.
PATH is a `list' of keys we should consequently find in JSON and
proceed with a nested JSON further on."
  ;; read JSON from string into an alist and proceed
  (jeison--read-internal type
                         (cond ((bufferp input-source) (jeison--read-json-buffer input-source))
                               ((stringp input-source) (jeison--read-json-string input-source))
                               (t input-source))
                         path))

(define-error 'jeison-error "jeison generic error" 'error)
(define-error 'jeison-wrong-parsed-type "jeison encountered unexpected type"
  'jeison-error)
(define-error 'jeison-invalid-symbol "jeison invalid symbol converson"
  'jeison-error)

;; Extend eieio's (and cl-lib's) type checking to include 'vector-of',
;; analogous to 'list-of'.  This should really be in eieio-core.el
;; (adapted from that code. :-)
(cl-deftype jeison-vector-of (elem-type)
  `(and vectorp
        (satisfies ,(lambda (vec)
                      (cl-every (lambda (elem) (cl-typep elem elem-type))
                                vec)))))

(cl-deftype jeison-hash-table-of (key-type elem-type)
  `(and hash-table-p
        (satisfies ,(lambda (htab)
                      (let ((snickers (or (eq key-type 'string)
                                          (eq key-type 'symbol))))
                        (maphash (lambda (key value)
                                   (setq snickers (and snickers
                                                       (cl-typep key key-type)
                                                       (cl-typep value elem-type))))
                                 htab)
                        ;; ... because snickers really satisfies (sm). :-)
                        snickers)))))

(defun jeison--read-internal (type json &optional path)
  "Read TYPE from JSON by the given PATH.

TYPE is a CL-defined type (should work with `cl-typep'), that means
also that t is a valid type.
JSON is an `alist' representing a JSON object where we want information
to be parsed from.
PATH is a `list' of keys we should consequently find in JSON and
proceed with a nested JSON further on."
  (let* ((json-stream (jeison--read-path json path))
         (result
          (pcase type
            ;; type is a jeison class: read JSON as a class
            ((pred jeison-class-p) (jeison--read-class type json-stream))
            ;; type is a homogeneous list of some sort
            (`(list-of ,element-type) (jeison--read-array element-type json-stream))
            (`(jeison-vector-of ,element-type) (apply #'vector (jeison--read-array element-type json-stream)))
            (`(jeison-hash-table-of ,key-type ,element-type)
              (jeison--read-name-value key-type element-type json-stream))
            ;; Slot value can be 'nil' or something legit...
            ((or `(or null ,element-type)
                 `(or ,element-type null))
             (jeison--read-internal element-type json-stream))
            ;; Convert string to symbol (useful for JSON member strings as hash keys and
            ;; lists items used as hash keys.)
            (`symbol (cond ((stringp json-stream) (intern json-stream))
                           (t (signal 'jeison-invalid-symbol
                                      (format "expected string, got %s, stream: %s"
                                              (type-of json-stream)
                                              json-stream)))))
            ;; not a special case of parsing - return whatever we found
            (_ json-stream))))
    ;; check that the parsed value matches the expected type...
    (or (null result)
        (cl-typep result type)
        (signal 'jeison-wrong-parsed-type
                (list (format "expected %s, got %s, result: %s" type (type-of result) result))))
    result))

(defclass jeison--slot nil
  ((name :initarg :name
         :documentation "Name of the slot.")
   (initarg :initarg :initarg
            :initform nil
            :documentation "Constructor's argument of the slot.")
   (path :initarg :path
         :documentation "JSON path to retrieve the slot.")
   (type :initarg :type
         :documentation "Lisp type of the slot."))
  :documentation
  "A small utility class containing information about jeison class slots.")

(defun jeison--read-class (class-name json)
  "Read jeison CLASS-NAME from the given JSON.

CLASS-NAME is a symbol name of the class.
JSON is an `alist' representing a JSON object where we want information
to be parsed from."
  ;; iterate over the class' slots and read data from JSON to fill them
  (-let* ((slots (jeison--get-slots class-name))
          ((initarg-slots rest-slots) (--separate (oref it initarg) slots))
          (constructor-arguments
           (-mapcat (lambda (slot) (jeison--read-slot slot json)) initarg-slots))
          (setter-arguments
           (mapcar (lambda (slot) (jeison--read-slot slot json)) rest-slots))
          ;; construct the class with parsed arguments
          (result (apply class-name constructor-arguments)))
    ;; set values to slots without `:initarg's
    (jeison--set-slot-values result setter-arguments)
    result))

(defun jeison--set-slot-values (object list-of-arguments)
  "Set slots of the OBJECT using the given LIST-OF-ARGUMENTS.

LIST-OF-ARGUMENTS is a list of lists ((name-1 value-1) .. (name-n value-n)),
where name-i is a name of OBJECT's slot and value-i is its new value."
  (mapc (lambda (arguments)
          (-let (((name value) arguments))
            (eieio-oset object name value)))
        list-of-arguments))

(defun jeison--read-slot (slot json)
  "Return a cons of arguments to initialize the given SLOT in a constructor.

It returns a list of a form (:field field-value) to be later used as arguments
in the target class' constructor:
  (class :field1 field1-value :field2 field2-value ...).

SLOT is a jeison descriptor of an slot, i.e. `jeison--slot'.
JSON is an `alist' representing a JSON object where we want information
to be parsed from."
  (let ((slot-value (jeison--read-internal
                     (oref slot type) json (oref slot path))))
    (if (null slot-value)
        nil
      (list (or (oref slot initarg)
                (oref slot name))
            slot-value))))

(defun jeison--read-path (json path)
  "Return nested JSON object found by the given list of keys PATH.

JSON is an `alist' representing JSON object.
PATH is a `list' of keys we should consequently find in JSON and
proceed with a nested JSON further on."
  (pcase path
    ;; unwrap one level of the path
    (`(,head . ,tail) (jeison--read-path (jeison--read-element head json) tail))
    ;; the path is empty - nothing left to do here
    ('nil json)
    ;; path is not a list - assume that it's a one-level path
    (_ (jeison--read-element path json))))

(defun jeison--read-element (element json)
  "Read ELEMENT from the given JSON.

ELEMENT is either a name of the field (`symbol' or `string'), or an index
of the required element of the sequence.
JSON is an `alist' representing JSON object."
  (cl-typecase element
    ;; symbol -> it's a simple key
    (symbol (assoc-default element json))
    ;; string -> it's the same semantics as with the symbol case,
    ;; but keys in alist are symbols and trying to get values by plain
    ;; string won't do, so we convert string to symbol
    (string (jeison--read-element (intern element) json))
    ;; integer -> it looks like json should be a sequence
    ;; and we want just a single element of it
    ;; additionally, if it's negative treat it as an index from the end
    (integer (elt json (if (< element 0)
                           (+ (length json) element) element)))
    ;; list -> it is a function call with path elements as its arguments
    (list (jeison--apply-function element json))))

(defun jeison--read-array (type json)
  "Read an array of TYPE elements from the JSON stream, returning a
list of TYPE elements."
  (unless (cl-typep json 'sequence)
    (signal 'jeison-wrong-parsed-type
            (list (format "expected sequence, got %s" (type-of json)))))
  ;; Iterate over the JSON array, transforming each element into a
  ;; 'type' value.
  (mapcar (lambda (element) (jeison--read-internal type element)) json))

(defun jeison--read-name-value (key-type value-type json)
  "Read a sequence of name/value pairs (object members) from the
JSON stream, wherein the names have the type KEY-TYPE and the
values have the type VALUE-TYPE, returning a hash table."
  ;; Minor performance optimization: use 'eq' for symbol-type keys,
  ;; 'equal' for strings.
  (let ((htab (if (eq key-type 'symbol)
                  (make-hash-table :test 'eq)
                (make-hash-table :test 'equal))))
    (dolist (elt json)
      ;; Keep this 'let', in case you need to do debugging. :-)
      (let ((key (car elt))
            (val (jeison--read-internal value-type (cdr elt))))
        (puthash (if (eq key-type 'symbol) key (symbol-name key)) val htab)))
    htab))

(defun jeison--apply-function (function-and-args json)
  "Parse FUNCTION-AND-ARGS and apply it to JSON.

FUNCTION-AND-ARGS is a list of the following structure:
  (FUNCTION . (ARGUMENT-PATHS))
FUNCTION can be a function symbol or a lambda expression.
ARGUMENT-PATHS is a list of paths (that can also include functions)
telling jeison where it can find arguments for the FUNCTION."
  (-let* (((function-element . argument-paths) function-and-args)
          ;; find arguments for the function by the specified paths...
          (arguments (mapcar (lambda (path) (jeison--read-path json path))
                             argument-paths)))
    ;; ...and return whatever is the result of this function
    (apply function-element arguments)))

(defun jeison--set-paths (class-or-class-name slots)
  "Save path information from SLOTS into CLASS-OR-CLASS-NAME.

CLASS-OR-CLASS-NAME can be a symbol name of the class or class itself.
SLOTS is a list of slot descriptions given by the user to
`jeison-defclass' macro.

We use this function to store additional information about slots
inside of them."
  (cl-mapcar #'jeison--set-path
             (jeison--class-slots class-or-class-name) slots))

(defun jeison--set-path (cl-slot slot-description)
  "Save path information into the given SL-SLOT.

CL-SLOT is an object representing `cl--slot-descriptor' class.
SLOT-DESCRIPTION is a description of the slot given by the user to
`jeison-defclass' macro.

Jeison stores path information in the slot's props because props are
just a simple `alist' and we can sneak additional information in there."
  (let* ((slot-options (cdr slot-description))
         ;; path is either user-defined :path
         (path (or (plist-get slot-options :path)
                   ;; or just a name of the slot
                   (cl--slot-descriptor-name cl-slot)))
         ;; retrieve props from the slot
         (slot-props (cl--slot-descriptor-props cl-slot))
         ;; add extracted :path to the props
         (slot-props (append slot-props `((:path . ,path)))))
    ;; save new version of props back into the slot
    (setf (cl--slot-descriptor-props cl-slot) slot-props)))

(defun jeison-class-p (class-or-class-name)
  "Return t if CLASS-OR-CLASS-NAME is a jeison class.

CLASS-OR-CLASS-NAME can be a symbol name of the class or class itself."
  (let ((class (jeison--find-class class-or-class-name)))
    (and (eieio--class-p class)
         (plist-get (eieio--class-options class) :jeison))))

(defun jeison-object-p (object)
  "Return t if OBJECT represents jeison class."
  ;; check that it's EIEIO object first
  (and (cl-typep object 'eieio-object)
       ;; and only then check the class itself
       (jeison-class-p (eieio-object-class object))))

(defun jeison--get-slots (class-or-class-name)
  "Construct a list of `jeison--slot' for all slots from CLASS-OR-CLASS-NAME.

CLASS-OR-CLASS-NAME can be a symbol name of the class or class itself."
  (let* ((class (jeison--find-class class-or-class-name))
         ;; compose a list of `jeison--slot' objects
         (class-slots (mapcar #'jeison--get-slot (jeison--class-slots class)))
         ;; retrieve a list of initargs from EIEIO
         (initargs (jeison--initargs class)))
    ;; match slots and initargs
    (jeison--set-initargs class-slots initargs)
    ;; return the list of slots
    class-slots))

(defun jeison--get-slot (raw-slot)
  "Construct `jeison--slot' object from RAW-SLOT and INITARG.

RAW-SLOT is a `cl--slot-descriptor' object of a jeison class.
INITARG is `:initarg' of RAW-SLOT, EIEIO keeps them separately."
  (let ((props (cl--slot-descriptor-props raw-slot)))
    (jeison--slot :name (cl--slot-descriptor-name raw-slot)
                  :type (cl--slot-descriptor-type raw-slot)
                  :path (assoc-default :path props))))

(defun jeison--initargs (class-or-class-name)
  "Return list of `:initarg's associated with the given CLASS-OR-CLASS-NAME.

CLASS-OR-CLASS-NAME can be a symbol name of the class or class itself."
  (eieio--class-initarg-tuples (jeison--find-class class-or-class-name)))

(defun jeison--set-initargs (class-slots initargs)
  "Set initarg field for the given CLASS-SLOTS using INITARGS.

CLASS-SLOTS is a list of `jeison--slot' objects.
INITARGS is a list of cons (initarg . field-name) from EIEIO.

As INITARGS has cons only for the fields that do have initargs in the first
place, common zip of two lists won't work.  However, all the elements from
INITARGS are guaranteed to be in CLASS-SLOTS.  INITARGS and CLASS-SLOTS are
also guaranteed to have the same order."
  ;; if at least of these lists is empty, we have not much to do here
  (when (and class-slots initargs)
    (-let (((slot . rest-slots) class-slots)
           (((initarg . name) . rest-initargs) initargs))
      ;; check that current initarg is defined for the current slot
      (when (equal (oref slot name) name)
        ;; set initarg field for the slot
        (oset slot initarg initarg)
        ;; pop the top initarg from the list
        ;; as we matched it already
        (setq initargs rest-initargs))
      ;; match the rest of slots
      (jeison--set-initargs rest-slots initargs))))

(defun jeison--class-slots (class-or-class-name)
  "Return slot objects associated with the given CLASS-OR-CLASS-NAME.

CLASS-OR-CLASS-NAME can be a symbol name of the class or class itself."
  (eieio--class-slots (jeison--find-class class-or-class-name)))

(defun jeison--find-class (class-or-class-name)
  "Return class object for the given CLASS-OR-CLASS-NAME.

CLASS-OR-CLASS-NAME can be a symbol name of the class or class itself."
  (if (symbolp class-or-class-name)
      (cl-find-class class-or-class-name)
    class-or-class-name))

(provide 'jeison)

;;; jeison.el ends here
