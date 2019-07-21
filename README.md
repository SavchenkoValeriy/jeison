# jeison [![Build Status](https://travis-ci.org/SavchenkoValeriy/jeison.svg?branch=master)](https://travis-ci.org/SavchenkoValeriy/jeison)[![MELPA](https://melpa.org/packages/jeison-badge.svg)](https://melpa.org/#/jeison)

*Jeison* is a library for transforming JSON objects (or *alist*s) into [EIEIO](https://www.gnu.org/software/emacs/manual/html_node/eieio/index.html "EIEIO manual") objects.

## Installation

It's available on [Melpa](https://melpa.org/):

<kbd>M-x package-install [RET] jeison [RET]</kbd>

## Using in a package

Add this to the header block at the top (see [Library-Headers](https://www.gnu.org/software/emacs/manual/html_node/elisp/Library-Headers.html#Library-Headers "Library-Headers")):

``` emacs-lisp
;; Package-Requires: ((jeison "1.0.0"))
```

Don't forget to add `require` command as well:

``` emacs-lisp
(require 'jeison)
```

Additionally, you might want to use [Cask](https://github.com/cask/cask) to manage package dependencies automatically.

## Main idea

Main idea behind *jeison* is to create an easier way of dealing with deep JSON objects that contain lots and lots of information. Converting those into your own internal structures (or simply working with them) might become a burden pretty fast. *Jeison* helps to avoid writing tons of boilerplate code and simply *declare* how to find your data inside of JSON objects.

In order to use *jeison* one must first define a class using `jeison-defclass`. It works absolutely like `defclass` from EIEIO, but allows you to add another property to the class slots: `:path`. Here is an example:

``` emacs-lisp
(jeison-defclass my-first-jeison-class nil
  ((name :initarg :name :path (personal name last))
   (job :initarg :job :path (job title))))
```

Defining class like this we tell *jeison* where it should fetch values for `name` and `job` slots. In *JavaScript* syntax, it would look similar to the following code:

``` javascript
name = json['personal']['name']['last'];
job = json['job']['title'];
```

The next step would be transforming actual JSON object into EIEIO object. This is done by function `jeison-read`. Let's assume that we have the following JSON object in the Emacs Lisp variable `json-person`:

``` json
// json-person
{
  "personal": {
    "address": { },
    "name": {
      "first": "John",
      "last": "Johnson"
    }
  },
  "job": {
    "company": "Good Inc.",
    "title": "CEO"
  },
  "skills": [ ]
}
```

Calling `jeison-read` will produce the following results:

``` emacs-lisp
(setq person (jeison-read my-first-jeison-class json-person))
(oref person name) ;; => "Johnson"
(oref person job)  ;; => "CEO"
```

`jeison-read` also accepts optional third argument that contains a path to sub-object that we should read. For example, we can use *jeison* just to get one value from JSON object:

``` emacs-lisp
(jeison-read 'string json-person '(personal name last)) ;; => "Johnson"
```

## Features

### Default paths

In many cases, classes that we use in code significantly resemble the structure of the source JSON object. This means that `:path` will have same value as the corresponding slot's name. In order to avoid this duplication, *jeison* allows to omit `:path` property and use slot's name as a default:

``` emacs-lisp
(jeison-defclass default-path-class nil
  ((first) (last) (full)))
```

``` json
// json-name
{
  "name": {
    "first": "John",
    "last": "Johnson",
    "full": "John Johnson"
  }
}
```

``` emacs-lisp
(setq name (jeison-read default-path-class json-name '(name)))
(oref name first) ;; => "John"
(oref name last)  ;; => "Johnson"
(oref name full)  ;; => "John Johnson"
```

### Type checks

*Jeison* checks that type that user wants to read from JSON object matches the one that was actually found:

``` emacs-lisp
(jeison-read 'string '((x . 1)) 'x) ;; => (jeison-wrong-parsed-type string 1)
```

### Nested objects

EIEIO allows annotating class slots with *types*. Besides checking the type of the found object, *Jeison* uses this information for constructing *nested objects*.

Let's consider the following example:

``` emacs-lisp
(jeison-defclass jeison-person nil
  ((name :initarg :name :path (personal name last))
   (job :initarg :job :type jeison-job)))

(jeison-defclass jeison-job nil
  ((company :initarg :company)
   (position :initarg :position :path title)
   (location :initarg :location :path (location city))))
```

In this example, `jeison-person` has a slot that has a type of another *jeison* class: `jeison-job`. As the result of this hierarchy, for the next JSON object:

``` json
// json-person
{
  "personal": {
    "address": { },
    "name": {
      "first": "John",
      "last": "Johnson"
    }
  },
  "job": {
    "company": "Good Inc.",
    "title": "CEO",
    "location": {
      "country": "Norway",
      "city": "Oslo"
    }
  },
  "skills": [ ]
}
```

We have these results:

``` emacs-lisp
(setq person (jeison-read jeison-person json-person))
(oref person name) ;; => "Johnson"
(setq persons-job (oref person job))
(oref persons-job company) ;; => "Good Inc."
(oref persons-job position) ;; => "CEO"
(oref persons-job location) ;; => "Oslo"
```

### Lists

Another JSON's basic data type is *array*. *Jeison* can deal with arrays in two ways:

* **ignore**: *jeison* can behave like it is a normal value and do nothing about it

``` emacs-lisp
(jeison-read
 t "{\"numbers\": [1, 2, 10, 40, 100]}" 'numbers) ;; => [1 2 10 40 100]
```

It is a vector by the given path and *jeison* can simply return it.

However, sometimes we might want to have a list or check that all elements have certain type.

* **type-specific processing**: *jeison* can process JSON arrays based on the expected type

``` emacs-lisp
(jeison-read '(list-of integer)
             "{\"numbers\": [1, 2, 10, 40, 100]}"
             'numbers) ;; => (1 2 10 40 100)
```

EIEIO defines a very handy type `list-of` that we can use for processing array elements and checking that they match the corresponding type.

This mechanism also allows *jeison* to parse **lists of nested objects**. Let's continue our "John Johnson" example and add skill processing:

``` emacs-lisp
(jeison-defclass jeison-person nil
  ((name :initarg :name :path (personal name last))
   (job :initarg :job :type jeison-job)
   (skills :initarg :skills :type (list-of jeison-skill))))

(jeison-defclass jeison-job nil
  ((company :initarg :company)
   (position :initarg :position :path title)
   (location :initarg :location :path (location city))))

(jeison-defclass jeison-skill nil
  ((name :initarg :name :type string)
   (level :initarg :level :type integer)))
```

For the following JSON object:

``` json
// json-person
{
  "personal": {
    "address": { },
    "name": {
      "first": "John",
      "last": "Johnson"
    }
  },
  "job": {
    "company": "Good Inc.",
    "title": "CEO",
    "location": {
      "country": "Norway",
      "city": "Oslo"
    }
  },
  "skills": [
    {
      "name": "Programming",
      "level": 9
    },
    {
      "name": "Design",
      "level": 4
    },
    {
      "name": "Communication",
      "level": 1
    }
  ]
}
```

*jeison* produces these results:

``` emacs-lisp
(setq person (jeison-read jeison-person json-person))
(oref person name) ;; => "Johnson"
(mapcar
 (lambda (skill)
   (format "%s: %i" (oref skill name) (oref skill level)))
 (oref person skills)) ;; => ("Programming: 9" "Design: 4" "Communication: 1")
```

### Indexed elements

Sometimes, though, it is not required to process the whole list. Sometimes we want just one element, especially in case of heterogeneous arrays. For this use case, *jeison* supports indices in its `:path` properties.

Here is an example:

``` json
// json-company
{
  "company": {
    "name": "Good Inc.",
    "CEOs": [
      {
        "name": {
          "first": "Gunnar",
          "last": "Olafson"
        },
        "founder": true
      },
      {
        "name": "TJ-B",
        "cool": true
      },
      {
        "name": {
          "first": "John",
          "last": "Johnson"
        }
      }
    ]
  }
}
```

``` emacs-lisp
(jeison-read 'boolean json-company '(company CEOs 0 founder)) ;; => t
```

Unlike arguments to `elt` function, indices can be *negative*. Negative indices have the same semantics as in **Python** language (enumeration from the end):

``` emacs-lisp
(jeison-read 'string json-company '(company CEOs -1 name last)) ;; => "Johnson"
(jeison-read 'boolean json-company '(company CEOs -2 cool)) ;; => t
```

### Turning regular classes into jeison classes

Additionally, *jeison* has a feature of transforming existing classes declared with `defclass` macro into *jeison* classes:

``` emacs-lisp
(defclass usual-class nil ((x) (y)))
(jeisonify usual-class)

(setq parsed (jeison-read usual-class "{\"x\": 10, \"y\": 20}"))
(oref parsed x) ;; => 10
(oref parsed y) ;; => 20
```

**NOTE 1** even if `defclass` included `:path` properties, *jeison* will still use default paths.

**NOTE 2** *jeison* hacks into the structure of EIEIO classes and their slots. If the modified class relies on the purity of slot properties or class options, don't use `jeisonify` functionality and create a new class instead.

## Development

*Jeison* uses [cask](https://github.com/cask/cask). After the installation of **cask**, install all dependencies and run tests:

```
$ cask install
$ cask exec ert-runner
```

## Contribute

All contributions are most welcome!

It might include any help: bug reports, questions on how to use it, feature suggestions, and documentation updates.

## License

[GPL-3.0](./LICENSE)
