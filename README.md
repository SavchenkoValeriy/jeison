# jeison [![Build Status](https://travis-ci.org/SavchenkoValeriy/jeison.svg?branch=master)](https://travis-ci.org/SavchenkoValeriy/jeison)

*Jeison* is a library for transforming JSON objects (or *alist*s) into [EIEIO](https://www.gnu.org/software/emacs/manual/html_node/eieio/index.html "EIEIO manual") objects.

## Installation

TODO: add to MELPA

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
                 ((company :initarg company)
                  (position :initarg position :path title)
                  (location :initarg location :path (location city))))
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

### Indexed elements

### Turning regular classes into jeison classes

## Contribute
