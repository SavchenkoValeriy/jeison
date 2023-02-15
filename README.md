# jeison [![Build Status](https://travis-ci.org/SavchenkoValeriy/jeison.svg?branch=master)](https://travis-ci.org/SavchenkoValeriy/jeison)[![MELPA](https://melpa.org/packages/jeison-badge.svg)](https://melpa.org/#/jeison)

*Jeison* is a a JSON and *alist* transformer library that produces
[EIEIO][eieio] objects from a parsed JSON input stream.

## New in 1.1.0

Notable enhancements include:

- Use Emacs' native JSON parsing support, falling back to `json.el` if native
  isn't availble.

- `jeison-false` is the meta-variable bridge for JSON `false` values. If using
  native JSON, it's value will be `:false`. For `json.el`, it's value will be
  `:json-false`.

- New type constraints: `jeison-vector-of`, `jeison-hash-table-of` and `(or null
  type)`

  - `:type (jeison-vector-of type)` supports vectors of things. This is an
    alternative to `list-of` things in a `:type` constraint.

  - `:type (jeison-hash-table-of key-type value-type)` supports hash
    tables, where the values are `value-type`. The `key-type` can be
    `string` or `symbol`.

  - `:type (or null type)` and `:type (or type null)` allows you to initilize
    a slot to `nil`. `jeison-read` can parse and subsequently set the slot to
    `type`.

    The primary use for `:type (or null type)` is handling optional
    JSON elements and you want to use `nil` to determine if the
    element was present in the JSON stream. For example:

      ``` emacs-lisp
      (jeison-defclass something ()
        ((mandatory-elt :initarg :mandataory
                        :initform -1
                        :type integer)
         (optional-elt  :initarg :optional
                        :initform nil
                        :type (or null string))))
      ```

    In your code:

      ``` emacs-lisp
      (let ((thing (jeison-read something json-input)))
        (with-slots (optional-elt) thing
          (when optional-slot
            ;; do something with optional-slot
          )))
      ```

    `(or null type)` is also useful with the `jeison-hash-table-of`
    constraint, because you can't properly initialize a slot by
    calling a function (`defclass` quotes `:initform` arguments.) You
    either need to write an `initialize-instance :before` generic
    method for your class and initialize the slot there, or initialize
    the slot to `nil` and use this constraint. Alternatively, you can
    leave out `:initform` arguments for hash tables, if the hash table
    isn't optional in the JSON input stream.

## Installation

It's available on [Melpa](https://melpa.org/):

<kbd>M-x package-install [RET] jeison [RET]</kbd>

## Using in a package

Add this to the header block at the top (see [Library-Headers][library-headers]):

``` emacs-lisp
;; Package-Requires: ((jeison "1.0.0"))
```

Don't forget to add `require` command as well:

``` emacs-lisp
(require 'jeison)
```

Additionally, you might want to use [Cask][cask] to manage package
dependencies automatically.

## Main idea

Main idea behind *jeison* is to create an easier way of dealing with deep JSON
objects that contain lots and lots of information. Converting those into your
own internal structures (or simply working with them) might become a burden
pretty fast. *Jeison* helps to avoid writing tons of boilerplate code and
simply *declare* how to find your data inside of JSON objects.

In order to use *jeison*, define a class using `jeison-defclass`. It works
absolutely like `defclass` from EIEIO, but allows you to add another property
to the class slots: `:path`. Here is an example:

``` emacs-lisp
(jeison-defclass my-first-jeison-class nil
  ((name :initarg :name :path (personal name last))
   (job :initarg :job :path (job title))))
```

(Note: You can try these examples in *Interactive Emacs Lisp* mode: <kbd>M-x ielm</kbd>)

Defining class like this we tell *jeison* where it should fetch values for
`name` and `job` slots. In *JavaScript* syntax, it would look similar to the
following code:

``` javascript
name = json['personal']['name']['last'];
job = json['job']['title'];
```

The next step is transforming actual JSON object into EIEIO object. This is
done by function `jeison-read`. Let's assume that we have the following JSON
object in the Emacs Lisp variable `json-person`:

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

`jeison-read` also accepts optional third argument that contains a path to
sub-object that we should read. For example, we can use *jeison* just to get
one value from JSON object:

``` emacs-lisp
(jeison-read 'string json-person '(personal name last)) ;; => "Johnson"
```

## Features

### Default paths

In many cases, classes that we use in code significantly resemble the
structure of the source JSON object. This means that `:path` will have same
value as the corresponding slot's name. In order to avoid this duplication,
*jeison* allows to omit `:path` property and use slot's name as a default:

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

*Jeison* checks that type that user wants to read from JSON object matches the
one that was actually found:

``` emacs-lisp
(jeison-read 'string '((x . 1)) 'x) ;; => (jeison-wrong-parsed-type string 1)
```

### Nested objects

EIEIO allows annotating class slots with *types* as a type constraint on the
slot. Besides checking the type of the found object, *Jeison* uses this
information for constructing *nested objects*.

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

In this example, `jeison-person` has a slot that has a type of another
*jeison* class: `jeison-job`. As the result of this hierarchy, for the next
JSON object:

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

We get these results:

``` emacs-lisp
(setq person (jeison-read jeison-person json-person))
(oref person name) ;; => "Johnson"
(setq persons-job (oref person job))
(oref persons-job company) ;; => "Good Inc."
(oref persons-job position) ;; => "CEO"
(oref persons-job location) ;; => "Oslo"
```

### Lists

Another JSON's basic data type is *array*. *Jeison* can deal with arrays in
two ways:

* Return the value created by the JSON parser, do no conversions, Just leave
  it alone.

``` emacs-lisp
(jeison-read t "{\"numbers\": [1, 2, 10, 40, 100]}" 'numbers)
;; => [1 2 10 40 100]
```

The default JSON array type for both the native JSON parser and `json.el` is
*vector*. Unsurprisingly, this example returns an integer vector.

The `t` argument to `jeison-read` permits `jeison-read` to read any legitimate
type from the JSON stream, so you can mix integers, strings and symbols in the
array:

``` emacs-lisp
(jeison-read t "{\"numbers\": [1, \"foo!\", 10.0, 45, 100]}" 'numbers)
;; => [1 "foo!" 10.0 45 100]
```

* **type-specific processing**: Use a type constraint so that *jeison*
  produces JSON arrays of the expected type:

``` emacs-lisp
(jeison-read '(list-of integer) "{\"numbers\": [1, 2, 10, 40, 100]}" 'numbers)
;; => (1 2 10 40 100)
(jeison-read '(jeison-vector-of integer) "{\"numbers\": [1, 2, 10, 40, 100]}" 'numbers)
;; => [1 2 10 40 100]
```

EIEIO defines a very handy type constraint, `list-of`, that instructs
*jeison-read* to convert the JSON array to a list, and causes EIEIO to check
the returned value's type to ensure it is a list of integers. Similarly,
*jeison* defines a `jeison-vector-of` type that instructs *jeison-read* to
convert the JSON array to an integer vector and causes EIEIO to ensure that
the returned value is, in fact, an integer vector.

This mechanism also allows *jeison* to parse **lists of nested
objects**. Let's continue our "John Johnson" example and add skill
processing:

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

Sometimes, though, it is not required to process the whole list. Sometimes we
want just one element, especially in case of heterogeneous arrays. For this
use case, *jeison* supports indices in its `:path` properties.

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

Unlike arguments to `elt` function, indices can be *negative*. Negative
indices have the same semantics as in **Python** language (enumeration from
the end):

``` emacs-lisp
(jeison-read 'string json-company '(company CEOs -1 name last)) ;; => "Johnson"
(jeison-read 'boolean json-company '(company CEOs -2 cool)) ;; => t
```

### Hash Tables

There are times when the JSON stream's path doesn't directly
correspond to lists or EIEIO objects, or the objects are indirectly
referenced by another object's member name. The `jeison-hash-table-of`
type constraint addresses these use cases.

Let's modify the "Skills" example as follows (note: you can mark,
<kbd>M-w</kbd> copy, yank and execute the code in *Interactive Emacs
Lisp Mode* (<kbd>M-x ielm</kbd>)):

``` emacs-lisp
(jeison-defclass jeison-people nil
  ;; Change 1: We have more than one person:
  ((people :initarg :people
           :initform ()
           :type (list-of jeison-person))
   ;; Change 2: Skills are indirectly referenced via the
   ;; skills hash table. Note that the hash table's key
   ;; type is 'symbol'.
   (skills :initarg :skills
           :type (jeison-hash-table-of symbol jeison-skill))))
```
``` emacs-lisp
(jeison-defclass jeison-person nil
  ((name :initarg :name :path (personal name last))
   (job :initarg :job :type jeison-job)
   ;; Change 3: skills are a list of skill references
   ;; into the skills hash table. Note that the references
   ;; themselves are a list of symbols, not strings:
   (skill-refs :initarg :skill-refs
               :initform ()
               :type (list-of symbol))))
```
``` emacs-lisp
(jeison-defclass jeison-job nil
  ((company :initarg :company)
   (position :initarg :position :path title)
   (location :initarg :location :path (location city))))
```
``` emacs-lisp
(jeison-defclass jeison-skill nil
  ((name :initarg :name :type string)
   (description :initarg :level :type string)))
```

Note 1: The `:initform` value in a `jeison-hash-table-of`-typed slot
declaration is not particularly important because *jeison* intializes
and sets the object's slot values before EIEIO checks the newly
created object's type constraints. **Do not** use `nil` as the
initializer because this will cause an invalid type failure when
`jeison-defclass` defines the class and EIEIO type checks the
`:initform` value.

Note 2: This part of the example uses symbols as the hash key
type. You can also use strings, which is discussed further on. The
`"skill-1"`, `"skill-2"` and `"skill-3"` object member strings in the
`skills` JSON object are converted to intern-ed symbols, i.e.,
`'skill-1`, `'skill-2` and `'skill-3`. This means that you need to use
`list-of symbol` to ensure that the skill reference strings in the
`skill-refs` arrays are also converted into symbols to avoid a type
mismatch when looking up elements with `gethash`.

Using the following JSON object as an Emacs Lisp string:

``` emacs-lisp
(setq json-people "{
  \"people\": [
    {
      \"personal\": {
        \"address\": { },
        \"name\": {
          \"first\": \"John\",
          \"last\": \"Johnson\"
        }
      },
      \"job\": {
        \"company\": \"Good Inc.\",
        \"title\": \"CEO\",
        \"location\": {
          \"country\": \"Norway\",
          \"city\": \"Oslo\"
        }
      },
      \"skill-refs\": [
        \"skill-1\",
        \"skill-3\"
      ]
    },
    {
      \"personal\": {
        \"address\": { },
        \"name\": {
          \"first\": \"Sammy\",
          \"last\": \"Smith\"
        }
      },
      \"job\": {
        \"company\": \"Good Inc.\",
        \"title\": \"VP, Marketing\",
        \"location\": {
          \"country\": \"USA\",
          \"city\": \"Peoria\"
        }
      },
      \"skill-refs\": [
        \"skill-2\",
        \"skill-3\"
      ]
    }
  ],
  \"skills\": {
    \"skill-1\": {
      \"name\": \"Programming\",
      \"description\": \"Can code like Richard S or Linus T\"
    },
    \"skill-2\": {
      \"name\": \"Design\",
      \"description\": \"Can think out loud, structure code systems\"
    },
    \"skill-3\": {
      \"name\": \"Communication\",
      \"description\": \"Can explain complicated things to social scientists\"
    }
  }
}")
```

The code below demonstrates how to use the `skills` hash table and
symbols as keys:

```emacs-lisp
(let* ((people-data (jeison-read jeison-people json-people)))
    ;; Do useful things with the data (in this case, just print):
    (mapcar (lambda (person)
              (princ (format "%s skills: %s\n"
                             (oref person name)
                             (mapconcat (lambda (the-skill-ref)
                                          (let ((the-skill (gethash the-skill-ref
                                                                    (oref people-data skills))))
                                            (oref the-skill name)))
                                        (oref person skill-refs)
                                        ", "))))
            (oref people-data people))
    (pp (oref people-data skills)))
```

The output in the *ielm* buffer should be:

```
Johnson skills: Programming, Communication
Smith skills: Design, Communication
#s(hash-table size 65 test eq rehash-size 1.5 rehash-threshold 0.8125 data
              (skill-1 #s(jeison-skill "Programming" "Can code like Richard S or Linus T")
                       skill-2 #s(jeison-skill "Design" "Can think out loud, structure code systems")
                       skill-3 #s(jeison-skill "Communication" "Can explain complicated things to social scientists")))
```

The contents of the `skills` hash table are pretty-printed to show the
keys are symbols.

`jeison-hash-table-of` also accepts `string` for the hash table's key
type. Simply change the definitions of `jeison-people` and
`jeison-person` to use `string` instead of `symbol`:

``` emacs-lisp
(jeison-defclass jeison-people nil
  ;; Change 1: We have more than one person:
  ((people :initarg :people
           :initform ()
           :type (list-of jeison-person))
   ;; Change 2: Skills are indirectly referenced via the
   ;; skills hash table. Note that the hash table's key
   ;; type is now 'string'.
   (skills :initarg :skills
           :type (jeison-hash-table-of string jeison-skill))))
```
``` emacs-lisp
(jeison-defclass jeison-person nil
  ((name :initarg :name :path (personal name last))
   (job :initarg :job :type jeison-job)
   ;; Change 3: skills are a list of skill references
   ;; to the skills hash table. Note that the references
   ;; are a list of strings:
   (skill-refs :initarg :skill-refs
               :initform ()
               :type (list-of string))))
```

You don't need to change the demonstration code -- re-executing the
demonstration code after changing the class definitions produces the
same output:

```emacs-lisp
;; Same code as above... just repeated here...

(let* ((people-data (jeison-read jeison-people json-people)))
    ;; Do useful things with the data (in this case, just print):
    (mapcar (lambda (person)
              (princ (format "%s skills: %s\n"
                             (oref person name)
                             (mapconcat (lambda (the-skill-ref)
                                          (let ((the-skill (gethash the-skill-ref
                                                                    (oref people-data skills))))
                                            (oref the-skill name)))
                                        (oref person skill-refs)
                                        ", "))))
            (oref people-data people))
    (pp (oref people-data skills)))
```

And the output -- the `skills` hash table keys are now strings:

```
Johnson skills: Programming, Communication
Smith skills: Design, Communication
#s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data
              ("skill-1" #s(jeison-skill "Programming" "Can code like Richard S or Linus T")
                         "skill-2" #s(jeison-skill "Design" "Can think out loud, structure code systems")
                         "skill-3" #s(jeison-skill "Communication" "Can explain complicated things to social scientists")))
```

Using `symbol` or `string` is a matter of personal preference. There
is a slight performance advantage to using symbols because the hash
table uses `eq` for comparisons, vice `equal` for strings. However, if
the hash table has a short lifetime, the performance advantage is
likely minimal.

### `(or null type)`: Optional JSON elements

The `(or null type)` type constraint helps detect when optional JSON
elements are present in an input stream. This allows you to specify
`nil` as the slot's `:initform` in addition to its expected type if it
receives a value from `jeison-read`. If the slot's value isn't
initialized to a non-`nil` value by `jieson-read`, the `nil` `:initform`
value is preserved.

Let's modify `jeison-person` so that the `skill-refs` skill references
are optional:

``` emacs-lisp
(jeison-defclass jeison-person nil
  ((name :initarg :name :path (personal name last))
   (job :initarg :job :type jeison-job)
   ;; skill-refs are optional:
   (skill-refs :initarg :skill-refs
               :initform nil
               :type (or null (list-of string)))))
```

Then, we add "Tommy Twiddler", who has no skills, to the `json-people`
JSON object:

``` emacs-lisp
(setq json-people "{
  \"people\": [
    {
      \"personal\": {
        \"address\": { },
        \"name\": {
          \"first\": \"John\",
          \"last\": \"Johnson\"
        }
      },
      \"job\": {
        \"company\": \"Good Inc.\",
        \"title\": \"CEO\",
        \"location\": {
          \"country\": \"Norway\",
          \"city\": \"Oslo\"
        }
      },
      \"skill-refs\": [
        \"skill-1\",
        \"skill-3\" 
      ]
    },
    {
      \"personal\": {
        \"address\": { },
        \"name\": {
          \"first\": \"Sammy\",
          \"last\": \"Smith\"
        }
      },
      \"job\": {
        \"company\": \"Good Inc.\",
        \"title\": \"VP, Marketing\",
        \"location\": {
          \"country\": \"USA\",
          \"city\": \"Peoria\"
        }
      },
      \"skill-refs\": [
        \"skill-2\",
        \"skill-3\" 
      ]
    },
    {
      \"personal\": {
        \"address\": { },
        \"name\": {
          \"first\": \"Tommy\",
          \"last\": \"Twiddler\"
        }
      },
      \"job\": {
        \"company\": \"Good Inc.\",
        \"title\": \"Mail Distribution\",
        \"location\": {
          \"country\": \"USA\",
          \"city\": \"Peoria\"
        }
      }
    }
  ],
  \"skills\": {
    \"skill-1\": {
      \"name\": \"Programming\",
      \"description\": \"Can code like Richard S or Linus T\"
    },
    \"skill-2\": {
      \"name\": \"Design\",
      \"description\": \"Can think out loud, structure code systems\"
    },
    \"skill-3\": {
      \"name\": \"Communication\",
      \"description\": \"Can explain complicated things to social scientists\"
    }
  }
}")
```

When you execute the code below:

``` emacs-lisp
(let* ((people-data (jeison-read jeison-people json-people)))
  (mapcar (lambda (person)
            (princ (format "%s skills: %s\n"
                           (oref person name)
                           (if (oref person skill-refs)
                               ; The person has skill references
                               (mapconcat (lambda (the-skill-ref)
                                            (let ((the-skill (gethash (intern the-skill-ref)
                                                                      (oref people-data skills))))
                                              (oref the-skill name)))
                                          (oref person skill-refs)
                                          ", ")
                               ; No skill references for this person...
                               "Missing or unknown skills"))))
          (oref people-data people)))
```

You should see this output in *ielm*:

```
Johnson skills: Programming, Communication
Smith skills: Design, Communication
Twiddler skills: Missing or unknown skills
```

### Turning regular classes into jeison classes

Additionally, *jeison* has a feature of transforming existing classes declared
with `defclass` macro into *jeison* classes:

``` emacs-lisp
(defclass usual-class nil ((x) (y)))
(jeisonify usual-class)

(setq parsed (jeison-read usual-class "{\"x\": 10, \"y\": 20}"))
(oref parsed x) ;; => 10
(oref parsed y) ;; => 20
```

**NOTE 1** even if `defclass` included `:path` properties, *jeison*
will still use default paths.

**NOTE 2** *jeison* hacks into the structure of EIEIO classes and
their slots. If the modified class relies on the purity of slot
properties or class options, don't use `jeisonify` functionality and
create a new class instead.

### Functional elements of the path

Often the data in JSON obect is not exactly what we need, sometimes we
need to change it before making it usable by our application. In this
situation, simply reading it into a class' slot won't be enough. For
this (and some other) use cases, *jeison* provides a feature of
*functional elements*.

*Functional elements* might have a bit of an obscure syntax, so it
might be better to see how it works within real examples.

Let's say we have the following JSON object:

``` json
// json-movie
{
  "movie": {
    "title": "The Shawshank Redemption",
    "rating": "10.0"
  }
}
```

*Jeison* class:

``` emacs-lisp
(jeison-defclass jeison-movie nil
  ((title :initarg :title :type string)
   (rating :initarg :rating :type string :path (info rating))))
```

would do, but *rating* is a string, which is not a very good type if
we want to compare different movies. In order to make this class more
user-friendly, *functional element* can be used as follows:

``` emacs-lisp
(jeison-defclass jeison-movie nil
  ((title :initarg :title :type string)
   (rating :initarg :rating :type number
           :path (info (string-to-number rating)))))
```

Parsing this new version of `jeison-movie` class from JSON will
produce the following results:

``` emacs-lisp
(setq movie (jeison-read jeison-movie json-movie 'movie))
(oref movie title) ;; => "The Shawshank Redemption"
(oref movie rating) ;; => 10.0
(> (oref movie rating) 9.8) ;; => t
```

*Functional element* `(string-to-number rating)` tells *jeison* that
it should fetch whatever data is located by the path `rating`, call
`string-to-number` function with this data as an argument and return
that value.

In the most generic case path including a *functional element* might
look like this:

``` emacs-lisp
(a (f b1 b2 b3) c)
```

Describing what is going on with this path is easier with a similar
JavaScript code:

``` javascript
// obj is the target JSON object
f(obj['a']['b1'], obj['a']['b2'], obj['a']['b3'])['c']
```

As you can see from this example, *functional elements* can have more
than one argument. Let's consider the following modification of one of
our previous examples:

``` json
// json-name
{
  "name": {
    "first": "John",
    "last": "Johnson",
  }
}
```

``` emacs-lisp
(defun get-full-name (first last)
  (format "%s %s" first last))

(jeison-read 'string json-name
             '(name (get-full-name first last))) ;; => "John Johnson"
```

The same solution could've been implemented a bit differently:

``` emacs-lisp
(jeison-read 'string json-name
             '((get-full-name (name first) (name last)))) ;; => "John Johnson"
```

## Development

*Jeison* uses [cask](https://github.com/cask/cask). After the
installation of **cask**, install all dependencies and run tests:

```
$ cask install
$ cask exec ert-runner
```

## Contribute

All contributions are most welcome!

It might include any help: bug reports, questions on how to use it,
feature suggestions, and documentation updates.

## License

[GPL-3.0](./LICENSE)


[eieio]: https://www.gnu.org/software/emacs/manual/html_node/eieio/index.html "EIEIO manual"
[library-headers]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Library-Headers.html#Library-Headers "Library-Headers"
[cask]: https://github.com/cask/cask
[cmake]: https://cmake.org/
