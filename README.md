# [Fluokitten](http://fluokitten.uncomplicate.org) - category theory in idiomatic Clojure
[![Build Status](https://secure.travis-ci.org/uncomplicate/fluokitten.png)](https://travis-ci.org/uncomplicate/fluokitten)

Fluokitten is a Clojure library that implements category theory concepts, such as functors, applicative functors, monads, monoids etc. in idiomatic Clojure.

## Project Goals

* Fit well into idiomatic Clojure - Clojure programmers should be able to use and understand Fluokitten like any regular Clojure library.
* Fit well into Haskell monadic types conventions - programmers should be able to reuse existing widespread monadic programming know-how and easily translate it to Clojure code.
* Be reasonably easy to learn - the code from the existing books, articles and tutorials for learning monadic programming, which is usually written in Haskell should be easily translatable to Clojure with Fluokitten.
* Offer good performance.

## Installation & Requirements

Add the following dependency to your `project.clj` file:

```clojure
[uncomplicate/fluokitten "0.2.0"]
```

Fluokitten artifacts are distributed through Clojars, so they will be downloaded by leiningen by default. If you are using other tools for dependency management, you can download Fluokitten from Clojars.org, or build it from the source by running `lein jar`.

Fluokitten requires at least Clojure 1.5, since it uses its reducers library.
Reducers use java fork/join, so you have to run it on Java 7+ jdk, or Java 6 with `jsr166y.jar` included in your project dependencies (see [Clojure's POM] (https://github.com/clojure/clojure/blob/master/pom.xml) for the dependency info).

## Usage

`use` or `require` `uncomplicate.fluokitten.core` and `uncomplicate.fluokitten.jvm` in your namespace, and call appropriate functions from the Fluokitten library.
```clojure
(ns example
  (:use [uncomplicate.fluokitten core jvm]))

(fmap inc [1 2 3])
```

There are a couple of typical ways you would want to use Fluokitten:
* Use the built-in integration that makes clojure core concepts (data structures, functions, etc.) categorical.
* Build your own implementations of the categorical protocols for your specific data types.

The place for detailed instructions is [Fluokitten Homepage](http://fluokitten.uncomplicate.org). I will try to build a comprehensive collection of links to various resources related to category theory in programming (usually related to monads and friends) as well as code examples and instructions on how to do the equivalent stuff in Clojure with Fluokitten.
I assume that the first step for most users would be to just use the stuff built in the library and learn about category theory and related programming concepts. Therefore, most tutorials and literature will be written with that in mind for now. Until specific tutorials on implementing fluokitten protocols appear, advanced users may find the documentation in the source code, tests, and the source code itself helpful (I really think it already is).

## Get Involved

I welcome anyone who is willing to contribute, no mather the level of experience. Here are some ways in which you can help:
* If you are a native English speaker, i would really appreciate if you can help with correcting the English on the Fluokitten site and in the  documentation.
* Contribute your example code (your own or the ports from Haskell tutorials) to be added to Fluokitten tests.
* Contribute articles and tutorials.
* Do code review of the Fluokitten code and suggest improvements.
* If you find bugs, report them via [Fluokitten issue tracker](https://github.com/uncomplicate/fluokitten/issues).

## Documentation & Examples

The project is documented in the following ways:
* [API documentation](http://fluokitten.uncomplicate.org/codox).
* Learning resources on the [Fluokitten Homepage](http://fluokitten.uncomplicate.org)
* Code examples that are parts of the test suite.

## Project Maturity

While the project is in the 0.X.Y version it is considered in development, so the priority is adding the new features and enhancing the existing code as much as possible, rather than backward compatibility.
Once it reaches version 1.0.0 it will be considered stable, and more consideration will be directed towards supporting backward compatibility.

## License

Copyright © 2013 Dragan Djuric

Distributed under the Eclipse Public License, the same as Clojure
