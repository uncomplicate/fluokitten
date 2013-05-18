# Redcat, category theory concepts in idiomatic Clojure
[![Build Status](https://secure.travis-ci.org/uncomplicate/redcat.png)](https://travis-ci.org/uncomplicate/redcat)

Redcat is a Clojure library that implements category theory concepts, such as functors, applicative functors, monads, monoids etc. in idiomatic Clojure.

## Project Goals

* Fit well into idiomatic Clojure - Clojure programmers should be able to use and understand Redcat like any regular Clojure library.
* Fit well into Haskell monadic types conventions - programmers should be able to reuse existing widespread monadic programming literature and easily translate it to Clojure code.
* Be reasonably easy to learn - the code from the existing books, articles and tutorials for learning monadic programmng, which is usually written in Haskell should be easily translatable to Clojure and Redcat.
* Offer good performance.

## Instalation & Requirements
FIXME - THIS FILE IS UNDER DEVELOPMENT. VERSION 0.1.0 HAS NOT BEEN RELEASED TO CLOJARS YET. CURRENTLY, YOU CAN ONLY USE REDCAT IF YOU BUILD IT FROM THE SOURCE.
Add the following dependency to your `project.clj` file:
```clojure
[org.uncomplicate/redcat "0.1.0"]
```

Redcat artifacts are distributed through Clojars, so they will be downloaded by leiningen by default. If you are using other tools for dependency management, you can download Redcat from Clojars.org, or build it from the source by running `lein jar`.

Redcat requires at least Clojure 1.5, since it uses its reducers library.
Reducers use java fork/join, so you have to run it on Java 7+ jdk, or include jsr166y.jar in your project dependencies (see [Clojure's POM] (https://github.com/clojure/clojure/blob/master/pom.xml) for the dependency info).

## Usage

## Get Involved

## Documentation & Examples

## Project Maturity

## License

Copyright © 2013 Dragan Djuric

Distributed under the Eclipse Public License, the same as Clojure
