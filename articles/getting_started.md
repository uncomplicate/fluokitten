---
title: "Getting Started"
layout: article
---
# Get Started
This is a brief introductory guide to Fluokitten that is aimed at giving you the basic technical information on how to start with Fluokitten. It should also give you an overview of the available resources that will help you learn category theory related concepts in programming and use them in Clojure with Fluokitten.

## How to get started
* Walk through this guide and set up your development environment
TODO OUTLINE THE NEXT STEPS

## What this guide covers

TODO

## Overview

TODO

## Installation

Fluokitten is a Clojure library that is packaged in a `jar` file, which is distributed through Clojars. It follows [maven](http://www.maven.org) (and [leiningen](http://www.leiningen.org) naming conventions: `groupId` is `uncomplicate` and `artifactId` is `fluokitten`.

### With Leiningen

The most straightforward way to include Fluokitten in your project is with leiningen. Add the following dependency to your `project.clj`:

```clojure
[uncomplicate/fluokitten "0.1.0"]
```

### With Maven

Add Clojars repository definition to your pom.xml:

```xml
<repository>
  <id>clojars.org</id>
  <url>http://clojars.org/repo</url>
</repository>
```

And then the dependency:

```xml
<dependency>
  <groupId>uncomplicate</groupId>
  <artifactId>fluokitten</artifactId>
  <version>0.1.0</version>
</dependency>
```

### Use up-to-date version

Note that you should replace `0.1.0` with the latest version of uncomplicate that is available in Clojars at [this link](https://clojars.org/uncomplicate/fluokitten).
If you are using other tools for dependency management, you can download Fluokitten jar file manually from Clojars.org, or build it from the source by running `lein jar`.

## Requirements

Fluokitten requires at least Clojure `1.5`, since it uses its reducers library.
Reducers use java fork/join, so you have to run it on Java 7+ jdk, or Java 6 with `jsr166y.jar` included in your project dependencies (see [Clojure's POM] (https://github.com/clojure/clojure/blob/master/pom.xml) for the dependency info). Fluokitten does not have other dependencies or requirements, except that it provides test helpers that require [midje](https://github.com/marick/Midje) testing library, wich you will have to include in your test dependencies if you would like to use it in your tests.

## Usage

See the [source code](https://github.com/uncomplicate/fluokitten/blob/master/test/uncomplicate/fluokitten/articles/getting_started_test.clj) used in this tutorial as a midje test.

First `use` or `require` `uncomplicate.fluokitten.core` and `uncomplicate.fluokitten.jvm` in your namespace, and you'll be able to call appropriate functions from the Fluokitten library.

```clojure
(ns example
  (:use [uncomplicate.fluokitten core jvm]))
```

Which functions are available? Not many (which is a good thing), because the main point of categorical programming is to provide a small set of highly generalized and well defined concepts that can be mixed, matched, and combined in many ways to get the desired effects. This getting started guide shows some simple examples mostly using Fluokiten extensions of Clojure built-in constructs. [More detailed guides](/articles/guides.html) will fully cover all functionality, most important of which enable you to make your own categorical implementations. It is also advisable to check out our list of tutorials about category theory and related concepts to understand conceptually how to effectively use Fluokitten. At any time, you can fall back to the [Fluokitten API documentation](/codox) and the [source code](https://github.com/uncomplicate/fluokitten) for the very specifics.

### Functors and fmap

The basic categorical concepts deal with contexts in which your data might be put. One fairly simple context is a sequence that might hold data, for example multiple numbers. Now, let's say that we have a plain old function that operates on numbers and we want to apply it to the data transparently, regardless of the actual box (context) it is in. Fortunately, Clojure already have a function that can reach inside the sequence and apply a function to each element - it's the ubiquitous `map` function:

```clojure
(map inc [1 2 3])
=> (1 2 3)
```

The only, usually minor, problem with `map` is that it converts the input into a lazy sequence, so our context is damaged a bit - we started with a vector and ended with a sequence.

Let's try an alternative - `fmap` function:

```clojure
(fmap inc [1 2 3])
=> [1 2 3])
```

Similar to `map`, but the starting context is preserved. `fmap` is a function that can reach inside any context that implements `Functor` protocol (in this case, plain Clojure vector), apply some plain function (in this case, inc) to the data inside the context (here, numbers) and produce the result that is inside the same type of context.
Fluokitten extends all Clojure collections with the `Functor` protocol, and provides specific implementations of fmap for each of them. Here are a couple of examples.

```clojure
(fmap + [1 2 3] [1 2 3 4])
=> [2 4 6]

(fmap + (list 1 2 3) [1 2] #{1 2 3 4})
=> (3 6)

(fmap + {:a 1 :b 2} {:a 3 :c 4} {:d 5})
=> {:a 4 :b 2 :c 4 :d 5}
```

Of course, Clojure sequences are not the only implementations of the `Functor` protocol. Fluokitten extends most of the Clojure types with the appropriate implementations of `Functor` protocol, some of them are shown in the following code snippet.

```clojure
(fmap * (atom 2) (ref 3) (atom 4))
=> (atom 24)

((fmap inc *) 2 3)
=> 7
```

Also, you can build your own implementations, which is covered in [detailed guides](/articles/guides.html).

### Applicative Functors, pure and fapply

Starting with the same idea of data inside a context, we can extend it to the function part: What if we want to apply a plain function or functions that are inside context with the data inside context? For example, a vector full of functions to data inside vector(s). There is a function for that in Fluokitten: `fapply`:



### Monads

### Monoids

### Foldables

## Tell us what you think!

TODO
