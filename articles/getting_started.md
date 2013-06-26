---
title: "Getting Started"
layout: article
---
# Get Started
This is a brief introductory guide to Fluokitten that is aimed at giving you the basic technical information on how to start. It should also give you a brief overview of the available resources that will help you learn category theory related concepts in programming and use them in Clojure with Fluokitten.

## How to get started
* Walk through this guide, set up your development environment, and try the examples;
* Read some of the [introductory tutorials](/articles/guides.html#tutorials_on_category_theory_in_programming)  while trying the relevant code examples;
* As you go, familiarize yourself with Fluokitten by reading [detailed articles](/articles/guides.html#fluokitten_documentation_and_tutorials) and API documentation;
* Follow along with advanced tutorials and [write your own implementations of Fluokitten protocols](/articles/guides.html).

## Overview

Fluokitten is a Clojure library that helps you use programming techniques inspired by category theory. It provides:

* Core library with categorical functions `uncomplicate.fluokitten.core`;
* Protocols for categorical concepts `uncomplicate.fluokitten.protocols`;
* Implementations of these protocols, which adds support to Clojure built-in constructs (collections, functions, etc.) `uncomplicate.fluokitten.jvm`;
* Macros and functions that help you write your own implementations of these protocols;
* Accompanying website with learning resources.

## Installation

Fluokitten is a Clojure library that is packaged in a `jar` file, which is distributed through Clojars. It follows [maven](http://www.maven.org) (and [leiningen](http://www.leiningen.org)) naming conventions: `groupId` is `uncomplicate` and `artifactId` is `fluokitten`.

### With Leiningen

The most straightforward way to include Fluokitten in your project is with leiningen. Add the following dependency to your `project.clj`:

```clojure
[uncomplicate/fluokitten "0.2.0"]
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
  <version>0.2.0</version>
</dependency>
```

### Use up-to-date version

Note that you should replace `0.2.0` with the latest version of uncomplicate that is available in Clojars at [this link](https://clojars.org/uncomplicate/fluokitten).
If you are using other tools for dependency management, you can download Fluokitten jar file manually from Clojars.org, or build it from the source by running `lein jar`.

### Requirements

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
;=> (1 2 3)
```

The only, usually minor, problem with `map` is that it converts the input into a lazy sequence, so our context is damaged a bit - we started with a vector and ended with a sequence.

Let's try an alternative - `fmap` function:

```clojure
(fmap inc [1 2 3])
;=> [1 2 3])
```

Similar to `map`, but the starting context is preserved. `fmap` is a function that can reach inside any context that implements `Functor` protocol (in this case, plain Clojure vector), apply some plain function (in this case, inc) to the data inside the context (here, numbers) and produce the result that is inside the same type of context.
Fluokitten extends all Clojure collections with the `Functor` protocol, and provides specific implementations of fmap for each of them. Here are a couple of examples. Note that, depending on how many arguments the function can accept, we may provide many contexts to `fmap`.

```clojure
(fmap + [1 2 3] [1 2 3 4])
;=> [2 4 6]

(fmap + (list 1 2 3) [1 2] #{1 2 3 4})
;=> (3 6)

(fmap + {:a 1 :b 2} {:a 3 :c 4} {:d 5})
;=> {:a 4 :b 2 :c 4 :d 5}
```

Of course, Clojure collections are not the only implementations of the `Functor` protocol. Fluokitten extends most of the Clojure types with the appropriate implementations of `Functor` protocol, some of them are shown in the following code snippet.

```clojure
(fmap * (atom 2) (ref 3) (atom 4))
;=> (atom 24)

((fmap inc *) 2 3)
;=> 7
```

Also, you can build your own implementations, which is covered in [detailed guides](/articles/guides.html).

### Applicative functors, pure and fapply

Starting with the same idea of data inside a context, we can extend it to the function part: What if we want to apply a plain function or functions that are inside context with the data inside context? For example, a vector full of functions to data inside vector(s). There is a function for that in Fluokitten: `fapply`:

```clojure
(fapply [inc dec (partial * 10)] [1 2 3])
;=> [2 3 4 0 1 2 10 20 30]
```

`fapply` is a function that can reach inside any context that implements `Applicative` protocol (in this case, Clojure vector), apply function(s) that are provided in the same type of context (vector) and produce the result inside the context (here, vector). In the case of vector as an `Applicative`, the resulting context contains all combinations of applying the provided functions and data.
Here are some more simple examples:

```clojure
(fapply [+ -] [1 2] [3 4])
;=> [4 6 -2 -2]

(fapply {:a + :b *} {:a 1 :b 2} {:a 3 :b 3 :c 4} {:d 5})
;=> {:a 4, :b 6, :c 4, :d 5}
```

`Applicative`s also support a function that puts any data into a minimal context of a certan type - `pure`.

```clojure
(pure [] 3)
;=> [3]

(pure (atom nil) 5)
;=> (atom 5)
```

Could we have implemented any of these functions to do anything we wished? NO! All these functions have to satisfy certain laws, which ensures they fit in with the rest of the framework regardless of the details. This guide does not cover these laws, but you should keep in mind for now that they have been taken into account in Fluokitten implementations and that you should take them into account once you start implementing your own categorical implementations. The laws are covered in advanced guides.

### Monads and bind

Monads are the most popular concept inspired by category theory, at least when it comes to applications in programming. They also deal with data in contexts. However, in contrast to functors and applicative functors, which were concerned with applying plain functions to data inside the contexts and transforming the data in that process, Monads are calso able to transforme the context by applying a function that accepts plain data (without a context) and produces the result inside a context, to the data that is inside a context. Sounds confusing? Until you gain some practical experience, it is - that is why there are so many tutorials on monads written every day. However, if you take the step-by-step approach to learning and not try to swallow everything in one sitting, it shouldn't be hard at all. So, in this tutorial we will only show a few brief examples of monads in Clojure, while you are expected to check out advanced tutorials and guides later to understand what's is all about.
The central function here is `bind`, and in the case of vector, it can be used as follows (remember, the example is rather trivial):

```clojure
(bind [1 2 3] #(vector (inc %) (dec %)))
;=> [2 0 3 1 4 2]

(bind (atom 1) (comp atom inc))
;=> (atom 2)
```

Fluokitten implements `Monad` protocol for many Clojure core types. Please check out the tutorials and docs and be patient until it clicks for you.

### Monoids

`Monoid` is a protocol that offers a default operation `op` on some type, and an identity element, `id` for that operation. `op` has to be closed, meaning (op x y) must have the same type as x and y, and it has to be associative. For example, the default operation for numbers in Fluokitten is +, with the identity element 0, while for lists it is concat, with the default element empty list.

```clojure
(id 4)
;=> 0

(op 1 2)
;=> 3

(id [4 5 6])
;=> []

(op [1 2] [3])
;=> [1 2 3]
```

### Foldables and fold

The previous concepts manipulated with contexts and the data they hold. But, how do we take the data from the context in a generalized way, without using methods provided by functions created specifically for a particular context? If we implement the `Foldable` protocol, which Fluokitten does for many Clojure types, we can use `fold` function to get a summary of the things inside the context:

```clojure
(fold (atom 3))
;=> 3
```
When there is a single value in a context, like in the previous example, usually it is enough to just return that value. But, when there is more than one thing in a context, or no things at all, as is the case with the vectors in the following examples, fold should return some aggregate summary value from the context. If the data consists of monoids, fold can use the polymorphic `op` to summarize it transparently.


```clojure
(fold [])
;=> nil

(fold [1 2 3])
;=> 6
```

## Where to go next

This should be enough to get you started and explore more tutorials on this site on your own. I expect to build a comprehensive base of articles and references for learning this stuff, so please check the [All Guides](/articles/guides.html) page from time to time. More importantly, I will post article with Clojure code for various interesting articles, tutorials and video lectures related to this stuff that are already available on the web. Of course, you should also check Fluokitten API for specific details, and while you are there, it should be a good idea to peek in the source code.

## Tell us what you think!

Please take your time to inform us on your experience with this library and the accompanying site. [Let us know](/articles/community.html) what is uncovered or not clear enough. If you are willing to contribute improvements, even better!
