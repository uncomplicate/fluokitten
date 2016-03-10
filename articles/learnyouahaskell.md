---
title: "Fluokitten: Learn You a Haskell for Great Good - in Clojure"
layout: article
---

FIXME - To be written. LOTS OF CODE IS ALREADY [HERE](https://github.com/uncomplicate/fluokitten/blob/master/test/uncomplicate/fluokitten/articles)

This tutorial is a Clojure version of the book [Learn You a Haskell for Great Good](http://learnyouahaskell.com). This article is not self-contained: it is ment to be read side-by-side with the [book](http://learnyouahaskell.com), and used as a commentary and reference for the Clojure version of the examples. Due to the differences in typing, data structures, support for varargs and currying, there are differences in how the concepts that the original article explains are implemented in Haskell and Clojure.

It is recommended that you first read the article [Functors, Applicatives, and Monads in Pictures - in Clojure](/articles/functors_applicatives_monads_in_pictures.html) before reading this article.

To be able to follow along, you'll have to have Clojure installed and Fluokitten library included as a dependency in your project, as described in [Getting Started Guide](/articles/getting_started.html). Obviously, you'll need a reasonable knowledge of Clojure (you don't have to be an expert, though), and being familiar with the basics of Haskell is helpful, but not a necessity. So, after checking out [Getting Started Guide](/articles/getting_started.html), start up Clojure REPL and open this article and the [Learn You a Haskell for Great Good](http://learnyouahaskell.com) book side by side and we are ready to go.

The complete source code of the examples used in this article is available [here](https://github.com/uncomplicate/fluokitten/blob/master/test/uncomplicate/fluokitten/articles) in the form of [midje](https://github.com/marick/Midje) tests.

## Chapter 6 - Higher order functions

### Curried functions

## Chapter 8 - Making our own types and typeclasses

### The Functor typeclass

## Chapter 11 - Functors, Applicative Functors and Monoids

### Functors redux

### Applicative functors

### Monoids

#### Lists are monoids

#### Maybe the monoid

#### Using monoids to fold data structures

## Chapter 12 - A Fistful of Monads

### Getting our feet wet with Maybe

### The Monad typeclass

### Walk the line

### do notation

### The list monad

#### King's quest

### Monad laws

#### Left identity

#### Right identity

#### Associativity

## Chapter 13 - For a Few Monads More

### Writer? I hardly know her

#### Monoids to the rescue

#### The writer type

#### Inefficient list construction

#### Using do notation with Writer

#### Adding logging to programs

#### Difference lists

#### Comparing performance

### Reader? Ugh, not this joke again

### Tasteful stateful computations

#### Stacks and stones

#### The state monad

#### Randomness and the state monad

### Error error on the wall

### Some useful monadic functions

#### liftM and friends

#### The join function

#### filterM

#### foldM

#### Making a safe RPN calculator

#### Composing monadic functions

### Making monads

## Conclusion

## Where to Go Next

## Tell Us What You Think!

Please take some time to tell us about your experience with the library and this site. [Let us know](/articles/community.html) what we should be explaining or is not clear enough. If you are willing to contribute improvements, even better!
