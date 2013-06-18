---
title: "Fluokitten Extensions of Clojure Core"
layout: article
---

# Fluokitten Extensions of Clojure Core

This article is a guide to using Clojure core artifacts as implementations of Fluokitten protocols. By requiring `org.uncomplicate.fluokitten.jvm` namespace in your namespace, you activate Fluokitten's extensions to Clojure core types so they can act as functors, applicatives, monads etc.

To be able to follow this article, you'll have to have Clojure installed and Fluokitten library included as a dependency in your project, as described in [Getting Started Guide](/articles/getting_started.html). Obviously, you'll need a reasonable knowledge of Clojure (you don't have to be an expert, though). So, after checking out [Getting Started Guide](/articles/getting_started.html), start up Clojure REPL and open this article and we are ready to go.

The complete source code of the examples used in this article is available [here](https://github.com/uncomplicate/fluokitten/blob/master/test/uncomplicate/fluokitten/articles/fluokitten_extensions_clojure_core_test.clj) in the form of [midje](https://github.com/marick/Midje) tests.

## Data structures

The following is a list of common Clojure core data structures and their behavior as various Fluokitten protocols implementations.
Generally, any data structure can be considered as a specific context that can hold zero, one or more values.

### Functor

`fmap` behaves in the same way as `map` for most data structures, but takes care that the resulting data structure is of the same type as the first argument after the function. You have to provide as many additional arguments as the function support.

```clojure
(fmap inc [])
;=> []

(fmap inc [1 2 3])
;=> [2 3 4]

(fmap + [1 2] [3 4 5] [6 7 8])
;=> [10 13]

(fmap inc (list))
;=> (list)

(fmap inc (list 1 2 3))
;=> (list 2 3 4)

(fmap + (list 1 2) (list 3 4 5) (list 6 7 8))
;=> (list 10 13)

(fmap inc (empty (seq [1]))) => (empty (seq [2]))

(fmap inc (seq [1 2 3]))
;=> (seq [2 3 4])

(fmap + (seq [1 2]) (seq [3 4 5]) (seq [6 7 8]))
;=> (seq [10 13])

(fmap inc (lazy-seq []))
;=> (lazy-seq [])

(fmap inc (lazy-seq [1 2 3]))
;=> (lazy-seq [2 3 4])

(fmap + (lazy-seq [1 2]) (lazy-seq [3 4 5]) (lazy-seq [6 7 8]))
;=> (lazy-seq [10 13])

(fmap inc #{})
;=> #{}

(fmap inc #{1 2 3})
;=> #{2 3 4}

(fmap + #{1 2} #{3 4 5} #{6 7 8})
;=> #{10 13}
```
Clojure maps (data structure) have a richer structure than the previously described structures. While `map` would treat a map structure as a sequence of entries, `fmap` treats is as a context and just applies a function to the values, while taking care that keys are preserved and matched in the case of a modifying function that takes more than one argument:

```clojure
(fmap inc {})
;=> {}

(fmap inc {:a 1 :b 2 :c 3})
;=> {:a 2 :b 3 :c 4}

(fmap + {:a 1 :b 2} {:a 3 :b 4 :c 5} {:a 6 :b 7 :c 8})
;=> {:a 10 :b 13 :c 13}
```
Map entries are functors, too. The modifying function is applied to the value(s) of map entry or entries, and the key of the first entry will be the key of the result:

```clojure
(fmap inc (first {:a 1}))
;=> (first {:a 2})

(fmap + (first {:a 1})
        (first {:a 3})
        (first {:b 6}))
;=> (first {:a 10})
```
Finally, Clojure's [reducibles](http://clojure.com/blog/2012/05/15/anatomy-of-reducer.html) are also properly handled by `fmap` (but it can only accept one reducible at a time):

```clojure
(into [] (fmap inc (r/map identity  [1 2 3])))
;=> [2 3 4]

(into [] (fmap + (r/map identity [1 2])
                 (r/map identity [3 4 5])
                 (r/map identity [6 7 8])))
;=> (throws UnsupportedOperationException)
```

### Applicative

For most core structures, `pure` produces a collection of the required type, with the supplied value as its only element:

```clojure
(pure [4 5] 1)
;=> [1]

(pure (list) 1)
;=> (list 1)

(pure (seq [3]) 1)
;=> (seq [1])

(pure (lazy-seq [4]) 1)
;=> (lazy-seq [1])

(pure #{} 1)
;=> #{1}
```

For maps and map entries, `pure` uses nil to generate the default key for the pure entry:

```clojure
(pure {} 1)
;=> {nil 1}

(pure (first {1 1}) 1)
;=> (first {nil 1})
```

`pure` handles Clojure [reducibles](http://clojure.com/blog/2012/05/15/anatomy-of-reducer.html):

```clojure
(into (list) (pure (r/map identity [2 3]) 1))
 => (list 1)
```

`fapply` is pretty straightforward for most structures (except for maps and map entries, where it has a more complex behavior). It expect two structures of the same type as arguments: a function(s) structure and a data structure. Then it applies the functions to the data. The actual matching of the functions and the data depends on the data structure. For most core structures, it applies all combinations of functions and data:

```clojure
(fapply [] [])
;=> []

(fapply [] [1 2 3])
;=> []

(fapply [inc dec] [])
;=> []

(fapply [inc dec] [1 2 3])
;=> [2 3 4 0 1 2]

(fapply [+ *] [1 2 3] [4 5 6])
;=> [5 7 9 4 10 18]

(fapply (list) (list))
;=> (list)

(fapply (list) (list 1 2 3))
;=> (list)

(fapply (list inc dec) (list))
;=> (list)

(fapply (list inc dec) (list 1 2 3))
;=> (list 2 3 4 0 1 2)

(fapply (list + *) (list 1 2 3) (list 4 5 6))
;=> (list 5 7 9 4 10 18)

(fapply (empty (seq [2])) (empty (seq [3])))
;=> (empty (seq [1]))

(fapply (empty (seq [33])) (seq [1 2 3]))
;=> (empty (seq [44]))

(fapply (seq [inc dec]) (empty (seq [1])))
;=> (empty (seq [3]))

(fapply (seq [inc dec]) (seq [1 2 3]))
;=> (seq [2 3 4 0 1 2])

(fapply (seq [+ *]) (seq [1 2 3]) (seq [4 5 6]))
;=> (seq [5 7 9 4 10 18])

(fapply (lazy-seq []) (lazy-seq []))
;=> (lazy-seq [])

(fapply (lazy-seq []) (lazy-seq [1 2 3]))
;=> (lazy-seq [])

(fapply (lazy-seq [inc dec]) (lazy-seq []))
;=> (lazy-seq [])

(fapply (lazy-seq [inc dec]) (lazy-seq [1 2 3]))
;=> (lazy-seq [2 3 4 0 1 2])

(fapply (lazy-seq [+ *])
        (lazy-seq [1 2 3])
        (lazy-seq [4 5 6]))
;=> (lazy-seq [5 7 9 4 10 18])

(fapply #{} #{})
;=> #{}

(fapply #{} #{1 2 3})
;=> #{}

(fapply #{inc dec} #{})
;=> #{}

(fapply #{inc dec} #{1 2 3})
;=> #{2 3 4 0 1}

(fapply #{+ *} #{1 2 3} #{4 5 6})
;=> #{5 7 9 4 10 18}
```

In the case of maps and map entries, it matches the functions and the data by map keys, while the `nil` key serves as a universal key, whose function applies to any data that does not have the specific matching function with the same key:

```clojure
(fapply {} {})
;=> {}

(fapply {} {:a 1 :b 2 :c 3})
;=> {:a 1 :b 2 :c 3}

(fapply {:a inc} {})
;=> {}

(fapply {:a inc :b dec nil (partial * 10)}
        {:a 1 :b 2 :c 3 :d 4 nil 5})
;=> {:a 2 :b 1 :c 30 :d 40 nil 50}

(fapply {nil /  :a + :b *} {:a 1 :c 2} {:a 3 :b 4} {:c 2 :d 5})
;=> {:a 4 :b 4 :c 1 :d 1/5}

(fapply (first {:a inc}) (first {:b 1}))
;=> (first {:b 1})

(fapply (first {:a inc}) (first {:a 1}))
;=> (first {:a 2})

(fapply (first {nil inc}) (first {:a 1}))
;=> (first {:a 2})

(fapply (first {nil inc}) (first {nil 1}))
;=> (first {nil 2})
```

### Monad

Similarily to the previous examples, `bind` behaves in a straightforward way for most core data structures, except maps and map entries that are a bit specific. It requires one or more data structures, and a function as the last parameter. That function should accept elements extracted from all provided data structures (one by one), and wrap the result(s) in the same type of structure. Here are typical examples of sequences and friends:

```clojure
(bind [] (comp vector inc))
;=> []

(bind [1 2 3] (comp vector inc))
;=> [2 3 4]

(bind [1 2 3] [4 5 6] (comp vector +))
;=> [5 7 9]

(bind (list) (comp list inc))
;=> (list)

(bind (list 1 2 3) (comp list inc))
;=> (list 2 3 4)

(bind (list 1 2 3) (list 4 5 6) (comp list +))
;=> (list 5 7 9)

(bind (empty (seq [2])) (comp seq vector inc))
;=> (emp ty (seq [3]))

(bind (seq [1 2 3]) (comp seq vector inc))
;=> (seq [2 3 4])

(bind (seq [1 2 3]) (seq [4 5 6]) (comp seq vector +))
;=> (seq [5 7 9])

(bind (lazy-seq [])
      (fn [& args]
        (lazy-seq (vector (apply inc args)))))
;=> (lazy-seq [])

(bind (lazy-seq [1 2 3])
      (fn [& args]
        (lazy-seq (vector (apply inc args)))))
;=> (lazy-seq [2 3 4])

(bind (lazy-seq [1 2 3]) (lazy-seq [4 5 6])
      (fn [& args]
        (lazy-seq (vector (apply + args)))))
;=> (lazy-seq [5 7 9])

(bind #{} (comp hash-set inc))
;=> #{}

(bind #{1 2 3} (comp hash-set inc))
;=> #{2 3 4}

(bind #{1 2 3} #{4 5 6} (comp hash-set +))
;=> #{5 7 9}
```

With maps and map entries, `bind` treats keys as parts of the context, and feeds only the values of the corresponding entries to the function. Since the function returns a map for each entry, the results have to be flattened, so the final result is a map. It is easier to understand what happens with a few examples:

```clojure
(bind {} #(hash-map :x %)) => {}

(bind {:a 1} #(hash-map :increment (inc %)))
;=> {[:a :increment] 2}

(bind {:a 1  :b 2 :c 3} #(hash-map :increment (inc %)))
;=> {[:a :increment] 2 [:b :increment] 3 [:c :increment] 4}

(bind {:a 1} {:a 2 :b 3} {:b 4 :c 5} (fn [& args] {:sum (apply + args)}))
;=> {[:a :sum] 3 [:b :sum] 7 [:c :sum] 5}
```
So, the function takes the corresponding values for each key from each map, feed it to the function, which returns the result wrapped in a context of a map. Then, all the resulting maps are joined by pairing the key of the input and the key of the output for a particular value.

Map entries do not care about the keys when feeding the values to the function. Only the key of the first map entry is used as the key of the result:


```clojure
(bind (first {:a 1}) #(first {:increment (inc %)}))
=> (first {[:a :increment] 2})

(bind (first {:a 1}) (first {:a 2}) (first {:b 4}) (fn [& args] (first {:sum (apply + args)})))
=> {[:a :sum] 8}
```

The `join` function flattens the data structure if it contains nested data structures of the same type, in a similar way as clojure's `flatten` function, for all collections except maps. For maps, it have to take account of the nesting of the map's keys as parts of the context, by creating a vector of all the keys that were flattened and using it as the key for the value of the flattened entry, as shown in the following examples:

```clojure
(join [[1 2] [3 [4 5] 6]])
;=> [1 2 3 4 5 6]

(join (list (list 1 2) (list 3 (list 4 5) 6)))
;=> (list 1 2 3 4 5 6)

(join (seq (list (list 1 2) (list 3 (list 4 5) 6))))
;=> (seq (list 1 2 3 4 5 6))

(join (lazy-seq (list (list 1 2) (list 3 (list 4 5) 6))))
;=> (lazy-seq (list 1 2 3 4 5 6))

(join #{#{1 2} #{3 #{4 5} 6}})
;=> #{1 2 3 4 5 6}

(join {:a 1 :b {:c 2 :d {:e 3}}})
;=> {:a 1 [:b :c] 2 [:b :d :e] 3}

(join (first {:a (first {:b 1})}))
;=> (first {[:a :b] 1})
```

### Magma

`op` is a pretty straightforward for Clojure core data structures - it is similar to the `concat` function. The difference is that `concat` turns everything into lazy sequences while `op` preserves the type of the data structure. `op` is associative for all Clojure core data structures, so they form semigroups.

```clojure
(op [1 2 3] [4 5 6])
;=> [1 2 3 4 5 6]

(op [1 2 3] [4 5 6] [7 8 9] [10 11 12])
;=> [1 2 3 4 5 6 7 8 9 10 11 12]

(op (list 1 2 3) (list 4 5 6))
;=> (list 1 2 3 4 5 6)

(op (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12))
;=> (list 1 2 3 4 5 6 7 8 9 10 11 12)

(op (lazy-seq [1 2 3]) (lazy-seq [4 5 6]))
;=> (lazy-seq [1 2 3 4 5 6])

(op (lazy-seq [1 2 3]) (lazy-seq [4 5 6])
    (lazy-seq [7 8 9]) (lazy-seq [10 11 12]))
;=> (lazy-seq [1 2 3 4 5 6 7 8 9 10 11 12])

(op (seq [1 2 3]) (seq [4 5 6]))
;=> (seq [1 2 3 4 5 6])

(op (seq [1 2 3]) (seq [4 5 6])
    (seq [7 8 9]) (seq [10 11 12]))
;=> (seq [1 2 3 4 5 6 7 8 9 10 11 12])

(op #{1 2 3 6} #{4 5 6})
;=> #{1 2 3 4 5 6}

(op #{1 2 3 6} #{4 5 6} #{7 8 9} #{10 11 12})
;=> #{1 2 3 4 5 6 7 8 9 10 11 12}

(op {:a 1 :b 2} {:a 3 :c 4})
;=> {:a 3 :b 2 :c 4}

(op {:a 1 :b 2} {:a 3 :c 4} {:d 5} {:e 6})
;=> {:a 3 :b 2 :c 4 :d 5 :e 6}

(op (first {:a 1}) (first {:b 2}))
;=> (first {:ab 3})

(op (first {:a 1}) (first {:b 2}) (first {:b 3}))
;=> (first {:abb 6})
```

### Monoid
`id` for Clojure's data structures' `op` is an empty structure of the same type as the `id`'s argument:

```clojure
(id [2])
;=> []

(id (list 4 5 6))
;=> (list)

(id (seq [1 2]))
;=> (empty (seq [2]))

(id (lazy-seq [1 23]))
;=> (lazy-seq [])

(id #{2 3})
;=> #{}

(id {:1 2})
;=> {}

(id (first {:a 1}))
;=> [(keyword "") 0]
```

### Foldable

The `fold` function aggregates the content of the core data structures into one value. All elements in the data structure must belong to the same monoid, i.e. they have the same type that implementat Monoid protocol, or of types compatible with the first element's type for the Magma's `op` function.

```clojure
(fold [1 2 3 4 5 6])
=> 21

(fold (list "a" "b" "c"))
;=> "abc"

(fold (seq [:a :b :c]))
;=> :abc

(fold (lazy-seq [[1] (list 2) (seq [3]])))
;=> [1 2 3]

(fold #{1 2 3})
;=> 6

(fold {:a 1 :b 2 :c 3})
;=> 6

(fold (first {:a 1}))
;=> 1
```

## Objects


### Applicative

### Monad

### Magma

### Monoid

### Foldable


## String


### Applicative

### Monad

### Magma

### Monoid

### Foldable


## Keyword


### Applicative

### Monad

### Magma

### Monoid

### Foldable

## Numbers

### Applicative

### Monad

### Magma

### Monoid

### Foldable



## Functions

### Applicative

### Monad

### Magma

### Monoid

### Foldable

## References

### Applicative

### Monad

### Magma

### Monoid

### Foldable
