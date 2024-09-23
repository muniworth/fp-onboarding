# Functional Programming
### Defining Functional Programming
```
"The Essence of Functional Programming"
- Richard Feldman, FnConf 2022
https://www.youtube.com/watch?v=l0ruvPCQh9I

"The Next Paradigm Shift in Programming"
- Richard Feldman, 2020
https://youtu.be/6YbK8o9rZfI
```

### Functors
A functor is a container that you can map over (i.e. inject transform logic into).
```
type functor = {
   // aliases: Fmap, Map, Select
   fmap: M<A> -> (A -> B) -> M<B>
}

"An Introduction to Functors in Javascript"
- Ijemma Onwuzulike, 2020
https://youtu.be/XcM39gnqgNc
```

### Monads
Definition 1 (from Mark Seemann):
A monad is a functor that can be flattened.

Definition 2 (loosely taken from Richard Feldman in the podcast Software Unscripted):
A monad is a `.then()`-able, i.e. a chainable data structure that takes callbacks
```
type monad = functor & {
   // aliases: Bind, FlatMap, Collect, Selectmany, Chain, Then
   // Equivalent to `FMap >> Flat`
   bind: M<A> -> (A -> M<B>) -> M<B>
}
```

### [Bind](https://fsharpforfunandprofit.com/posts/elevated-world-2/#bind)
Handle the "happy path" for code last and with type safety, ex.
```
parse1 ThingToParse
|> Result.Bind parse2
|> Result.Filter predicate
|> Result.Map doSomethingUseful
```

### [Lift](https://fsharpforfunandprofit.com/posts/elevated-world/#return)
aliases: return, pure, unit, yield, point

Not to be confused with [Lift](https://fsharpforfunandprofit.com/posts/elevated-world/#lift), an alias for FMap. FSharp calls Lift 'return'.

```
"Monads are everywhere... Maybe that's bad?"
Explains Monads and Algebraic Effects (Koka, Unison).
- Till Schröder
https://youtu.be/nGhoZzihbHY

"Mo'Problems, Mo'Nads"
- Kyle Simpson
https://youtu.be/bg0Wtz3sR9U

"Hitler reacts to functional programming"
https://youtu.be/ADqLBc1vFwI
```

### Computation Expressions
```
"Computation Expressions Explained | Step-By-Step Tutorial | F# Functional Programming"
by Ben Gobeil, 2021
https://youtu.be/pC4ZIeOmgB0
```