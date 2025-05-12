{ TODO: Search for "?" and fill in numbers.  }

# Definition of Functional Programming

In 1966, Peter Landin coined the term "Functional Programming" (FP) to describe
the language ISWIM [?]. However, he didn't provide a definition for FP, leaving
the term open for debate. Most attempts at defining functional programming
resort to referencing particular language features, such as closures and lack of
mutable state, but no language feature is essential to FP. Rather, functional
programming builds around one core tenet, ***compositionality***, the ability to
practically reason about programs by breaking them into components that can in
turn be understood independently.

For example, take the simple program `t + u`, where `t` and `u` are arbitrary
subprograms. To understand this program, we'd like to apply the principle of
compositionality by (1) understanding `t` (independently of `u`), (2)
understanding `u` (independently of `t`), and (3) understanding what it means to
add two things together. Functional languages seek to admit this logic (as much
as possible).

For instance, take
```
t = 2
u = 3
```
Obviously, the principle of compositionality applies. By contrast, consider
```
t = (x := 3; 2)
u = x
```
In this case, `u` cannot be understood independently of `t`, so the whole
program `t + u` can only be understood as a whole. The inability to reason
compositionally is not really felt in this small example, but the
interconnectedness of large components in large programs quickly becomes
unmanageable. Indeed, the inapplicability of the principle of compositionality
is the very definition of complexity [?].

The value of compositional reasoning can only be learned through experience, but
we now take it for granted and explore its consequences.

# Effects

We saw above how state can preclude compositional reasoning, but state is not
uniquely problematic in this regard. More generally, we must carefully manage
all effects to program functionally.

Vaguely speaking, an ***effect*** is any aspect of a computation that can be
viewed as an interaction with the ambient execution environment. (The "execution
environment" is merely a conceptual device; it doesn't necessarily have some
cohesive existence during the execution of a program.) Common effects include
the following:
- **Failure**: Programs interact with their execution environment by asking
  it to abort execution, often with some detail for why execution must abort
  (e.g., `throw error("Something broke!")`).
- **State**: The environment records the current value of the state, and
  programs interact with the environment by getting and setting the state.
- **File I/O**: The environment has access to a file system, and programs
  interact with their environment by asking it to manipulate files.

In other words, an effect is any aspect of a computation whose meaning is
(partially) determined from *without*, whereas the principle of compositionality
requires meaning to come entirely (or as much as possible) from *within*. And
therein lies the fundamental incompatibility of effects and compositionality.

So, if we're not willing to budge on compositionality, must we give up effects
entirely? No! If that were the case, functional programming would surely be
useless:

> In the end, a program with no effect, there's no point in running it, is
> there? You have this black box and you press go and it gets hot, but there's
> no output. Why did you run the program? The reason to run a program is to have
> an effect.
>
> -- Simon Peyton Jones [?]

The key to the compositionality/effect dilemma is to *encode* effects without
immediately *performing* them. That is, instead of writing a program that, say,
issues a command to print `"foo"`, we instead collect all the information needed
to print `"foo"` (the string `"foo"` itself, an identifier for the file we're
printing to, etc.) and then do nothing. Thus, we can have our cake and eat it,
too: we can speak of effects, but since the *encodings* of the effects are pure
(that is, void of effects), we don't lose compositionality. The importance of
this idea can not be overstated; it is the main concept you need to understand
to program in a pure style effectively (pun intended).

To be clear, we have not trivialized the problem by simply never performing
effects. We *will* eventually perform the effects, but where and how we perform
them depends on the concrete effect in question. This will all become clear
later, after some examples.

### Further Information

{ TODO: Just move these to references and summarize them here, so readers can
decide if they want to watch them fully? }

Richard Feldman, "The Essence of Functional Programming", [FnConf 2022](https://www.youtube.com/watch?v=l0ruvPCQh9I).

Richard Feldman, "The Next Paradigm Shift in Programming", [ETE 2020](https://youtu.be/6YbK8o9rZfI)

# Encoding Effects

We encode effects as type-level functions `f` that send a type `a` of "pure
values" to a type `f a` of "effectful values". Additonally, each concrete effect
comes equipped with a set of operations for creating and manipulating effectful
values.

Let's see how this plays out on the three effects mentioned above, namely
failure, state, and file I/O.

## Failure

We encode the effect of failure, with failure detail of type `e`, as follows:
```Haskell
data Fallible e a = Success a | Failure e
--   ^^^^^^^^ ^^^   ^^^^^^^ ^   ^^^^^^^ ^
--       1     2       3    4      3    4
```
> **Haskell novices**: Read this type declaration as follows:
> 1. Name of the new type.
> 2. Type parameters. I.e., `Fallible e a` is a type for any types `e` and `a`.
>    You may know such type parameters as "generics" from other languages.
> 3. Constructor names. In general, data types can have any number of
>    constructors, but here we only require two.
> 4. Fields. In general, each constructor can have multiple fields, but here we
>    only require one per constructor.

That is, a `Fallible e a` computation either succeeds, returning a value of
type `a`, or it fails, returning an error of type `e`.

Immediately from the definition of `Fallible` we get two essential operations
of failure effects:
```Haskell
pure :: a -> Fallible e a -- (1)
pure = Success -- (2)

fail :: e -> Fallible e a
fail = Failure
```
> **Haskell novices**: Line (1) declares the type of `pure`, which implicitly
> quantifies over each variable within that begins with a lower case letter
> (`a` and `e` in this case), so we could equivalently write
> ```Haskell
> pure :: forall a e. a -> Fallible e a
> ```
> Line (2) defines `pure`.

(Of course, we could just as well directly use `Success` and `Failure` instead
of `pure` and `fail`.) We choose the name "`pure`", because it lifts values into
the "pure fragment" of `Fallible e`, i.e., the part of `Fallible e` that doesn't
really have any failure effect.

For example, let's create a function `uncons` for splitting a list into its head
(first element) and tail (all but first element), or failing if the input list
is empty:
```Haskell
data UnconsError = UnconsError

uncons :: [a] -> Fallible UnconsError (a, [a])
uncons = \case
    x:xs -> pure (x, xs)
    [] -> fail UnconsError
```
> **Haskell novices**: The definition of `uncons` pattern-matches on a list, a
> built-in type approximately defined as
> ```Haskell
> data List a = Nil | Cons a (List a)
> ```
> except we write `[a]` for `List a`, `[]` for `Nil`, and `x:xs` for
> `Cons x xs`. The "`\case ⟨cases⟩`" notation is syntactic sugar for
> "`\x -> case x of ⟨cases⟩`", where `\p -> e` is a lambda (i.e., function
> expression) with pattern `p` and body `e`.

We can repeatedly apply `uncons` to pull, say, four elements off the start of a
list:
```Haskell
uncons4 :: [a] -> Fallible UnconsError (a, a, a, a, [a])
uncons4 xs0 = case uncons xs0 of
    Success (x0, xs1) -> case uncons xs1 of
        Success (x1, xs2) -> case uncons xs2 of
            Success (x2, xs3) -> case uncons xs3 of
                Success (x3, xs4) -> pure (x0, x1, x2, x3, xs4)
                Failure e -> fail e
            Failure e -> fail e
        Failure e -> fail e
    Failure e -> fail e
```
Yikes! Often, we want implicit propagation of errors, but here we suffer a
manual error check after each call to `uncons`. Following good programming
practice of abstracting out duplicate code, let's write a function `bind` that
automatically propagates errors, allowing us to focus on the success case:
```Haskell
bind :: Fallible e a -> (a -> Fallible e b) -> Fallible e b
bind fa fb = case fa of
    Success a -> fb a
    Failure e -> fail e
```
Intuitively, `bind` sequences two fallible computations, where the second
computation can depend on the output value of the first computation, such that
the composite computation fails if either input computation fails.

With the help of `bind`, `uncons4` simplifies to the following:
```Haskell
uncons4 xs0 =
    bind (uncons xs0) \(x0, xs1) ->
        bind (uncons xs1) \(x1, xs2) ->
            bind (uncons xs2) \(x2, xs3) ->
                bind (uncons xs3) \(x3, xs4) ->
                    pure (x0, x1, x2, x3, xs4)
```
Much better, although we still must endure the excessive indentation (for now).

### Exercise ?: `catch`

Implement the following function for catching errors:
```Haskell
catch :: Fallible e a -> (e -> Fallible f a) -> Fallible f a
```
(In part, this exercise asks you to figure out what `catch` should do based on
its type signature.)

### Exercise ?: The Duality of Success and Failure

Let `dual` be the following function:
```Haskell
dual :: Fallible e a -> Fallible a e
dual = \case
    Success a -> Failure a
    Failure e -> Success e
```
Use `dual` to equationally relate `bind` and `catch`. That is, using only `dual`
and `bind`, implement `catch`, and using only `dual` and `catch`, implement
`bind`.

### Summary

```Haskell
data Fallible e a
pure :: a -> Fallible e a
bind :: Fallible e a -> (a -> Fallible e b) -> Fallible e b
fail :: e -> Fallible e a
catch :: Fallible e a -> (e -> Fallible f a) -> Fallible f a
```

## State

We encode the effect of state of type `s` as a function from an initial state
value to an output value paired with a final state value:
```Haskell
data State s a = State (s -> (a, s))
```

Like with `Fallible e`, `State s` has a pure fragment containing computations
that don't really get or set the state, and we enter it with `pure`:
```Haskell
pure :: a -> State s a
pure a = State \s -> (a, s)
```

We will also find it convenient to have a suggestively-named function to strip
off the `State` constructor of a state action:
```Haskell
runState :: State s a -> s -> (a, s)
runState (State f) = f
```

As an example, let's write a pseudorandom number generator:
```Haskell
-- Generate a stream x of 64-bit psuedorandom numbers of form
--     x[i+1] = (a * x[i] + c) mod m
-- for parameters a, c, and m, a so-called linear congruential generator. We
-- take parameters from Knuth. The mod operation is implicit, because m = 2^64.
rand :: State Word64 Word64
rand = State \s ->
    let x = 6364136223846793005 * s + 1442695040888963407
    in (x, x)
```
We can repeatedly run `rand` to generate, say, four random numbers:
```Haskell
rand4 :: State Word64 (Word64, Word64, Word64, Word64)
rand4 = State \s0 -> case runState rand s0 of
    (x0, s1) -> case runState rand s1 of
        (x1, s2) -> case runState rand s2 of
            (x2, s3) -> case runState rand s3 of
                (x3, s4) -> ((x0, x1, x2, x3), s4)
```
Yikes! Usually we want the state to implicitly thread between state actions, but
here we suffer manual plumbing of the state value. Following good programming
practice of abstracting out duplicate code, let's write a function `bind` that
automatically threads state:
```Haskell
bind :: State s a -> (a -> State s b) -> State s b
bind sa sb = State \s ->
    case runState sa s of
        (a, s') -> runState (sb a) s'
```
Intuitively, `bind` sequences two stateful computations, where the second
computation can depend on the output value of the first computation.

With the help of `bind`, `rand4` simplifies to the following:
```Haskell
rand4 = bind rand \x0 ->
    bind rand \x1 ->
        bind rand \x2 ->
            bind rand \x3 ->
                pure (x0, x1, x2, x3)
```
Much better, although we still must endure the excessive indentation (for now).

### Exercise ?: `get` and `set`

Implement the following fundamental operations of the state effect:
```Haskell
-- Return the current state, leaving the state unchanged.
get :: State s s

-- Set the state and return the unit.
set :: s -> State s ()
```

### Example: Finite-State Automaton for Substring Containment

Let's write a function that checks if a string contains "xyz". This function
should have the following signature:
```Haskell
contains'xyz' :: String -> Bool
```
> **Haskell novices**: Haskell allows single quotes in names, except at the
> beginning.

Internally, this function will inspect its input character-by-character, and as
we move from one character to the next, we must remember how much of "xyz" we've
seen so far. We'll record this state with type `Q`:
```Haskell
data Q = Seen'' | Seen'x' | Seen'xy' | Seen'xyz'
```
Each input character changes the state according to the following function:
```Haskell
transition :: Char -> State Q ()
transition nextChar = bind get \currState ->
    case (currState, nextChar) of
        -- This is not the most economical set of cases.
        (Seen'', 'x') -> set Seen'x'
        (Seen'', _) -> set Seen''
        (Seen'x', 'x') -> set Seen'x'
        (Seen'x', 'y') -> set Seen'xy'
        (Seen'x', _) -> set Seen''
        (Seen'xy', 'x') -> set Seen'x'
        (Seen'xy', 'z') -> set Seen'xyz'
        (Seen'xy', _) -> set Seen''
        (Seen'xyz', _) -> set Seen'xyz'
```
Once we've scanned the entire input, we get the state to check if we've seen
"xyz":
```Haskell
haveSeen'xyz' :: State Q Bool
haveSeen'xyz' = bind get \case
    Seen'xyz' -> pure True
    _ -> pure False
```
With these two helper functions, we can implement a stateful version of
`contains'xyz'`:
```Haskell
statefulContains'xyz' :: String -> State Q Bool
statefulContains'xyz' = \case
    [] -> haveSeen'xyz'
    c:cs -> bind (transition c) \() -> statefulContains'xyz' cs
```
Finally, we implement the original `contains'xyz'` function as
`statefulContains'xyz'` with an initial state of `Seen''`:
```Haskell
contains'xyz' cs = fst (runState (statefulContains'xyz' cs) Seen'')
```
> **Haskell novices**: `fst` projects the first component out of a pair:
> ```Haskell
> fst :: (a, b) -> a
> fst (a, _) = a
> ```
> Here we use `fst` to get the output from a state computation, ignoring the
> final state.

### Exercise ?: `map`

In the implementation of `haveSeen'xyz'`, the second computation of the `bind`
is always pure. Write a function
```Haskell
map :: (a -> b) -> State s a -> State s b
```
and use it to simplify the implementation of `haveSeen'xyz'`.

### Exercise ?: An Alternative Representation of State Effects

Suppose we instead define the state effect as follows:
```Haskell
data State' s a
  = Pure a
  | Get (s -> State' s a)
  | Set s (State' s a)
```

1. Implement the fundamental state operations for `State' s`:
   ```Haskell
   pure' :: a -> State' s a
   bind' :: State' s a -> (a -> State' s b) -> State' s b
   get' :: State' s s
   set' :: s -> State' s ()
   ```

2. Show that `State` and `State'` are equivalent by implementing functions that
   interpret `State s` effects in terms of `State' s` effects and vice versa:
   ```Haskell
   interpretA :: State s a -> State' s a
   interpretB :: State' s a -> State s a
   ```
   **Hint**: What properties should `interpretA` and `interpretB` have? In
   particular, how should `get` and `get'` relate? What about `set` and `set'`?

### Summary

```Haskell
data State s a
pure :: a -> State s a
bind :: State s a -> (a -> State s b) -> State s b
get :: State s s
set :: s -> State s ()
```

## File I/O

Unlike failure and state, there's no standard encoding of file I/O effects, in
particular because there's no standard precise definition of file I/O. However,
for sake of example, let's encode basic file I/O effects as follows:
```Haskell
type FileName = String
type FileContents = String

data FileIO a
  = Pure a
  | Exists FileName (Bool -> FileIO a)
  | Read FileName (FileContents -> FileIO a)
  | Write FileName FileContents (FileIO a)
  | Delete FileName (FileIO a)
```
> **Haskell novices**: `type` declarations create *type aliases*.

Informally, we interpret a `FileIO a` computation as either
- performing no file I/O effects;
- checking if a file exists and passing the boolean result to a continuation;
- reading an existing file and passing its entire contents to a continuation;
- writing a string to a file (overwriting its contents if the file already
  exists) and proceeding with a continuation;
- deleting a file and proceeding with a continuation.
> **Haskell novices**: The Haskell community says "continuation" to mean "the
> thing to do next", usually denoted with a `k`. In the case of `FileIO`, the
> "thing to do next" is always essentially another `FileIO` computation.

From the definition of `FileIO`, we immediately derive a few core operations of
file I/O effects (including `pure`, an analogue to the `pure` operation for
`Fallible e` and `State s` effects):
```Haskell
pure :: a -> FileIO a
pure = Pure

exists :: FileName -> FileIO Bool
exists x = Exists x pure

read :: FileName -> FileIO FileContents
read x = Read x pure

write :: FileName -> FileContents -> FileIO ()
write x s = Write x s (pure ())

delete :: FileName -> FileIO ()
delete x = Delete x (pure ())
```
The latter four of these functions behave exactly like their constructor
counterpart, except without the ability to specify a continuation, so we lose
the ability to sequence file I/O computations. To see the problem here, consider
the following computation, which duplicates the contents of "`foo.txt`" if it
exists:
```Haskell
duplicateContents :: FileIO ()
duplicateContents = Exists "foo.txt" \case
    True -> Read "foo.txt" \s ->
        Write "foo.txt" (s <> s) (pure ())
    False -> pure ()
```
> **Haskell novices**: `<>` is string concatenation.

Now, try to write `duplicateContents` using `exists`, `read`, and `write` in
place of `Exists`, `Read`, and `Write`. You'll find we require an analogue of
`Fallible e` and `State s`'s `bind`:
```Haskell
bind :: FileIO a -> (a -> FileIO b) -> FileIO b
bind fa fb = case fa of
    Pure a -> fb a
    Exists x k -> Exists x \e -> bind (k e) fb
    Read x k -> Read x \s -> bind (k s) fb
    Write x s k -> Write x s (bind k fb)
    Delete x k -> Delete x (bind k fb)
```
Using `bind`, we can rewrite `duplicateContents` as follows:
```Haskell
duplicateContents = bind (exists "foo.txt") \case
    True -> bind (read "foo.txt") \s ->
        write "foo.txt" (s <> s)
    False -> pure ()
```

### Exercise ?: Simulating `FileIO` Effects

Suppose we're creating an application that performs `FileIO` effects. To test
the application, we want a stable/consistent testing environment. In particular,
we want to guarantee that `FileIO` effects behave deterministically, regardless
of the state of any external file system.

To this end, design types `FileError` and `FileSystem`, and write a function
`simulate` that interprets `FileIO` effects in terms of the `Fallible FileError`
and `State FileSystem` effects:
```Haskell
data FileError
data FileSystem
simulate :: FileIO a -> State FileSystem (Fallible FileError a)
```

**Remark**: This exercise demonstrates the strength of realizing effects as
ordinary values.

### Summary

```Haskell
data FileIO a
pure :: a -> FileIO a
bind :: FileIO a -> (a -> FileIO b) -> FileIO b
exists :: FileName -> FileIO Bool
read :: FileName -> FileIO FileContents
write :: FileName -> FileContents -> FileIO ()
delete :: FileName -> FileIO ()
```

# Monads: An Abstraction for Effects

{ TODO }

# Abstractions for Effects

The notions of effect we're interested in all have some common structure that we
abstract out. Abstraction here has a number of benefits:
- We can write code that is generic over effects of a certain class, preventing
  code duplication.
- Dealing with concrete effects in an ad hoc manner can obscure the essential
  properties of effectful code.
- It is illuminating to see how different effects can be classified.

The effect abstractions we cover (namely, in order of increasing power,
`Functor`, `Applicative`, `Monad`, and `Traversable`) were pioneered in a
practical functional programming setting by Haskell, and their utility is well
established. Roughly speaking, each of these abstractions is based around a
notion of *function application with a twist*, where the "twist" somehow
captures the effect.

## Functors

```Haskell
class Functor f where
   map :: (a -> b) -> f a -> f b
```
- **Identity**: `map id = id`
- **Composition**: `map (f . g) = map f . map g`

{ TODO: examples: data structures, `Identity`, `Const b` }

{ TODO: abstract exercises: define `flap` and other weird functions }

{ TODO: concrete exercises: ??? }

## Applicative Functors

```Haskell
class Functor f => Applicative f where
    pure :: a -> f a
    ap :: f (a -> b) -> f a -> f b
```
- **Identity**: ``pure id `ap` u = u``
- **Composition**: ``pure (.) `ap` u `ap` v `ap` w = u `ap` (v `ap` w)``
- **Homomorphism**: ``pure f `ap` pure x = pure (f x)``
- **Interchange**: ``u `ap` pure x = pure (\f -> f x) `ap` u``

### Exercise

```Haskell
class Functor f => Applicative' f where
    unit :: f ()
    cross :: f a -> f b -> f (a, b)
```
- **Left identity**: ``map snd (unit `cross` v) = v``
- **Right identity**: ``map fst (u `cross` unit) = u``
- **Associativity**: ``map assoc (u `cross` (v `cross` w)) = (u `cross` v) `cross` w``
{ TODO: Exercise: implement monoidal with applicative and vice versa }

{ TODO: examples: accumulating errors; context-free parsing? }

{ TODO: abstract exercises: define `fmap` from `pure` and `ap`, proving that
all applicatives are functors }

{ TODO: concrete examples: put examples from McBride and Paterson in a practical
context }

[McBride and Paterson](https://www.cambridge.org/core/journals/journal-of-functional-programming/article/applicative-programming-with-effects/C80616ACD5687ABDC86D2B341E83D298)

### Idiom Brackets

{ TODO: Emphasize that applicatives really are all about function application
with a twist (or with an "idiom", as McBride and Paterson say) }

## Monads

```Haskell
class Applicative f => Monad f where
    bind :: f a -> (a -> f b) -> f b
```
- **Left identity**: ``pure a `bind` k = k a``
- **Right identity**: ``m `bind` pure = m``
- **Associativity**: ``m `bind` (\x -> k x `bind` l) = (m `bind` k) `bind` l``

### Exercise

```Haskell
class Applicative f => Monad' f where
    join :: f (f a) -> f a
```
- **Left identity**: ``join . pure = id``
- **Right identity**: ``pure . join = id``
- **Associativity**: ``join . join = join . fmap join``

{ TODO: examples: short-circuiting errors }

{ TODO: abstract exercises: define `ap` form `pure` and `bind`, proving that
all monads are applicative functors }

{ TODO: concrete exercises: ??? }

[Wadler](https://homepages.inf.ed.ac.uk/wadler/papers/marktoberdorf/baastad.pdf)


```
"Hitler reacts to functional programming"
https://youtu.be/ADqLBc1vFwI
```

### Do Notation

{ TODO: demonstrate how bad indentation can get after a chain of `bind`s without
do notation }

## Traversable Functors

{ TODO: type class, laws }

{ TODO: examples: parsing }

# TODO

Combining effects (like monad transformers, but dumbed down a bit)

# References

[1] Richard Feldman, "The Essence of Functional Programming", [FnConf 2022](https://www.youtube.com/watch?v=l0ruvPCQh9I).

[2] Rich Hickey, "Simple Made Easy", [Strange Loop 2011](https://youtu.be/SxdOUGdseq4).

[3] Kris Jenkins, "Side-Effects Are The Complexity Iceberg", [YOW! 2024](https://www.youtube.com/watch?v=_nG09Z_tdUU).

[4] Simon Peyton Jones, "Haskell is Useless", [YouTube](https://youtu.be/iSmkqocn0oQ).

[5] Peter Landin, "The Next 700 Programming Languages", March 1966.
