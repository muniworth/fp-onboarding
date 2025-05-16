# Definition of Functional Programming

In 1966, Peter Landin coined the term "Functional Programming" (FP) to describe
the language ISWIM [4]. However, he didn't provide a definition for FP, leaving
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
is the very definition of complexity [1].

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
> -- Simon Peyton Jones [3]

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

- Richard Feldman, "The Essence of Functional Programming", FnConf 2022.
  [Link](https://youtu.be/l0ruvPCQh9I).

- (Optional) Richard Feldman, "The Next Paradigm Shift in Programming", ETE 2020.
  [Link](https://youtu.be/6YbK8o9rZfI)


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
> quantifies over each variable within that begins with a lowercase letter
> (`a` and `e` in this case), so we could equivalently write
> ```Haskell
> pure :: forall a e. a -> Fallible e a
> ```
> Line (2) defines `pure`.

> [!NOTE]
> Here and elsewhere, we declare functions that clash with standard functions
> exported from the Haskell Prelude. To avoid "ambiguous occurrence" errors
> when referring to such declarations, hide the Prelude versions:
> ```Haskell
> import Prelude hiding (pure, fail) -- Add more to this list as needed.
> ```

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
practice of abstracting out duplicate code, let's write a function `(>>=)`
(pronounced "bind") that automatically propagates errors, allowing us to focus
on the success case:
```Haskell
(>>=) :: Fallible e a -> (a -> Fallible e b) -> Fallible e b
fa >>= fb = case fa of
    Success a -> fb a
    Failure e -> fail e
```
> **Haskell novices**: Functions with a symbolic name, like "`>>=`", use infix
> notation.
>
> Wrap an infix function in parentheses to use prefix notation. For example,
> `x >>= y` is equivalent to `(>>=) x y`.

Intuitively, `(>>=)` sequences two fallible computations, where the second
computation can depend on the output value of the first computation, such that
the composite computation fails if either input computation fails.

With the help of `(>>=)`, `uncons4` simplifies to the following:
<a name="fallible-disturbing-indentation"></a>
```Haskell
uncons4 xs0 =
    uncons xs0 >>= \(x0, xs1) ->
        uncons xs1 >>= \(x1, xs2) ->
            uncons xs2 >>= \(x2, xs3) ->
                uncons xs3 >>= \(x3, xs4) ->
                    pure (x0, x1, x2, x3, xs4)
```
Much better, although we still must endure the excessive indentation (for now).

### Exercise: `catch`

Implement the following function for catching errors:
```Haskell
catch :: Fallible e a -> (e -> Fallible f a) -> Fallible f a
```
(In part, this exercise asks you to figure out what `catch` should do based on
its type signature.)

### Exercise: The Duality of Success and Failure

Let `dual` be the following function:
```Haskell
dual :: Fallible e a -> Fallible a e
dual = \case
    Success a -> Failure a
    Failure e -> Success e
```
Use `dual` to equationally relate `(>>=)` and `catch`. That is, using only
`dual` and `(>>=)`, implement `catch`, and using only `dual` and `catch`,
implement `(>>=)`.

### Summary

```Haskell
data Fallible e a
pure :: a -> Fallible e a
(>>=) :: Fallible e a -> (a -> Fallible e b) -> Fallible e b
fail :: e -> Fallible e a
catch :: Fallible e a -> (e -> Fallible f a) -> Fallible f a
```

## State

We encode the effect of state of type `s` as a function from an initial state
value to an output value paired with a final state value:
```Haskell
data State s a = State (s -> (a, s))
```

Like `Fallible e`, `State s` has a pure fragment containing computations that
don't really get or set the state, and we enter it with `pure`:
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
practice of abstracting out duplicate code, let's write a function `(>>=)` that
automatically threads state:
```Haskell
(>>=) :: State s a -> (a -> State s b) -> State s b
sa >>= sb = State \s ->
    case runState sa s of
        (a, s') -> runState (sb a) s'
```
Intuitively, `(>>=)` sequences two stateful computations, where the second
computation can depend on the output value of the first computation.

With the help of `(>>=)`, `rand4` simplifies to the following:
<a name="state-disturbing-indentation"></a>
```Haskell
rand4 =
    rand >>= \x0 ->
        rand >>= \x1 ->
            rand >>= \x2 ->
                rand >>= \x3 ->
                    pure (x0, x1, x2, x3)
```
Much better, although we still must endure the excessive indentation (for now).

### Exercise: `get` and `set`

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
transition nextChar = get >>= \currState ->
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
haveSeen'xyz' = get >>= \case
    Seen'xyz' -> pure True
    _ -> pure False
```
With these two helper functions, we can implement a stateful version of
`contains'xyz'`:
```Haskell
statefulContains'xyz' :: String -> State Q Bool
statefulContains'xyz' = \case
    [] -> haveSeen'xyz'
    c:cs -> transition c >>= \() -> statefulContains'xyz' cs
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

### Exercise: `map`

In the implementation of `haveSeen'xyz'`, the second computation of the `(>>=)`
is always pure. Write a function
```Haskell
map :: (a -> b) -> State s a -> State s b
```
and use it to simplify the implementation of `haveSeen'xyz'`.

### Exercise: An Alternative Representation of State Effects

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
(>>=) :: State s a -> (a -> State s b) -> State s b
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
> **Haskell novices**: "Continuation" means "the thing to do next", usually
> denoted with a `k`. In the case of `FileIO`, the "thing to do next" is always
> essentially another `FileIO` computation.

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
the following computation, which duplicates the contents of a given file if it
exists:
```Haskell
duplicateContents :: FileName -> FileIO ()
duplicateContents x = Exists x \case
    True -> Read x \s ->
        Write x (s <> s) (pure ())
    False -> pure ()
```
> **Haskell novices**: `<>` is string concatenation.

Now, try to write `duplicateContents` using `exists`, `read`, and `write` in
place of `Exists`, `Read`, and `Write`. You'll find we require an analogue of
`Fallible e` and `State s`'s `(>>=)`:
```Haskell
(>>=) :: FileIO a -> (a -> FileIO b) -> FileIO b
fa >>= fb = case fa of
    Pure a -> fb a
    Exists x k -> Exists x \e -> k e >>= fb
    Read x k -> Read x \s -> k s >>= fb
    Write x s k -> Write x s (k >>= fb)
    Delete x k -> Delete x (k >>= fb)
```
Using `(>>=)`, we can rewrite `duplicateContents` as follows:
```Haskell
duplicateContents = exists x >>= \case
    True -> read x >>= \s ->
        write x (s <> s)
    False -> pure ()
```

### Exercise: Simulating `FileIO` Effects

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
(>>=) :: FileIO a -> (a -> FileIO b) -> FileIO b
type FileName = String
type FileContents = String
exists :: FileName -> FileIO Bool
read :: FileName -> FileIO FileContents
write :: FileName -> FileContents -> FileIO ()
delete :: FileName -> FileIO ()
```


# Monads: An Abstraction for Effects

We've now seen three different concrete effects, all of the following form:
```Haskell
data M a
pure :: a -> M a
(>>=) :: M a -> (a -> M b) -> M b
-- (Plus additional operations specific to the concrete effect.)
```
Let's abstract out this common structure into a type class:
```Haskell
class Monad m where
    pure :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b
```
> **Haskell novices**: A *type class* is a collection of methods that types can
> implement, similar to traits in Rust or inferfaces in Java. Type classes have
> nothing to do with classes in the object-oriented sense.
>
> Note that `Monad` describes behavior for a *type-level function* `m`, not a
> (proper) type. In the official lingo, `m` has kind `* -> *`. ***Kinds*** are
> the "types" of types-level entities. Most programming languages only support
> *proper* types, like `Int` and `String`, which have kind `*` in Haskell, but
> Haskell also supports so-called *higher-kinds*, which just means kinds that
> contain an arrow `->`. We've seen a few concrete examples of higher-kinded
> types already, namely `Fallible, State :: * -> * -> *` and `FileIO :: * -> *`.
>
> Despite the explicit signatures in the type class declaration of `Monad`,
> `pure` and `(>>=)` really have the following types:
> ```Haskell
> pure :: Monad m => a -> m a
> (>>=) :: Monad m => m a -> (a -> m b) -> m b
> ```
> The "`Monad m => ...`" part of the signatures is a *(type class) constraint*.
> In general, a constraint `C a` demands that `a` implements the type class `C`,
> licensing access to the methods of `C` within the scope of the constraint.

We implement `Monad` for `Fallible e`, `State s`, and `FileIO` by copying the
definition of `pure` and `(>>=)` from the previous section:
```Haskell
instance Monad (Fallible e) where
    pure = ...
    (>>=) = ...

instance Monad (State s) where
    pure = ...
    (>>=) = ...

instance Monad FileIO where
    pure = ...
    (>>=) = ...
```
> **Haskell novices**: Like with functions, instance declarations implicitly
> quantify over all variables beginning with a lowercase letter that appear
> in the implementing type. So here we really declare a `Monad` instance for
> `Fallible e` *for each `e`*, and likewise for `State s`.

## Laws

Although Haskell can't enforce it, all monads should satisfy a few laws:

- **Left identity**: for all `a` and `k`, `pure a >>= k  =  k a`
- **Right identity**: for all `m`, `m >>= pure  =  m`
- **Associativity**: for all `m`, `k`, and `l`, `m >>= (\x -> k x >>= l)  =  (m >>= k) >>= l`

Don't focus too much on the details. Essentially, the two identity laws say that
`pure` really creates pure computations, in the sense that computations created
with `pure` don't add any effects when sequenced with any other computation,
and the associativity law says that sequencing of effects is associative.

### Exercise (Optional): Lawfulness of `Fallible e`, `State s`, and `FileIO`

Prove that the `Monad` instances for `Fallible e`, `State s`, and `FileIO`
satisfy the monads laws.

--------------------------------------------------------------------------------

By abstracting out the concept of a monad, we gain the ability to create
computations that work for *all* monads. For example, we can write a
monad-generic flipped version of `(>>=)` (which sometimes improves readability
compared to the regular `(>>=)`):
```Haskell
(=<<) :: Monad m => (a -> m b) -> m a -> m b
(=<<) = flip (>>=)
```
> **Haskell novices**: `flip :: (a -> b -> c) -> (b -> a -> c)` flips the
> argument order of a curried binary function.

The following exercises ask you to implement a couple more particularly useful
effect-polymorphic functions.

### Exercise: "Semicolon"

Create a operator `(>>)` (pronounced "then") that sequences two monadic
computations, ignoring the result of the first computation and returning the
result of the second, analogous to semicolons in imperative programming
languages:
```Haskell
(>>) :: Monad m => m a -> m b -> m b
```

### Exercise: Kleisli Composition

Recall that left-to-right function composition has type
```Haskell
(a -> b) -> (b -> c) -> (a -> c)
```
> **Haskell novices**: The `Control.Arrow` module provides this function
> composition operation as an infix operator `(>>>)`.

Create a version of function composition that augments each function involved
with effects from some monad:
```Haskell
(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
```

## `do` Notation

> Pure functional languages have this advantage: all flow of data is made
> explicit. And this disadvantage: sometimes it is painfully explicit.
>
> -- Philip Wadler [6]

When sequencing many monadic computations with bind, we have to indent once
per bind, leading to horrific expressions of the following form:
```Haskell
m1 >>= \p1 ->
    m2 >>= \p2 ->
        m3 >>= \p3 ->
            ...
                mk >>= \pk -> m
```
Haskell (and, nowadays, many other functional languages following Haskell's
lead in providing ergonomic support for monadic effects) has a feature, called
*`do` notation*, to flatten such expressions to the following:
```Haskell
do
    p1 <- m1
    p2 <- m2
    p3 <- m3
    ...
    pk <- mk
    m
```
As an additional notational convenience, if we don't need some `pi`, we can
write just "`mi`" instead of "`pi <- mi`".

> [!NOTE]
> By default, Haskell desugars `do` notation using the `(>>=)` and `(>>)`
> methods of the `Prelude.Monad` type class. To run the following examples
> involving `do` notation, you'll need to either
> - provide instances for `Prelude.Monad` instead of our own `Monad` class
>   (which involves concepts we haven't covered yet), or
> - enable GHC's
>   [RebindableSyntax](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/rebindable_syntax.html)
>   extension to make GHC desugar `do` notation using whatever operators named
>   `(>>=)` and `(>>)` happen to be in scope.

### Example: Cleaning Up `uncons4` and `rand4`

Recall the following disturbing code samples from our investigation of
[`Fallible e`](#fallible-disturbing-indentation) and
[`State s`](#state-disturbing-indentation):
```Haskell
uncons4 :: [a] -> Fallible UnconsError (a, a, a, a, [a])
uncons4 xs0 =
    uncons xs0 >>= \(x0, xs1) ->
        uncons xs1 >>= \(x1, xs2) ->
            uncons xs2 >>= \(x2, xs3) ->
                uncons xs3 >>= \(x3, xs4) ->
                    pure (x0, x1, x2, x3, xs4)

rand4 :: State Word64 (Word64, Word64, Word64, Word64)
rand4 =
    rand >>= \x0 ->
        rand >>= \x1 ->
            rand >>= \x2 ->
                rand >>= \x3 ->
                    pure (x0, x1, x2, x3)
```
With `do` notation, these functions improve significantly:
```Haskell
uncons4 xs0 = do
    (x0, xs1) <- uncons xs0
    (x1, xs2) <- uncons xs1
    (x2, xs3) <- uncons xs2
    (x3, xs4) <- uncons xs3
    pure (x0, x1, x2, x3, xs4)

rand4 = do
    x0 <- rand
    x1 <- rand
    x2 <- rand
    x3 <- rand
    pure (x0, x1, x2, x3)
```
How pleasant!

## The Identity Monad: Pure Computation

When investigating a new concept X, it's often a good idea to ask if there are
any trivial instances of X. In the case of monads, we have the identity monad:
```Haskell
data Identity a = Identity a

runIdentity :: Identity a -> a
runIdentity (Identity x) = x

instance Monad Identity where
    pure = Identity
    Identity x >>= k = k x
```

What use does the identity monad have? For one, it can help cure code of the
disease know as "readability":
```Haskell
f x  =  runIdentity $ pure . f =<< pure x
```
More seriously, the identity monad comes into play with *monad transformers*,
which exceed our scope.

## The List Monad: Nondeterministic Computation

Lists (or arrays) have a `Monad` instance with an interesting interpretation as
nondeterministic computations. Specifically, we interpret a list as a single
nondeterministic value. For example, we view `[1, 2, 3]` as a single value
equal to either `1`, `2`, or `3`. To compute with such a value, we apply bind
(which has type `[a] -> (a -> [b]) -> [b]`) to "fork" computation by mapping
each case (`1`, `2`, `3`) to a new nondeterministic value. We can picture it
like this:
```
                    1                2                3
                  / | \                              / \
                 4  5  6                            7   8
```
In this case, `1` branches to `4`, `5`, or `6` (nondeterministically); `2`
branches to nothing (the branch "dies"); and `3` branches to `7` or `8`
(nondeterministically). Thus, the output value is `4`, `5`, `6`, `7`, or `8`,
a nondeterministic value realized as the list `[4, 5, 6, 7, 8]`.

That explains the behavior of `(>>=)` for the list `Monad`, but what about
`pure`? For any monadic effect, `pure` should represent the case of trivial
effects, so for the effect of nondeterminism, `pure` ought to produce
*deterministic* computations, i.e., singleton lists.
```Haskell
instance Monad [] where
    pure x = [x]

    [] >>= _ = []
    (x:xs) >>= k = k x ++ (xs >>= k)
```
> **Haskell novices**: As a type, `[] :: * -> *` denotes the list type
> constructor, not to be confused with "`[]`" as a term, which denotes the nil
> constructor of lists.

For example, here's a nondeterministic function for flipping a coin:
```Haskell
data CoinFlip = Heads | Tails deriving Eq

flip :: [CoinFlip]
flip = [Heads, Tails]
```
> **Haskell novices**: `deriving Eq` instructs Haskell to automatically
> implement the `Eq` type class, enabling equality comparison.

We can use `flip` and the monadic structure of lists to write a nondeterministic
computation that finds all possible ways of flipping three coins and getting
two or more heads:
```Haskell
-- The empty list, but with a more suggestive name.
die :: [a]
die = []

flip3With2OrMoreHeads :: [(CoinFlip, CoinFlip, CoinFlip)]
flip3With2OrMoreHeads = do
    x <- flip
    y <- flip
    z <- flip
    if count (== Heads) [x, y, z] >= 2 then
        pure (x, y, z)
    else
        die
```
Notice how the `do` notation makes it look like we really are flipping three
coins nondeterministically!

As expected,
```Haskell
flip3With2OrMoreHeads  =  [(Heads,Heads,Heads),(Heads,Heads,Tails),(Heads,Tails,Heads),(Tails,Heads,Heads)]
```

### Exercise: Nondeterministic Flush

Let's model ordered poker hands as follows:
```Haskell
data Value = Ace | N2 | N3 | N4 | N5 | N6 | N7 | N8 | N9 | N10 | Jack | Queen | King deriving Eq
data Suit = Spades | Hearts | Clubs | Diamonds deriving Eq
data Card = Card Value Suit deriving Eq
data Hand = Hand Card Card Card Card Card
```
Write a nondeterministic computation that outputs all flushes (i.e., `Hand`s
with all five cards of the same suit) obtainable in a game of poker (meaning the
cards in a hand must be distinct).
```Haskell
flushes :: [Hand]
```


# Applicative Functors: A Weaker Abstraction for Effects

We previously saw how the `Fallible e` monad provides short-circuiting error
handling. That is, as soon as one error occurs, `Fallible e` aborts the rest of
the computation and returns that error. This is not some incidental feature of
`Fallible` that we can easily change. To see why, recall the definition of
`Fallible` and the type signature of its bind operation:
```Haskell
data Fallible e a = Success a | Failure e
(>>=) :: Fallible e a -> (a -> Fallible e b) -> Fallible e b
```
Imagine we sequence a failed computation with some continuation
`k :: a -> Fallible e b` by forming the computation `Failure e >>= k`. What can
this composite computation possibly do? To do anything useful with `k`, we need
to feed `k` something of type `a`, but we don't have an `a`; we only have an
`e`. Therefore, the composite computation must evaluate to `Failure e`; the
types forbid any other behavior.

However, what if we want to accumulate errors throughout a computation, without
ever short-circuiting? We often want this notion of failure when validating
some sort of user input, so we can alert the user of *all* input errors. The
type that encodes this effect exactly matches `Fallible`:
```Haskell
data Validation e a = Success a | Failure e
```
The similarity with `Fallible` stops there, however. As argued above,
`Validation e` has no `Monad` instance (with the intended behavior). Instead,
`Validation e` implements the weaker effect interface of  ***applicative
functors***:
```Haskell
class Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b
```
Just like monads, applicative functors have a notion of pure computation created
with `pure`. Applicative functors also have a binary operation `(<*>)`
(pronounced "ap") that performs function application "inside" the applicative
functor.

We can implement `Applicative` for `Validation e`, but only for semigroups `e`,
because we need some way of combining errors:
```Haskell
instance Semigroup e => Applicative (Validation e) where
    pure = Success

    Success f <*> Success a  = Success (f a)
    Success _ <*> Failure e  = Failure e
    Failure e <*> Success _  = Failure e
    Failure e <*> Failure e' = Failure (e <> e') -- The main difference with Fallible e
```
> **Haskell novices**: `Semigroup` is the type class
> ```Haskell
> class Semigroup a where
>     (<>) :: a -> a -> a
> ```
> where `(<>)` must be associative.

For example, suppose we want to parse a string of bits (`'0'` or `'1'`) to a
list of integers (`0` or `1`, respectively). Of course, the string could contain
characters besides `'0'` and `'1'`, so let's use the `Validation [String]`
applicative functor to collect string error messages lamenting each invalid
character. That is, we take `[String]` as our type of errors, whose semigroup
operation is `(++)`, list concatenation.

First, let's write a function to parse a single bit:
```Haskell
parseBit :: Char -> Validation [String] Int
parseBit = \case
    '0' -> pure 0
    '1' -> pure 1
    c   -> Failure ["Oh no! Expected '0' or '1', but got '" ++ [c] ++ "'."]
```
Next, we map `parseBit` over the entire input string. We can not directly cons
the results of each call to `parseBit` together *outside* the applicative
functor; instead we "lift" `(:)` into the applicative functor (with `pure`) to
cons *inside*:
```Haskell
parseBits :: String -> Validation [String] [Int]
parseBits = \case
    c:cs -> pure (:) <*> parseBit c <*> parseBits cs
    [] -> pure []
```
For instance:
```Haskell
parseBits "0101"  =  Success [0,1,0,1]
parseBits "0x11"  =  Failure ["Oh no! Expected '0' or '1', but got 'x'."]
parseBits "foo0"  =  Failure [
    "Oh no! Expected '0' or '1', but got 'f'.",
    "Oh no! Expected '0' or '1', but got 'o'.",
    "Oh no! Expected '0' or '1', but got 'o'."
]
```

## Laws

Like monads, applicative functors should satisfy a few laws:

- **Identity**: for all `u`, `pure id <*> u  =  u`
  (i.e., `id u  =  u` in the applicative functor)
- **Composition**: for all `u`, `v`, and `w`, `pure (.) <*> u <*> v <*> w  =  u <*> (v <*> w)`
  (i.e., `(u . v) w  =  (.) u v w  =  u (v w)` in the applicative functor)
- **Homomorphism**: for all `f` and `x`, `pure f <*> pure x  =  pure (f x)`
  (i.e., application of pure computations is pure application)
- **Interchange**: for all `u` and `x`, `u <*> pure x  =  pure (\f -> f x) <*> u`
  (i.e., `pure` really creates pure computations, in the sense that pure
  computations commute with any other computation)

### Exercise (Optional): Lawfulness of `Validation e`

Prove that the `Applicative` instance for `Validation e` satisfies the
applicative functor laws.

--------------------------------------------------------------------------------

### Exercise: Monads are Applicative Functors

Implement `(<*>)` using `pure` and `(>>=)`.

**Optional**: Also, prove that the monad laws imply the applicative functor
laws.

**Remark**: The exercise demonstrates that all monads are applicative functors,
but we saw the converse fails.
> The moral is this: if you have got an `Applicative` functor, that is good; if
> you have also got a `Monad`, that is even better! And the dual of the moral is
> this: if you need a `Monad`, that is fine; if you need only an `Applicative`
> functor, that is even better!
>
> -- Conor McBride and Ross Paterson [5]

### Exercise: Success is not an Option

Let's define a variant of `Validation` without `Success`. By convention, we call
it `Const`:
```Haskell
data Const b a = Const b
```
Implement `Applicative` for `Const b` when `b` is a `Monoid`:
```Haskell
instance Monoid b => Applicative (Const b) where
    ...
```
> **Haskell novices**: `Monoid` is the type class
> ```Haskell
> class Semigroup a => Monoid a where
>     mempty :: a
> ```
> where `mempty` must be a left and right unit for `(<>)`.

Does the `Applicative` instance of `Const b` ever extend to a `Monad` instance?
If so, what is it? If not, why not?

### Exercise: Applicative Functors Compose

Let `Compose f g` be the composition of two type-level functions
`f, g :: * -> *`:
```Haskell
data Compose f g a = Compose (f (g a))
```
Show that the composition of two applicative functors is an applicative functor:
```Haskell
instance (Applicative f, Applicative g) => Applicative (Compose f g) where
    ...
```

Does the `Applicative` instance of `Compose f g` ever extend to a `Monad`
instance? If so, what is it? If not, why not?

### Hitler's Take on FP

"Hitler reacts to functional programming". [Link](https://youtu.be/ADqLBc1vFw).


# Functors

As we saw back in the `contains'xyz'` example with the `haveSeen'xyz'` action,
sometimes we want to change the *value* of a computation without touching the
*effects*. We even created a function that does this for the `State s` effect,
namely `map`:
```Haskell
map :: (a -> b) -> State s a -> State s b
```
As we will see, `map`-like operations are very useful, and whenever we find a
useful abstraction, we often benefit from codifying it as a type class. In this
case, we define `Functor`:
```Haskell
class Functor f where
   (<$>) :: (a -> b) -> f a -> f b
```
Following the pattern of `Monad` and `Applicative`, we assign map a symbolic
name "`<$>`", but we can also define an alphabetic alias:
```Haskell
map :: Functor f => (a -> b) -> f a -> f b
map = (<$>)
```

Informally, `Functor`s represent containers of values, and mapping a function
`g :: a -> b` over an element `x :: f a` of a functor (i.e., `g <$> x`) changes
the value(s) inside the container without affecting the structure of the
container. We capture this idea with the following laws:
- **Identity**: `map id  =  id`
- **Composition**: for all `f` and `g`, `map (f . g)  =  map f . map g`

### Exercise: Applicative Functors are Functors

Implement `(<$>)` using `pure` and `<*>`.

**Optional**: Also, prove that the applicative functor laws imply the functor
laws, proving that applicative functors (in particular, monads) are functors.


# References

[1] Rich Hickey, "Simple Made Easy", Strange Loop 2011.
[Link](https://youtu.be/SxdOUGdseq4).

[2] Kris Jenkins, "Side-Effects Are The Complexity Iceberg", YOW! 2024.
[Link](https://youtu.be/_nG09Z_tdUU).

[3] Simon Peyton Jones, "Haskell is Useless".
[Link](https://youtu.be/iSmkqocn0oQ).

[4] Peter Landin, "The Next 700 Programming Languages", 1966.
[Link](https://web.archive.org/web/20250514174928/https://www.cs.cmu.edu/~crary/819-f09/Landin66.pdf).

[5] Conor McBride and Ross Paterson, "Applicative programming with effects", 2008.
[Link](https://web.archive.org/web/20230306110258/https://www.cambridge.org/core/services/aop-cambridge-core/content/view/C80616ACD5687ABDC86D2B341E83D298/S0956796807006326a.pdf/applicative-programming-with-effects.pdf).

[6] Philip Wadler, "Monads for functional programming", 1995.
[Link](https://web.archive.org/web/20250330183238/https://homepages.inf.ed.ac.uk/wadler/papers/marktoberdorf/baastad.pdf).


# TODO

## Monads

### Imperative Programming

***Being in a monad is the essence of imperative programming.***

Point out how `do` notation looks like a sequence of imperative actions
(because it is).

### The Reader Monad: Computation in a Read-Only Context

### Interpreting Effects?

Monad morphisms

### Combining Effects?

Like monad transformers, but dumbed down a bit.

## Applicative Functors

### Exercise/Example: ZipList

### Example: Context-Free Parsing

## Functors

Data structures examples

### Exercise ?: A Symmetric Representation of Applicative Functors

```Haskell
class Functor f => Applicative' f where
    unit :: f ()
    cross :: f a -> f b -> f (a, b)
```
- **Left identity**: ``map snd (unit `cross` v) = v``
- **Right identity**: ``map fst (u `cross` unit) = u``
- **Associativity**: ``map assoc (u `cross` (v `cross` w)) = (u `cross` v) `cross` w``

### Exercise ?: A Symmetric Representation of Monads

```Haskell
class Applicative f => Monad' f where
    join :: f (f a) -> f a
```
- **Left identity**: ``join . pure = id``
- **Right identity**: ``pure . join = id``
- **Associativity**: ``join . join = join . map join``


## Traversable Functors

Type class and laws

Example: parsing


## `Functor`, `Applicative`, `Monad`: Function Application With a Twist

```Haskell
  ($) ::                    (a ->   b) ->   a ->   b
(<$>) :: Functor f     =>   (a ->   b) -> f a -> f b
(<*>) :: Applicative f => f (a ->   b) -> f a -> f b
(=<<) :: Monad f       =>   (a -> f b) -> f a -> f b
```

### Exercise: `flap`

### Exercise: `flop`

Come up with some operation `flop` involving two functors `f` and `g` and ask to
deduce the requisite type classes on `f` and `g`.
