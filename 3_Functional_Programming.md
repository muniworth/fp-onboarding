# Definition of Functional Programming

In 1966, Peter Landin coined the term "Functional Programming" (FP) to describe
the language ISWIM [?]. However, he didn't provide a definition for FP, leaving
the term open for debate. Most attempts at defining functional programming
resort to referencing particular language features, such as closures and lack of
mutable state, but no language feature is essential to FP. Rather, functional
programming builds around one core tenet, **compositionality**, the ability to
reason about programs by breaking them into components that can in turn be
understood independently.

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

Functional programming is a cult:
- Programmers who don't value composition don't understand why FP matters.
- It's difficult to explain why FP is valuable.
- Once you see the value of composition, you never want to leave.

We drink the coolaid. The value of compositional reasoning can only be learned through experience, but we now take it for granted and explore its consequences.

# Effects

We saw above how state can preclude compositional reasoning, but state is not
uniquely problematic in this regard. More generally, we must carefully manage
all effects to program functionally.

Vaguely speaking, an ***effect*** is any aspect of a computation that can be
viewed as an interaction with the ambient execution environment. (The "execution
environment" is merely a conceptual device; it doesn't necessarily have some
cohesive existence during the execution of a program.) Common effects include
the following:
- **State**: The environment records the current value of the state, and
  programs interact with the environment by getting and setting the state. In
  the previous example with `t = (x := 3; 2)` and `u = x`, the computation `t`
  interacts with the environment by setting `x` to `3`, and `u` interacts with
  the environment by getting `x`.
- **Exceptions**: Programs interact with their execution environment by asking
  it to abort execution, often with some detail for why execution must abort
  (e.g., `throw error("Something broke!")`).
- **File I/O**: The environment has access to a file system, and programs
  interact with their environment by asking it to open/close, read from, and
  write to files.

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
values" to a type `f a` of "effectful values". Let's see how this plays out on
the three kinds of effects from above. Don't worry too much about the details
of the specific effects here; just observe the pattern.

### State

We encode the effect of state with type `s` as follows:
```Haskell
data State s a = State (s -> (a, s))
--   Inital state value ^        ^ Final state value
```
That is, a `State s a` computation is a function from an initial state value
to an output value paired with a final state value.

#### Example

{ TODO: finite state machine (parsing) example }

#### Exercise

Implement the following:
```Haskell
-- Return the given value, leaving the state unchanged.
pure :: a -> State s a

-- Return the current state, leaving the state unchanged.
get :: State s s

-- Set the state and return the unit.
set :: s -> State s ()
```

#### Exercise

Suppose we instead define the state effect as follows:
```Haskell
data State' s a
  = Pure a -- No/trivial effects
  | Get (s -> State' s a)
  | Set s (State' s a)
```
Implement a function that interprets `State'` as `State` "reasonably", with the
following type signature:
```Haskell
run :: State' s a -> State s a
```

<details>
    <summary>**Hint**</summary>
    You should first determine what "resonable" should mean.
</details>

### Exceptions

We encode the effect of exceptions with type `e` as follows:
```Haskell
data Except e a = Success a | Failure e
```
That is, an `Except e a` computation either succeeds, returning a value of type
`a`, or it fails, returning an error of type `e`.

#### Exercise

Implement the following function for catching errors:
```Haskell
catch :: Except e a -> (e -> Except e a) -> Except e a
```

### File I/O

Unlike state and exceptions, there is no standard encoding of file I/O effects,
but we could encode basic file I/O effects as follows:
```Haskell
data FileIO a
  = Pure a -- No/trivial effects
  | Open String (FileHandle -> FileIO a) -- Like "with open..." in Python
  | Read FileHandle (String -> FileIO a) -- Read entire file
  | Write FileHandle String (FileIO a) -- Overwrite entire file
```
We interpret a `FileIO a` computation as either
- performing no file I/O effects;
- opening a file by name, creating a file handle and passing it to a
  continuation;
- reading the file with a given handle, passing the contents to a continuation;
- writing a string to the file with a given handle, then proceeding with a
  continuation.

For example, here's a computation that opens "foo.txt" and duplicates its
contents (`<>` is string concatenation):
```Haskell
example :: FileIO ()
example = Open "foo.txt" (\h -> Read h (\s -> Write h (s <> s) (Pure ())))
```

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
   map :: forall a b. (a -> b) -> f a -> f b
```

{ TODO: laws }

{ TODO: examples: data structures, `Identity`, `Const b` }

{ TODO: abstract exercises: define `flap` and other weird functions }

{ TODO: concrete exercises: ??? }

## Applicative Functors

{ TODO: type class, laws }

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

{ TODO: type class, laws }

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

## Interlude: Semigroups, Monoids, and Foldable Data Structures

{ TODO: type classes, laws }

{ TODO: exercises: rank 1 and rank 2 matrix reductions }

## Traversable Functors

{ TODO: type class, laws }

{ TODO: examples: parsing }

# References

[1] Richard Feldman, "The Essence of Functional Programming", [FnConf 2022](https://www.youtube.com/watch?v=l0ruvPCQh9I).

[2] Rich Hickey, "Simple Made Easy", [Strange Loop 2011](https://youtu.be/SxdOUGdseq4).

[3] Kris Jenkins, "Side-Effects Are The Complexity Iceberg", [YOW! 2024](https://www.youtube.com/watch?v=_nG09Z_tdUU).

[4] Simon Peyton Jones, "Haskell is Useless", [YouTube](https://youtu.be/iSmkqocn0oQ).

[5] Peter Landin, "The Next 700 Programming Languages", March 1966.
