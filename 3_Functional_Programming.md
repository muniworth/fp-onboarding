# Definition
In 1966, Peter Landin coined the term "Functional Programming" to describe the language ISWIM. However, he didn't provide a definition for FP, leaving the term open for debate.

Most attempts at defining functional programming reference language features, such as closures and immutable state, but no language feature is essential to FP. Functional programming needs **compositionality**, meaning the ability to reason about programs by breaking them into understandable components. **Side effects prevent composition.** Kris Jenkins[1] and Richard Feldman[2] agree functional programming means managing effects to achieve composability.

For example, take the simple program in figure 1, where `t` and `u` are arbitrary
subprograms. To understand this program, we'd like to apply the principle of
compositionality by (1) understanding `t` (independently of `u`), (2)
understanding `u` (independently of `t`), and (3) understanding what it means
to add two things together. Functional languages seek to admit this logic (as
much as possible).

Figure 1
```
// Program
t() + u()

// Subprogram v1 - Composable
t = () -> 2
u = () -> 3

// Subprogram v2 - Not composable due to side effect
t = () -> x := 3; 2
u = () -> x
```

By contrast, subprogram v2 `u` cannot be understood independently of `t`, so the whole program `t + u` can only be understood as a whole. Interconnectedness of large components in large programs quickly becomes unmanageable. The inapplicability of the principle of compositionality is the [definition of complexity](https://youtu.be/SxdOUGdseq4).

Functional programming is a cult:
- Programmers who don't value composition don't understand why FP matters.
- It's difficult to explain why FP is valuable.
- Once you see the value of composition, you never want to leave.

We drink the coolaid. The value of compositional reasoning can only be learned through experience, but we now take it for granted and explore its consequences.

```
"The Essence of Functional Programming"
- Richard Feldman, FnConf 2022
https://www.youtube.com/watch?v=l0ruvPCQh9I

// Also worth watching
"The Next Paradigm Shift in Programming"
- Richard Feldman, 2020
https://youtu.be/6YbK8o9rZfI
```

# Effects

We saw above how state can preclude compositional reasoning, but state is not
uniquely problematic in this regard. More generally, we must carefully manage
all effects to program functionally.

Vaguely speaking, an ***effect*** is any aspect of a computation that can be
viewed as an interaction with the ambient execution environment. (The "execution
environment" is merely a conceptual device; it doesn't necessarily have some
cohesive existence during the execution of a program.) Here are some common
examples of effects:
- **State**: The environment records the current value of the state, and
  programs interact with the environment by getting and setting the state. In
  the above example program `t + u`, where `t` = `x := 3; 2` and `u` = `x`, when
  `t` executes, it interacts with the environment by setting `x` to `3`, and
  when `u` executes, it interacts with the environment by getting `x`.
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
[useless](https://youtu.be/iSmkqocn0oQ):

> In the end, a program with no effect, there's no point in running it, is
> there? You have this black box and you press go and it gets hot, but there's
> no output. Why did you run the program? The reason to run a program is to have
> an effect.
>
> -- Simon Peyton Jones

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

# Encoding Effects

We encode effects as type-level functions `f` that send a type `a` of "pure
values" to a type `f a` of "effectful values". Let's see how this plays out on
the three kinds of effects from above (don't worry too much about the details
of the specific effects here; just observe the pattern):

- **State**: We encode the effect of state of type `s` as follows:
  ```Haskell
  data State s a = State (s -> (a, s))
  --   Inital state value ^        ^ Final state value
  ```
  { TODO: Explain. }

- **Exceptions**: We encode the effect of exceptions with error detail of type
  `e` as follows:
  ```Haskell
  data Except e a = Ok a | Error e
  ```
  { TODO: Explain. }

- **File I/O**: Unlike state and exceptions, there is no standard encoding of
  file I/O effects, but we could encode basic file I/O effects as follows:
  ```Haskell
  data FileIO a
      = Pure a -- No/trivial effects
      | Open String (FileHandle -> FileIO a) -- Like "with" in Python
      | Read FileHandle (String -> FileIO a) -- Read entire file
      | Write FileHandle String (FileIO a) -- Overwrite entire file
  ```
  { TODO: Explain. }

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

{ TODO: type class, laws }

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

### Do Notation

{ TODO: demonstrate how bad indentation can get after a chain of `bind`s without
do notation }

## Interlude: Semigroups, Monoids, and Foldable Data Structures

{ TODO: type classes, laws }

{ TODO: exercises: rank 1 and rank 2 matrix reductions }

## Traversable Functors

{ TODO: type class, laws }

{ TODO: examples: parsing }

### References
[1] "Side-Effects Are The Complexity Iceberg", Kris Jenkins, [YOW! 2024](https://www.youtube.com/watch?v=_nG09Z_tdUU)
[2] "The Essence of Functional Programming", Richard Feldman, [FnConf 2022](https://www.youtube.com/watch?v=l0ruvPCQh9I)
