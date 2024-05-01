# Contents
[Approach](#approach)

[Example](#example)

[Related problems](#related-problems)

[Other Useful Videos](#other-useful-videos)

# Approach
Most developers learn to solve problems by declaring temporary variables and for loops, an antipattern Venkat Subramaniam calls [Primitive Obession](https://youtu.be/znZlF4uQBN0?t=1047). It doesn't allow for progress when you get stuck on a difficult problem, and results in unreadable, buggy code.

Instead, solve problems using a rigorous step-by-step approach:
1. Construct a domain model (only relevant if there's a domain to model, which isn't the case for Leetcode questions).
2. Specify type signature.
3. Write the type transformation pipeline (ex. reduce, scan, etc.)
4. Solve the problem one step at a time by implementing transformations.
5. Specialize the transformations using standard library functions. ex. Sum is a specialization of plus-reduce. Length is a specialization of count-reduce.

Using a rigorous approach increases the likelihood of writing maintainable code.

# Example
Given a table of numbers, create a total column.
```
Step 1
empty, since there are no domain types to model.

Step 2
number[][] -> number[]

Step 3
input
|> reduce rank2

Step 4
input
|> Array.map (fun cs -> Array.reduce (+) cs)

Step 5
input
|> Array.map Array.sum
```

# Array Rank
When working with multi-dimensional arrays, it's useful to know the leading axis convention. We count arrays from the outside in, and apply functions across a specific rank. In [array languages]((https://mlochbaum.github.io/BQN/doc/leading.html)) that follow this convention, omitting the rank modifier defaults to rank 1.
|               | Col 1 | Col 2 | Col 3 | Rank 2 Sum | Rank 2 Length |
| ------------- | ----- | ----- | ----- | ---------- | ------------- |
| Row 1         | 1     | 2     | 3     | 6          | 3             |
| Row 2         | 1     | 2     | 3     | 6          | 3             |
| Rank 1 Sum    | 2     | 4     | 6     |            |               |
| Rank 1 Length | 2     | 2     | 2     |            |               |

For functions that operate on arrays (ex. length), each increase in rank adds one additional FMap before calling the function:
```
// rank 1
input |> Array.length
// rank 2
input |> Array.map Array.length
```

When acting on cells, you can index across them (for brevity this code assumes a rectangular, non-empty array).
```
// rank 1
input.[0] |> Array.mapi (fun i _ -> input |> Array.map (Array.item i) |> Array.sum)
// rank 2
input |> Array.map Array.sum
```

Note that `SumRank1` is equivalent to `Transpose >> SumRank2`.

### Hoogle Translate for FMap
Called `Select` in C Sharp (LINQ).
<img src="/Media/ConorHoekstra--hoogle_translate_fmap.png" height="350px">

### The 3 Main Higher Order Functions
<img src="/Media/ConorHoekstra--3_main_higher_order_functions.jpg" height="500px">

### Variations of Reduce
<img src="/Media/ConorHoekstra--reductions.png" height="350px">

### Sampling of algorithm names across languages
```
"Consistently Inconsistent"
- Conor Hoekstra (Code Report), Meeting C++ 2019
https://youtu.be/tsfaE-eDusg
```

### TypeScript Examples
```
// Expansions (also called unfolds)
// Iota is a specialization for generating ranges
// https://aplwiki.com/wiki/Index_Generator
const years = Iota(yearsCount, yearStart)
const xsMinutes = Iota((24 * 60) / bucketSize, 0).map(t => t * bucketSize)

// Code to render a total row from two subtotals.
Zip2With(RevTotalsSos1, RevTotalsSos2, Add).map(TdNum(F.MCurrency))

// From code that converts usage tiers into running charges
const usageStarts = [0, thresh, ...c.Tiers.map(t => t.UsageOffset + thresh)]
const usageEnds = [...usageStarts.slice(1), Number.POSITIVE_INFINITY]
const rates = [c.TierRateFirst, ...c.Tiers.map(t => t.Rate), c.TierRateLast]
const runningCharges = Scan(
   Zip3(usageStarts, usageEnds, rates),
   (s, [us, ue, r]) => s + (ue - us) * r,
   0,
)

// If a function doesn't exist (ex. Clamp), create it!
// Non-generic functions should arrange parameter order for partial application
const scaleUnclamped = r.AdvScaleCharge ?? c.ScaleMinimum
const min = c.IsMinimum ? c.ScaleMinimum : Number.NEGATIVE_INFINITY
const max = c.IsMaximum ? c.ScaleMaximum : Number.POSITIVE_INFINITY
const scale = Clamp(min, max, scaleUnclamped)
const charge = c.Charge * scale
```

# Algorithm Intuition part 1
```
*** Watch the first 34 minutes ***
"C++ Seasoning -- Know Your Algorithms"
- Sean Parent, 2013
https://youtu.be/W2tWOdzgXHA

"Functional vs Array Programming"
- Conor Hoekstra (Code Report), 2021
https://youtu.be/UogkQ67d0nY

"APL vs BQN vs J vs Q vs NumPy vs Julia vs R"
- Conor Hoekstra (Code Report), 2022
https://youtu.be/8ynsN4nJxzU
```

### Related problems
For all problems, you can copy in any standard library function, ex. Scan or Iota. Focus on readability, not performance.

### 1
We have several teams of people, each defined as a list of team member names. Write code that returns the size of the largest team.
```
example 1
input: [["alice"], ["bob", "charlie"]]
output: 2

example 2:
input [["alice", "bob", "charlie"]]
output: 3
```

### 2
We make 1 dollar per month, increasing to 2 dollars per month starting in March. Given a fiscal year start month (numeric, 1-indexed) as input, return an array of our income for each month.
```
example 1
input 1// January
output [1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2]

example 2
input: 3// March
output: [2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2]

example 3
input: 4// April
output: [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2]
```

### 3
A list contains 2 types of particles: positive '+' or negative '-'. We select one index 'p' as the anchor point. Write code to gather all negative charges around the anchor point, while pushing the positive charges away. Charges may not cross over the anchor point. Your solution should be linear in time complexity.
```
example input list:
[-, +, -, -, +, -, +]
output for each index:
[-, -, -, -, +, +, +], p=0
[-, -, -, -, +, +, +], p=1
[+, -, -, -, -, +, +], p=2
[+, -, -, -, -, +, +], p=3
[+, -, -, -, -, +, +], p=4
[+, +, -, -, -, -, +], p=5
[+, +, -, -, -, -, +], p=6
[+, +, +, -, -, -, -], p=7
```

### 4
[Longest Substring](https://leetcode.com/problems/longest-substring-without-repeating-characters/)
Hint: you should use a library function not provided by JavaScript.

### 5
[Duplicate Zeros](https://leetcode.com/problems/duplicate-zeros/)
The problem says to mutate the input, so solve using a pure function and copy the results over:
```ts
const xs2 = duplicateZeros(xs)
for (let i=0; i < xs.length; i++) {
   xs[i] = xs2[i]
}
```

# Algorithm Intuition part 2
```
"Algorithm Intuition"
- Conor Hoekstra (Code Report), CppCon 2019
https://youtu.be/pUEnO6SvAMo (part 1)
https://youtu.be/sEvYmb3eKsw (part 2)
```

### Related problems
### 1
[Trapping Rainwater](https://leetcode.com/problems/trapping-rain-water/)
Hint: you need at least 2 passes over the array (forward and back). Remember that Scan returns a longer array than the input.

### 2
Given two strings, count the number of characters that exactly match between them. There are several elegant solutions, so aim for a memory complexity of O(1).

# Algorithm Intuition part 3
```
"Better Algorithm Intuition"
- Conor Hoekstra (Code Report), Meeting C++ 2019
https://youtu.be/TSZzvo4htTQ
```

TODO leetcode from the above video

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

### Functors and Monads
Definition 1 (loosely taken from Richard Feldman in the podcast Software Unscripted):
A monad is a `.then()`-able, i.e. a chainable data structure that takes callbacks

Definition 2 (from Mark Seemann):
A monad is a functor that can be flattened. A functor is a container that you can map over (i.e. inject transform logic into).

### [Bind](https://fsharpforfunandprofit.com/posts/elevated-world-2/#bind)
aliases: flatMap, andThen, collect, SelectMany

Equivalent to `FMap >> Flat`

### [Lift](https://fsharpforfunandprofit.com/posts/elevated-world/#return)
aliases: return, pure, unit, yield, point

Not to be confused with [Lift](https://fsharpforfunandprofit.com/posts/elevated-world/#lift), an alias for FMap. FSharp calls Lift 'return'.

```
"An Introduction to Functors in Javascript"
- Ijemma Onwuzulike, 2020
https://youtu.be/XcM39gnqgNc

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

### Combinators (advanced)
```
Combinators and when to use point-free style
"Point-Free or Die: Tacit Programming in Haskell and Beyond"
by Amar Shah, Strange Loop 2016
https://youtu.be/seVSlKazsNk

"Function Composition in Programming Languages"
- Conor Hoekstra (Code Report), CppNorth 2023
https://youtu.be/JELcdZLre3s
```
Question: How does Blackbird relate to Inner Product?

# Other Useful Videos
### Hexagonal Architecture:
```
"Functional architecture - The pits of success"
- Mark Seemann, NDC Sydney 2016
https://youtu.be/US8QG9I1XW0

"From Dependency injection to dependency rejection"
- Mark Seemann, NDC London 2017
https://youtu.be/cxs7oLGrxQ4
```

### Domain Driven Design:
```
"Getting rid of Option with Sum Types - Is Maybe an Option"
- The Dev Owl, 2020
Skip part 1 if you're familiar with Option/Maybe
Part 2 https://youtu.be/x5RA9gYPhnc
Part 3 https://youtu.be/YGqEEREMA0k
```
