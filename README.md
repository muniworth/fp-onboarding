## Approach
Most developers learn to solve problems by declaring temporary variables and for loops, an anti-pattern Venkat Subramaniam calls [Primitive Obession](https://youtu.be/znZlF4uQBN0?t=1047). It doesn't allow for progress when you get stuck on a difficult problem, and results in unreadable, buggy code.

Instead, solve problems using a rigorous step-by-step approach:
1. Construct a domain model (only relevant if there's a domain to model, which isn't the case for Leetcode questions).
2. Specify type signature.
3. Write the type transformation pipeline (expand, transform, reduce)
4. Solve the problem one step at a time by implementing transformations.
5. Specialize the transformations using standard library functions. ex. Sum is a specialization of plus-reduce. Length is a specialization of count-reduce.

Using a rigorous approach increases the likelihood of writing maintainable code.

## Example
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

## [Algorithm Intuition](1_Algorithm_Intuition.md)

## [Domain Driven Design](2_Domain_Driven_Design.md)

## [Functional Programming](3_Functional_Programming.md)

## [Hexagonal Architecture](#hexagonal-architecture)
```
"Functional architecture - The pits of success"
- Mark Seemann, NDC Sydney 2016
https://youtu.be/US8QG9I1XW0

"From Dependency injection to dependency rejection"
- Mark Seemann, NDC London 2017
https://youtu.be/cxs7oLGrxQ4
```

## [Combinators](#combinators)
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
