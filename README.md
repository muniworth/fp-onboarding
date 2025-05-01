## Approach
Most developers learn to solve problems by declaring temporary variables and for loops, an anti-pattern Venkat Subramaniam calls [Primitive Obession](https://youtu.be/znZlF4uQBN0?t=1047). It doesn't allow for progress when you get stuck on a difficult problem, and results in unreadable, buggy code.

Instead, solve problems using a rigorous step-by-step approach:
1. Construct a domain model (only if there's a domain to model, which isn't the case for Leetcode).
2. Specify type signature.
3. Write a pipeline that implements the type signature (expand, transform, reduce).
4. Solve the problem one step at a time by writing callbacks within the pipeline.
5. Specialize the algorithms using standard library functions. ex. Sum is a specialization of plus-reduce. Length is a specialization of count-reduce.

Using a rigorous approach increases the likelihood of writing maintainable code.

## Example
Given a table of numbers, create a total column.
```
Step 1
empty, since there are no domain types to model.

Step 2
number[][] -> number[]

Step 3
input |> reduce rank2

Step 4
input |> Array.map (Array.reduce (+))

Step 5
input |> Array.map Array.sum
```

1 - [Algorithm Intuition](1_Algorithm_Intuition.md)

2 - [Domain Driven Design](2_Domain_Driven_Design.md)

3 - [Functional Programming](3_Functional_Programming.md)

## 4 - Onion Architecture
aliases: pure architecture, hexagonal architecture, ports and adapters
```
"Functional architecture - The pits of success"
- Mark Seemann, NDC Sydney 2016
https://youtu.be/US8QG9I1XW0

"From Dependency injection to dependency rejection"
- Mark Seemann, NDC London 2017
https://youtu.be/cxs7oLGrxQ4
```

## Useful but out of scope
### Combinators
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
Question: How does S relate to ap?

### Database Normalization
```
"Learn Database Normalization - 1NF, 2NF, 3NF, 4NF, 5NF"
by Decomplexify, 2022
https://www.youtube.com/watch?v=GFQaEYEc8_8

"Learn Database Denormalization"
by Decomplexify, 2023
https://www.youtube.com/watch?v=4bTq0GdSeQs
```
