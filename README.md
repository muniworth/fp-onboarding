[Functional Programming](#functional-programming)

[Algorithm Intuition](#algorithm-intuition)

[Onion Architecture](#onion-architecture)

[Domain Driven Design](#domain-driven-design)

[And Beyond](#and-beyond)

# Functional Programming
[Functional Programming](3_Functional_Programming.md)
TODO inline

# Algorithm Intuition
Most developers solve problems by declaring temporary variables, an anti-pattern Venkat Subramaniam calls [Primitive Obession](https://youtu.be/znZlF4uQBN0?t=1047), then writing logic step-by-step inside loops. It's easier to break a problem into composable sub-problems if you follow a rigorous step-by-step approach:
1. Specify type signature.
2. Write a pipeline that implements the type signature (expand, transform, reduce).
3. Solve the problem one step at a time by writing callbacks within the pipeline.
4. Specialize the algorithms using standard library functions. ex. Sum is a specialization of plus-reduce. Length is a specialization of count-reduce.

## Example
Given a UI table, create a total column.
```
Step 1
number[][] -> number[]

Step 2
input |> reduce rank2

Step 3
input |> Array.map (Array.reduce (+))

Step 4
input |> Array.map Array.sum
```

## Intro Video
> [!IMPORTANT]
> Watch the first 34 minutes
```
"C++ Seasoning -- Know Your Algorithms"
- Sean Parent, 2013
https://youtu.be/W2tWOdzgXHA
```

## Algorithm Classification
### Transform
<details open>
   <summary>Transforms preserve the dimension of array.</summary>

|               | Type Signature                   | Len | Composition           | Subcategory | Parallel |
| ------------- | -------------------------------- | --- | --------------------- | ----------- | -------- |
| Map           | (a → b) → a[] → b[]              | n   |                       | Map         | 1        |
| Bind          | (a -> b[]) -> a[] -> b[]         | ≥ n | Map >> Flat           |             |          |
| Choose        | (a → b Option) → a[] → b[]       | ≤ n |                       | Selection   | 1        |
| Filter        | (a → bool) → a[] → a[]           | ≤ n | Choose Option.SomeIf  | Selection   | 1        |
| FilterMap     | (a → bool) → (a → b) → a[] → b[] | ≤ n | Filter >> Map         | Map         | 1        |
| MapFilter     | (a → b) → (b → bool) → a[] → b[] | ≤ n | Map >> Filter         | Map         | 1        |
| ScanL         | (s * t → s) → s → t[] → s[]      | n+1 |                       | Scan        | log(n)   |
| ScanInclusive | (s * t → s) → s → t[] → s[]      | n   | ScanL >> Skip 1       | Scan        | log(n)   |
| ScanExclusive | (s * t → s) → s → t[] → s[]      | n   | ScanL >> Take n       | Scan        | log(n)   |
| MapAccumL     | (s\*a → s\*b) → s → a[] → b[] * s  | n   |                       | Scan        | log(n)   |
| Rotate        | int → a[] → a[]                  | n   | SplitAt >> Flip Union | Permutation |          |
| Sort          | (a * a → -1\|0\|1) → a[] → a[]   | n   |                       | Permutation |          |
| Dedupe        | a[] → a[]                        | ≤ n |                       | Selection   |          |
| Intersperse   | a → a[] → a[]                    | ≥ n |                       |             |          |
| Partition     | (a → bool) → a[] → (a[] * a[])   |     | GroupBy Predicate     | Group       |          |
| SplitAt       | int → (a[] * a[])                |     |                       | Group       |          |

</details>

### Transform Exercises
#### [1089. Duplicate Zeros](https://leetcode.com/problems/duplicate-zeros/)
The problem says to mutate the input, so if solving on Leetcode, use a pure function and copy the results over:
```ts
const xs2 = duplicateZeros(xs)
for (let i=0; i < xs.length; i++) {
   xs[i] = xs2[i]
}
```

#### Charged Particle Movement
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

#### Robot Cleaner
A room exists on a cartesian plane, with dirty areas represented as points on the plane. Pathfind a robot cleaner through a room, starting from point `[1, 1]`. Move to points in the order given.
1. Output 'W' or 'E' for horizontal movement
2. Output 'N' or 'S' for vertical movement
3. Output 'C' when the robot cleans.
4. Output a space between each character.
```
sample input text:
`[2, 4]
[1, 3]
[4, 1]
[3, 2]
`
output:
`E N N N C W S C E E E S S C W N C`
```

### Reduction
Ken Iverson coined `Reduce` in his 1962 book "A Programming Language", which introduced Iversonian Notation and inspired [APL](https://aplwiki.com/wiki/Simple_examples). APL is the subject of his Turing award paper "[Notation as a Tool of Thought](https://www.eecg.utoronto.ca/~jzhu/csc326/readings/iverson.pdf)". In the context of arrays, `Reduce` reduces one axis of an N-dimensional array, yielding an (N-1) dimensional array (or scalar). For now we'll only consider arrays, but in general, `Reduce` reduces a foldable data structure to a summary value one element at a time. We classify reductions by the binary function used, called a *reducer* or *folder*.

|        | Direction     | Operator  | Initial Value | Type Signature                  |
| ------ | ------------- | --------- | ------------- | ------------------------------- |
| Reduce | Associative   | Monoid    | No            | (A → A → A) → A[] → A           |
| Seduce | Associative   | Semigroup | No            | (A → A → A) → non-empty A[] → A |
| FoldL  | Left-to-Right | Binary    | Yes           | (B → A → B) → B → A[] → B       |
| FoldR  | Right-to-Left | Binary    | Yes           | (A → B → B) → B → A[] → B       |

A **semigroup** is pairing of a data type with an associative binary operator on that type.

```haskell
-- Def'n of associativity for an operator/function 'op'.
-- This must hold true for a type and operator to form a semigroup.
op (op x y) z = op x (op y z)-- prefix notation
(x `op` y) `op` z = x `op` (y `op` z)-- infix notation

typeclass Semigroup a where
    op :: a -> a -> a

-- Integers form a semigroup under the operation of maximum
instance Semigroup Int where
    op = max
```

A **monoid** is a semigroup with an identity value.

```haskell
id `op` x = x-- Left Identity
x `op` id = x-- Right Identity

-- Integers form a monoid under the operation of multiply with an identity of 0
typeclass Monoid a extends Semigroup a where
    unit :: a
    op :: a -> a -> a

instance Monoid Int extends Semigroup Int where
    unit = 0
    op = multiply

-- Strings form a monoid under the operation of union with an identity of ""
instance Monoid String extends Semigroup String where
    unit = ""
    op = union
```

Language implementations of reduce/fold vary significantly, and many use a single function for every variation. For example, JavaScript and FSharp both throw if you don't provide an initial value for an empty list, unlike APL which infers the identity from the binary operator. More variations are possible: C++ has `fold_right_first` and `fold_left_first` which don't require an initial value, and `accumulate` with directionality defined by an iterator.

<details open>
   <summary>Reductions</summary>

|                | Type Signature                   | Composition             |
| -------------- | -------------------------------- | ----------------------- |
| Find           | (a → bool) → a[] → a Option      | Filter >> HeadEmpty     |
| Pick           | (a → b Option) → a[] → b Option  | Choose >> HeadEmpty     |
| Catenate       | a[][] -> a[]                     | Bind Id                 |
| Intercalate    | a[] → a[][] → a[]                | Intersperse >> Catenate |
| Head           | a::a[] → a                       |                         |
| Last           | a::a[] → a                       |                         |
| HeadEmpty      | a[] → a Option                   |                         |
| LastEmpty      | a[] → a Option                   |                         |
| Count          | a[] -> int                       | Foldr (K (+1)) 0        |
| Sum            | float[] -> float                 | Reduce (+)              |

</details>

<details open>
   <summary>Array Rank</summary>

When working with multi-dimensional arrays, it's useful to know the leading axis convention. We count arrays from the outside in, and apply functions across a specific rank. In [array languages]((https://mlochbaum.github.io/BQN/doc/leading.html)) that follow this convention, omitting the rank modifier defaults to rank 1.
|               | Col 1 | Col 2 | Col 3 | Rank 2 Sum | Rank 2 Length |
| ------------- | ----- | ----- | ----- | ---------- | ------------- |
| Row 1         | 1     | 2     | 3     | 6          | 3             |
| Row 2         | 1     | 2     | 3     | 6          | 3             |
| Rank 1 Sum    | 2     | 4     | 6     |            |               |
| Rank 1 Length | 2     | 2     | 2     |            |               |

For functions that operate on arrays (ex. length), each increase in rank adds one additional Map before calling the function:
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

`SumRank1` is equivalent to `Transpose >> SumRank2`.

</details>

### Reduction Exercises
#### Largest Team
We have several teams of people, each defined as a list of team member names. Write code that returns the size of the largest team.
```
// example 1
input: [["alice"], ["bob", "charlie"]]
output: 2

// example 2:
input [["alice", "bob", "charlie"]]
output: 3
```

#### [3. Longest Substring](https://leetcode.com/problems/longest-substring-without-repeating-characters/)
<details>
    <summary>**Hint**</summary>
    Try Scan then Reduce.
</details>

#### [42. Trapping Rainwater](https://leetcode.com/problems/trapping-rain-water/)
<details>
    <summary>**Hint**</summary>
    You need at least 2 passes over the array (forward and back). Remember that Scan returns a longer array than the input.
</details>

#### [2016. Maximum Difference Between Increasing Elements](https://leetcode.com/problems/maximum-difference-between-increasing-elements/description/)

#### Reduction Videos
```
"Functional vs Array Programming"
- Conor Hoekstra (Code Report), 2021
https://youtu.be/UogkQ67d0nY

"APL vs BQN vs J vs Q vs NumPy vs Julia vs R"
- Conor Hoekstra (Code Report), 2022
https://youtu.be/8ynsN4nJxzU
```

### Expansion / Unfold
<details open>
   <summary>Expansions are the opposite of reductions.</summary>

|              | Type Signature            |
| ------------ | ------------------------- |
| Init         | int → (int → a) → a[]     |
| Iota         | int → int → int[]         |
| OuterProduct | a[] → b[] → (a * b)[][]   |
| Range        | int → int → int[]         |
| Replicate    | int → a → a[]             |

</details>

### Expansion Exercises
#### Change on Fiscal Month
We make 1 dollar per month, increasing to 2 dollars per month starting in March. Given a fiscal year start month (numeric, 1-indexed) as input, return an array of our income for each month.
```
// example 1
input 1// January
output [1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2]

// example 2
input: 3// March
output: [2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2]

// example 3
input: 4// April
output: [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2]
```

## Specialization
Projection is a common specialization, denoted by `<name>By`. For example:
```
// Less Specialized
users |> Array.Find (fun x -> x.Name = "Bob")

// More Specialized
users |> Array.FindBy _.Name (= "Bob")
```

Always use the most specialized form of an algorithm possible. There are exceptions to this rule, ex. `+/` in APL is already quite terse, so defining `Sum` wouldn't make sense. Function specializations usually form a tree, ex. this figure for [Mismatch](https://en.cppreference.com/w/cpp/algorithm/mismatch)

<img src="/Media/ConorHoekstra--specializations.png" height="150px">

## Algorithm Names
### Sampling of algorithm names across languages
```
"Consistently Inconsistent"
- Conor Hoekstra (Code Report), Meeting C++ 2019
https://youtu.be/tsfaE-eDusg
```

### Hoogle Translate for Map
<img src="/Media/ConorHoekstra--hoogle_translate_fmap.png" height="350px">

- Called `.Select` in C Sharp (LINQ).
- Called `.map` in JavaScript.

### The 3 Main Higher Order Functions
<img src="/Media/ConorHoekstra--3_main_higher_order_functions.jpg" height="500px">


## Function Fusion
#### Fusion Exercise 1 - Dirty Input Sum
Sum the numbers in an array while ignoring non-numeric elements. What algorithm solves this with memory complexity of O(1)?

```
// example
input: [1, 2, "text"]
output: 3
```

#### Fusion Exercise 2 - Count Matched
Given two strings, count the number of characters that exactly match between them. What algorithm solves this with memory complexity of O(1)?

```
// example 1
input 1: "abcd"
input 2: "abzd"
output: 3

// example 2
input 1: "abcd"
input 2: "abdc"
output: 2
```

#### Fusion Videos 1
```
"Algorithm Intuition"
- Conor Hoekstra (Code Report), CppCon 2019
https://youtu.be/pUEnO6SvAMo (part 1)
https://youtu.be/sEvYmb3eKsw (part 2)
```

#### Fusion Exercise 3
[973. K Closest Points to Origin](https://leetcode.com/problems/k-closest-points-to-origin/description/)

#### Fusion Exercise 4
[977. Squares of a Sorted Array](https://leetcode.com/problems/squares-of-a-sorted-array)

#### Fusion Exercise 5 - Element Repeated Once
(Variation of [961.](https://leetcode.com/problems/n-repeated-element-in-size-2n-array))
In an array of size N, there are N-1 unique values. Return the duplicated value.
What computer science theorem guarantees a unique solution to this problem?

```
// example
input: [2,1,4,5,3,2]
output: 2
```

#### Fusion Exercise 6
[917. Reverse Only Letters](https://leetcode.com/problems/reverse-only-letters)

#### Fusion Videos 2
```
"Better Algorithm Intuition"
- Conor Hoekstra (Code Report), Meeting C++ 2019
https://youtu.be/TSZzvo4htTQ
```

# Onion Architecture
aliases: pure architecture, hexagonal architecture, ports and adapters
```
"Functional architecture - The pits of success"
- Mark Seemann, NDC Sydney 2016
https://youtu.be/US8QG9I1XW0

"From Dependency injection to dependency rejection"
- Mark Seemann, NDC London 2017
https://youtu.be/cxs7oLGrxQ4
```

# Domain Driven Design
Hide primitive types within implementation details.
```
UserId = Guid |> Brand "User"
OrganizationId = Guid |> Brand "Organization"
```

Non-domain types are fully abstract.
```
Option 'T =
   | Some 'T
   | None
```

Domain types are readable by a domain expert. Keep them as semantic as possible.
```
User =
   Name: string
   Email: EmailAddress
   Roles: Set<Role>
```

Schemas include constraints without a language requirement for dependent types.
```
SparseOpExRow =
   Variability: Percent |> Clamp(0.0, 1.0)
   Cells: Dict<Year, OpExCell>
```

```
"Getting rid of Option with Sum Types - Is Maybe an Option"
- The Dev Owl, 2020
Skip part 1 if you're familiar with Option/Maybe
Part 2 https://youtu.be/x5RA9gYPhnc
Part 3 https://youtu.be/YGqEEREMA0k
```




# And Beyond
Out of scope for FP onboarding, but good to know.
## Combinators
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

## Database Normalization
```
"Learn Database Normalization - 1NF, 2NF, 3NF, 4NF, 5NF"
by Decomplexify, 2022
https://www.youtube.com/watch?v=GFQaEYEc8_8

"Learn Database Denormalization"
by Decomplexify, 2023
https://www.youtube.com/watch?v=4bTq0GdSeQs
```
