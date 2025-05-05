# Intro Video
> [!IMPORTANT]
> Watch the first 34 minutes
```
"C++ Seasoning -- Know Your Algorithms"
- Sean Parent, 2013
https://youtu.be/W2tWOdzgXHA
```

## Algorithm Classification
### Reduction
Ken Iverson coined `Reduce` in his 1962 book "A Programming Language", which introduced Iversonian Notation, later described in his Turing award paper "[Notation as a Tool of Thought](https://www.eecg.utoronto.ca/~jzhu/csc326/readings/iverson.pdf)". In the context of arrays, `Reduce` reduces one axis of an N-dimensional array, yielding an (N-1) dimensional array (or scalar). In general, `Reduce` reduces a foldable data structure to a summary value one element at a time.

|        | Direction     | Operator  | Initial Value | Type Signature            |
| ------ | ------------- | --------- | ------------- | ------------------------- |
| Reduce | Associative   | Monoid    | No            | (A → A → A) → A[] → A     |
| Seduce | Associative   | Semigroup | Yes           | (A → A → A) → A → A[] → A |
| FoldL  | Left-to-Right | Binary    | Yes           | (B → A → B) → B → A[] → B |
| FoldR  | Right-to-Left | Binary    | Yes           | (A → B → B) → B → A[] → B |

Language implementations of reduce/fold vary significantly, and many use a single function for every variation. For example, JavaScript and FSharp both throw if you don't provide an initial value for an empty list, unlike APL which infers the identity from the binary operator. More variations are possible: C++ has `fold_right_first` and `fold_left_first` which don't require an initial value, and `accumulate` with directionality defined by an iterator.

<details open>
   <summary>Reductions</summary>

|                | Type Signature                   |
| -------------- | -------------------------------- |
| Find           | (a → bool) → a[] → a Option      |
| Pick           | (a → b Option) → a[] → b Option  |
| Head           | [a, ...a] → a                    |
| Last           | [a, ...a] → a                    |
| HeadEmpty      | [a, ...a] → a Option             |
| LastEmpty      | [a, ...a] → a Option             |
| Count          | a[] -> int                       |
| Sum            | float[] -> float                 |

</details>

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

### Transform
<details open>
   <summary>Transforms preserve the dimension of array.</summary>
   
|               | Type Signature                   | Length | Composition           | Subcategory | Parallelization |
| ------------- | -------------------------------- | ------ | --------------------- | ----------- | --------------- |
| Map           | (a → b) → a[] → b[]              | n      |                       | Map         | 1               |
| Choose        | (a → b Option) → a[] → b[]       | <= n   |                       | Selection   | 1               |
| Filter        | (a → bool) → a[] → a[]           | <= n   |                       | Selection   | 1               |
| FilterMap     | (a → bool) → (a → b) → a[] → b[] | <= n   | Filter >> Map         | Map         | 1               |
| MapFilter     | (a → b) → (b → bool) → a[] → b[] | <= n   | Map >> Filter         | Map         | 1               |
| Scan          | (s * t → s) → s → t[] → s[]      | n + 1  |                       | Scan        | log(n)          |
| ScanInclusive | (s * t → s) → s → t[] → s[]      | n      | Scan >> Skip 1        | Scan        | log(n)          |
| ScanExclusive | (s * t → s) → s → t[] → s[]      | n      | Scan >> Take n        | Scan        | log(n)          |
| Rotate        | int → a[] → a[]                  | n      |                       | Permutation |                 |
| Sort          | (a * a → -1\|0\|1) → a[] → a[]   | n      |                       | Permutation |                 |
| Dedupe        | a[] → a[]                        | <= n   |                       | Selection   |                 |
| Intersperse   | a → a[] → a[]                    | >= n   |                       |             |                 |
| Intercalate   | a[] → a[][] → a[]                | >= n   | Intersperse >> Concat |             |                 |
| Partition     | (a → bool) → a[] → (a[] * a[])   |        |                       | Group       |                 |
| SplitAt       | int → (a[] * a[])                |        |                       | Group       |                 |

</details>

### Specialization
Projection is a common specialization, denoted by `<name>By`. For example:
```
// Less Specialized
users |> Array.Find (fun x -> x.Name = "Bob")

// More Specialized
users |> Array.FindBy _.Name (= "Bob")
```

Always use the most specialized form of an algorithm possible. There are exceptions to this rule, ex. `+/` in APL is already quite terse, so defining `Sum` wouldn't make sense. Function specializations usually form a tree, ex. this figure for [Mismatch](https://en.cppreference.com/w/cpp/algorithm/mismatch)

<img src="/Media/ConorHoekstra--specializations.png" height="150px">

# Algorithm Names
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

# Array Rank
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

# Exercises
For part 1, watch the videos before solving the problems. For other parts, solve the problems first. For all problems, you can copy in any standard library function, ex. Scan or Iota. Focus on readability, not performance.

## Exercises Part 1
```
"Functional vs Array Programming"
- Conor Hoekstra (Code Report), 2021
https://youtu.be/UogkQ67d0nY

"APL vs BQN vs J vs Q vs NumPy vs Julia vs R"
- Conor Hoekstra (Code Report), 2022
https://youtu.be/8ynsN4nJxzU
```

### Problem 1
We have several teams of people, each defined as a list of team member names. Write code that returns the size of the largest team.
```
// example 1
input: [["alice"], ["bob", "charlie"]]
output: 2

// example 2:
input [["alice", "bob", "charlie"]]
output: 3
```

### Problem 2
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

### Problem 3
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

### Problem 4 [Longest Substring](https://leetcode.com/problems/longest-substring-without-repeating-characters/)
Hint: you should use a library function not provided by JavaScript.

### Problem 5 [Duplicate Zeros](https://leetcode.com/problems/duplicate-zeros/)
The problem says to mutate the input, so solve using a pure function and copy the results over:
```ts
const xs2 = duplicateZeros(xs)
for (let i=0; i < xs.length; i++) {
   xs[i] = xs2[i]
}
```

## Exercises Part 2
### Problem 1 [Trapping Rainwater](https://leetcode.com/problems/trapping-rain-water/)
Hint: you need at least 2 passes over the array (forward and back). Remember that Scan returns a longer array than the input.

### Problem 2 Dirty Input Sum
Sum the numbers in an array while ignoring non-numeric elements. What algorithm solves this with memory complexity of O(1)?

```
// example
input: [1, 2, "text"]
output: 3
```

### Problem 3 Count Matched
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

```
"Algorithm Intuition"
- Conor Hoekstra (Code Report), CppCon 2019
https://youtu.be/pUEnO6SvAMo (part 1)
https://youtu.be/sEvYmb3eKsw (part 2)
```

## Exercises Part 3
### Problem 1 [973. K Closest Points to Origin](https://leetcode.com/problems/k-closest-points-to-origin/description/)

### Problem 2 [977. Squares of a Sorted Array](https://leetcode.com/problems/squares-of-a-sorted-array)

### Problem 3 Element Repeated Once (Variation of [961.](https://leetcode.com/problems/n-repeated-element-in-size-2n-array))
In an array of size N, there are N-1 unique values. Return the duplicated value.

```
// example
input: [2,1,4,5,3,2]
output: 2
```

### Problem 4 [917. Reverse Only Letters](https://leetcode.com/problems/reverse-only-letters)

```
"Better Algorithm Intuition"
- Conor Hoekstra (Code Report), Meeting C++ 2019
https://youtu.be/TSZzvo4htTQ
```
