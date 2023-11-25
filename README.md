# Approach
Solve problems using a rigorous step-by-step approach:
1. Construct a domain model (only relevant if there's a domain to model, which isn't the case for Leetcode questions).
2. Identify type signature and the transformation process of getting there (ex. input |> reduce |> reduce, input |> scan, etc.)
3. Fill out the transformations until you reach a solution.
4. Refactor to use standard library specializations of the transformations. (ex. Sum is a specialization of plus-reduce. Length is a specialization of count-reduce).

# Leetcode
For all problems, you can copy in any standard library function, ex. Scan or Iota. Focus on readability, not performance.

[Longest Substring](https://leetcode.com/problems/longest-substring-without-repeating-characters/)
Hint: you should use a library function not provided by JavaScript.

[Duplicate Zeros](https://leetcode.com/problems/duplicate-zeros/)
The problem says to mutate the input, so solve using a pure function and copy the results over:
```ts
const xs2 = duplicateZeros(xs)
for (let i=0; i < xs.length; i++) {
   xs[i] = xs2[i]
}
```

[Trapping Rainwater](https://leetcode.com/problems/trapping-rain-water/)
Hint: you need at least 2 passes over the array (forward and back). Remember that Scan returns a longer array than the input.

# Additional Problems
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
[-, -, -, -, +, +, +], p=0, p=1
[+, -, -, -, -, +, +], p=2
[+, -, -, -, -, +, +], p=3, p=4
[+, +, -, -, -, -, +], p=5, p=6
[+, +, +, -, -, -, -], p=7
```
