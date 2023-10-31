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

[Trapping Rainwater](https://leetcode.com/problems/trapping-rain-water/submissions/)
Hint: you need at least 2 passes over the array (forward and back). Remember that Scan returns a longer array than the input.

# Theory
Implement a lightweight Option in TypeScript:
- Some
- None
- DefaultOf
- Map
- Exists
- Filter
- Flat
- Bind
- Iter

It does not need to support higher kinded types. This allows you to partially apply the function arguments and pipe the option.

```ts
// Expect no output
Option.Iter(a => console.log(a), Option.None())

// Expect output 3
console.log(Pipe.F2(
   Option.Some(2),
   Option.Map(a => a + 1),
   Option.DefaultOf(2),
))
```

Implement a function that partially applies all arguments to Iter, allowing you to call it later with no arguments.

