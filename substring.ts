const scan = <T, U>(
    list: T[],
    accumulator: (acc: U, item: T) => U,
    initialAcc: U
  ): U[] => {
    const results: U[] = [];
    let acc = initialAcc;
  
    for (const item of list) {
      acc = accumulator(acc, item);
      results.push(acc);
    }
  
    return results;
  };
  
const lengthOfLongestSubstring = (s: string): number => {
const charIndexMap: { [char: string]: number } = {};

return Math.max(
    ...scan(Array.from(s), (substring, char) => {
    const startIndex = charIndexMap.hasOwnProperty(char) ? charIndexMap[char] + 1 : 0;
    charIndexMap[char] = substring.length;
    return substring.slice(startIndex) + char;
    }, '')
    .map((substring) => substring.length)
);
};