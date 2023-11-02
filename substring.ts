function lengthOfLongestSubstring(s) {
    if (s.length === 0) {
      return 0;
    }
  
    const charExists = (char, charIndexMap) => charIndexMap.has(char);
  
    const updateStart = (char, charIndexMap, start) => {
      return charExists(char, charIndexMap)
        ? Math.max(charIndexMap.get(char) + 1, start)
        : start;
    };
  
    const updateCharIndexMap = (char, charIndexMap, end) => {
      charIndexMap.set(char, end);
      return charIndexMap;
    };
  
    const calculateMaxLength = (end, start) => end - start + 1;
  
    const result = s.split('').reduce(
      (state, char) => {
        const { maxLength, start, charIndexMap, end } = state;
        const updatedStart = updateStart(char, charIndexMap, start);
        const updatedCharIndexMap = updateCharIndexMap(char, charIndexMap, end);
        const updatedMaxLength = Math.max(maxLength, calculateMaxLength(end, updatedStart));
  
        return {
          maxLength: updatedMaxLength,
          start: updatedStart,
          charIndexMap: updatedCharIndexMap,
          end: end + 1,
        };
      },
      { maxLength: 0, start: 0, charIndexMap: new Map(), end: 0 }
    );
  
    return result.maxLength;
  }