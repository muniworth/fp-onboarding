function duplicateZeros(arr: number[]): void {
  const newArr: number[] = [];
  for (const num of arr) {
    newArr.push(num);
    if (num === 0) {
      newArr.push(0);
    }
  }
  for (let i = 0; i < arr.length; i++) {
    arr[i] = newArr[i];
  }
}