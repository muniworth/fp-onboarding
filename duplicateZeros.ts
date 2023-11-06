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

// method-2
function duplicateZeros2(arr: number[]): void {
    for (let i = 0; i < arr.length; i++) {
        if (arr[i] === 0) {
            arr.splice(i, 0, 0);
            arr.pop();
            i++;
        }
    }
}

// method-3
function duplicateZeros3(arr) {
    const newArr = arr.flatMap((num) => (num === 0 ? [0, 0] : [num])).slice(0, arr.length);
    arr.length = newArr.length;
    newArr.forEach((num, i) => (arr[i] = num));
}

// method-4
function duplicateZeros4(arr: number[]): number[] {
    const newArr: number[] = [];
    for (let i = 0; i < arr.length; i++) {
        newArr.push(arr[i]);
        if (arr[i] === 0) {
            newArr.push(0);
        }
    }
    return newArr;
}

// Example usage:
const xs = [1, 0, 2, 3, 0, 4, 5, 0];
const xs2 = duplicateZeros4(xs);
for (let i = 0; i < xs.length; i++) {
    xs[i] = xs2[i];
}
console.log(xs); // Output: [1, 0, 0, 2, 3, 0, 0, 4]
