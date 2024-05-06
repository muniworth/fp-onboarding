const ScanSkip1 = <T, U>(
    list: T[],
    accumulator: (acc: U, item: T) => U,
    initialAcc: U
): U[] => {
    const results: U[] = [];
    let acc = initialAcc;

    list.forEach((item) => {
        acc = accumulator(acc, item);
        results.push(acc);
    });

    return results;
};

const ScanBack = <T, U>(
    list: T[],
    accumulator: (acc: U, item: T) => U,
    initialAcc: U
): U[] => {
    const results: U[] = [];
    let acc = initialAcc;

    for (let i = list.length - 1; i >= 0; i--) {
        const item = list[i];
        acc = accumulator(acc, item);
        results.unshift(acc);
    }

    return results;
};

const zipWith3 = <T, U, V, W>(arr1: T[], arr2: U[], arr3: V[], zipper: (a: T, b: U, c: V) => W): W[] => {
    return arr1.map((elem, i) => zipper(elem, arr2[i], arr3[i]));
};

const trap = (height: number[]): number => {
    const ml = ScanSkip1(height, (max, h) => Math.max(max, h), 0);
    const mr = ScanBack(height, (max, h) => Math.max(max, h), 0);
    const tw = zipWith3(height, ml, mr, (h, l, r) => {
        let m = Math.min(l, r)
        return m > h ? m - h : 0;
    })

    return tw.reduce((total, water) => total + water, 0);
}
