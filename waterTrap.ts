const Scan = <T, U>(
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

// method-1
const trap = (height: number[]): number => {
    const n = height.length;
    const ml = Scan(height, (max, h) => Math.max(max, h), 0);
    const mr = Scan([...height].reverse(), (max, h) => Math.max(max, h), 0);
    mr.reverse();
    let tw = 0;
    for (let i = 0; i < n; i++) {
        const ms = Math.min(ml[i], mr[i]);
        if (ms > height[i]) {
            tw += ms - height[i];
        }
    }

    return tw;
};

// method-2
const trap2 = (height: number[]): number => {
    const ml = Scan(height, (max, h) => Math.max(max, h), 0);
    const mr = Scan([...height].reverse(), (max, h) => Math.max(max, h), 0);
    mr.reverse();
    const tw = height.map((h, i) => {
        const ms = Math.min(ml[i], mr[i]);
        return ms > h ? ms - h : 0;
    });

    return tw.reduce((total, water) => total + water, 0);
};
