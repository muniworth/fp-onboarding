// Iota from function.ts
export const Iota = (length: number, start: number) =>
    Array(length)
        .fill(0)
        .map((_, i) => start + i);

// Rotate from function.ts
export const Rotate = <T>(xs: T[], i: number) => {
    const j = i % xs.length;
    return [...xs.slice(j), ...xs.slice(0, j)];
};

function generateIncome(sm: number): number[] {
    const ima = Iota(12, 1);

    const ra = Rotate(ima, sm - 1);

    const income = ra.map((_, index) => (index < sm - 1 ? 1 : 2));

    return income;
}