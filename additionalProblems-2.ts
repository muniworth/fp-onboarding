function generateIncome2(sm: number): number[] {
    const ima = Array(12).fill(1)

    const income = ima.map((_, index) => {
        return index < sm - 1 ? 1 : 2;
    });

    return income;
}
