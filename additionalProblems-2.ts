function generateIncome(sm: number): number[] {
    const ima = Array(12).fill(1);
    const partOne = ima.slice(0, (sm + 1));
    const partTwo = Array(12 - (sm + 1)).fill(2);
    const income = partOne.concat(partTwo);
    return income;
}

function generateIncome2(sm: number): number[] {
    const ima = Array(12).fill(1)
    const income = ima.map((_, index) => {
        return index < sm - 1 ? 1 : 2;
    });

    return income;
}

function generateIncome3(sm: number): number[] {
    if (sm < 1 || sm > 12) {
        throw new Error("Invalid start month.");
    }
    const ima = Array(12).fill(1);
    const partOne = ima.slice(0, sm + 1);
    const partTwo = Array(12 - sm + 1).fill(2);
    const incomeArray = partOne.concat(partTwo);
    return incomeArray;
}