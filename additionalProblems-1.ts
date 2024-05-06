function mathMax(values: number[]): number {
    return values.reduce((a, b) => a > b ? a : b, 0)
}

function arrayLength(value: string[]): number {
    return value.reduce((a, b) => a + 1, 0)
}

function countTeamSize(tss: string[][]): number {
    return tss
        .map((ts) => ts.reduce((a, b) => a + 1, 0))
        .reduce((a, b) => a > b ? a : b, 0)
}

function countTeamSize2(tss: string[][]): number {
    const a = tss
        .map((ts) => ts.length)
    return Math.max(...a.concat([0]))
}

function countTeamSize3(tss: string[][]): number {
    return tss.reduce((ls, t) => Math.max(ls, t.length), 0);
}