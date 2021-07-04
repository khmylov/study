function uniquePaths(m: number, n: number): number {
    const mem: number[][] = new Array(m + 1)
    for (let i = 0; i <= m; i++) {
        const arr: number[] = new Array(n + 1)
        mem[i] = arr
        for (let j = 0; j <= n; j++) {
            arr[j] = 0
        }
    }


    for (let i = 1; i <= m; i++) {
        for (let j = 1; j <= n; j++) {
            if (i === 1 && j === 1) {
                mem[1][1] = 1
            } else {
                mem[i][j] = mem[i - 1][j] + mem[i][j - 1]
            }
        }
    }

    return mem[m][n]
}