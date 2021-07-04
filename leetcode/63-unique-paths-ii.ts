function uniquePathsWithObstacles(obstacleGrid: number[][]): number {
    const m = obstacleGrid.length
    const n = obstacleGrid[0].length

    const mem: number[][] = new Array(m)
    for (let i = 0; i < m; i++) {
        mem[i] = new Array(n).fill(0)
    }

    if (!obstacleGrid[0][0]) {
        mem[0][0] = 1
    } else {
        return 0
    }

    for (let i = 0; i < m; i++) {
        for (let j = 0; j < n; j++) {
            if (obstacleGrid[i][j]) {
                mem[i][j] = 0
                continue
            }

            if (i !== 0 && !obstacleGrid[i - 1][j]) {
                mem[i][j] += mem[i - 1][j]
            }

            if (j !== 0 && !obstacleGrid[i][j - 1]) {
                mem[i][j] += mem[i][j - 1]
            }
        }
    }

    return mem[m-1][n-1]
}
