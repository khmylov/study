function generateParenthesis(n: number): string[] {
    const results: string[] = []

    function loop(current: string, pos: number, numberOfOpen: number, numberOfClose: number) {
        if (pos === 2 * n) {
            results.push(current)
            return
        }

        if (numberOfOpen < n) {
            loop(current + '(', pos + 1, numberOfOpen + 1, numberOfClose)
        }

        if (numberOfOpen > numberOfClose) {
            loop(current + ')', pos + 1, numberOfOpen, numberOfClose + 1)
        }
    }
    
    loop('', 0, 0, 0)
    return results
}
