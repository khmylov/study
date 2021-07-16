class TreeAncestor {
    private readonly _jumps: number[][]

    /*
                       1
              +--------+---------+
              2                  3
        +-----+----+       +-----+-----+
        4          5       6           7
       / \        / \     / \         / \
      8   9      10  11  12  13      14  15
    /
   16 ......

    Let's define f(n, k) = k-th ancestor of node `n`
    Then we can observe that f(16, 4) = f(8, 3) = f(4, 2) = f(2, 1).


    Linear search time doesn't fit time complexity here, so we can use so called "binary lifting":
    pre-compute 1st, 2nd, 4th, 8th, ... ancestors for every node.
    and then represent k in its binary form to build a chain of jumps,
    e.g. f(n, 7) = n |> f (4) |> f(2) |> f(1)

    Using dynamic programming, let's pre-compute `jumps`, such as `jumps[i][j]` stores `2^j`-th parent of node `i`:
        jumps[i][j] = jumps[jumps[i][j-1]][j-1]
    */
    constructor(nodesCount: number, parents: number[]) {
        const jumps: number[][] = Array.from({length: nodesCount})
        // It's not actually stated in the description, but trees seem to be binary,
        // so max height is `log2(N) + 1`
        let height = Math.ceil(Math.log2(nodesCount))

        for (let j = 0; j < height; j++) {
            for (let i = 0; i < nodesCount; i++) {
                if (!jumps[i]) {
                    jumps[i] = Array.from({length: height}).map(() => -1)
                }

                if (j === 0) {
                    jumps[i][j] = parents[i]
                } else {
                    const ancestor = jumps[i][j-1];
                    if (ancestor !== -1) {
                        jumps[i][j] = jumps[ancestor][j-1]
                    }
                }
            }
        }
        this._jumps = jumps;
        // console.log(this._jumps);
    }

    getKthAncestor(node: number, k: number): number {
        while (k > 0 && node !== -1) {
            let i = Math.floor(Math.log2(k))
            const nextNode = this._jumps[node][i]
            //console.log({k, node, i, nextNode})
            node = nextNode
            k -= Math.pow(2, i)
        }

        return node;
    }
}

const treeAncestor = new TreeAncestor(7, [-1, 0, 0, 1, 1, 2, 2]);
console.log(treeAncestor.getKthAncestor(3, 1)); // returns 1 which is the parent of 3
console.log(treeAncestor.getKthAncestor(5, 2)); // returns 0 which is the grandparent of 5
console.log(treeAncestor.getKthAncestor(6, 3)); // returns -1 because there is no such ancestor