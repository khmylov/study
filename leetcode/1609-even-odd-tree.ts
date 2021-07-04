/**
 * Definition for a binary tree node.
 * class TreeNode {
 *     val: number
 *     left: TreeNode | null
 *     right: TreeNode | null
 *     constructor(val?: number, left?: TreeNode | null, right?: TreeNode | null) {
 *         this.val = (val===undefined ? 0 : val)
 *         this.left = (left===undefined ? null : left)
 *         this.right = (right===undefined ? null : right)
 *     }
 * }
 */

function xor(a: boolean, b: boolean): boolean {
    return (a && !b) || (!a && b)
}

function check(nodes: TreeNode[], isEven: boolean): boolean {
    if (!nodes.every(x => xor(x.val % 2 === 0, isEven))) {
        return false
    }
    
    let current = nodes[0].val
    for (let i = 1; i < nodes.length; i++) {
        if (isEven && nodes[i].val <= current) {
            return false
        } else if (!isEven && nodes[i].val >= current) {
            return false
        }
        current = nodes[i].val
    }
    
    return true
}

function getChildren(nodes: TreeNode[]): TreeNode[] {
    const res: TreeNode[] = []
    for (const n of nodes) {
        if (n.left) {
            res.push(n.left)
        }
        if (n.right) {
            res.push(n.right)
        }
    }
    
    return res
}

function isEvenOddTree(root: TreeNode | null): boolean {
    let current = [root!]
    let isEven = true
    while (current.length) {
        if (!check(current, isEven)) {
            return false
        }
        
        current = getChildren(current)
        isEven = !isEven
    }
    
    return true
}
