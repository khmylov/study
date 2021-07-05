function* traverse(nums: number[], iLow: number, iHigh: number): Generator<number> {
    if (iLow < 0 || iLow > iHigh || iHigh >= nums.length) {
        return;
    }

    const iMid = Math.ceil((iLow + iHigh) / 2);

    for (const x of traverse(nums, iLow, iMid - 1)) {
        yield x;
    }

    yield nums[iMid];

    for (const x of traverse(nums, iMid + 1, iHigh)) {
        yield x;
    }
}

function traverseAll(nums: number[]) {
    return traverse(nums, 0, nums.length - 1);
}

function* merge<T>(gen1: Generator<T>, gen2: Generator<T>): Generator<T> {
    let state1 = gen1.next();
    let state2 = gen2.next();
    while (!state1.done && !state2.done) {
        if (state1.value < state2.value) {
            yield state1.value;
            state1 = gen1.next();
        } else {
            yield state2.value;
            state2 = gen2.next();
        }
    }
    while (!state1.done) {
        yield state1.value;
        state1 = gen1.next();
    }
    while (!state2.done) {
        yield state2.value;
        state2 = gen2.next();
    }
}

function findMedianSortedArrays(nums1: number[], nums2: number[]): number {
    let totalVisited = 0;
    let gen1 = traverseAll(nums1);
    let gen2 = traverseAll(nums2);

    const sum = nums1.length + nums2.length;
    const expectedLength = Math.ceil(sum / 2);

    let prevValue: number = undefined!;
    let value: number = undefined!;
    for (value of merge(gen1, gen2)) {
        totalVisited++;
        if (totalVisited > expectedLength) {
            break;
        }

        prevValue = value;
    }

    if (prevValue === undefined) {
        return value;
    }

    return sum % 2 ? prevValue : (prevValue + value) / 2;
}

//console.log('res', findMedianSortedArrays([2,3,8,9,15,26,31], [1,2,3,4,5]));
console.log('res', findMedianSortedArrays([1,3], [2]), 2);
console.log('res', findMedianSortedArrays([1,2], [3,4]), 2.5);
console.log('res', findMedianSortedArrays([0,0], [0,0]), 0);
console.log('res', findMedianSortedArrays([], [1]), 1);
console.log('res', findMedianSortedArrays([2], []), 2);
