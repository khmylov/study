function longestPalindrome(s: string): string {
    let maxLength = 1
    let startIndex = 0
    let length = s.length

    for (let index = 1; index < length; index++) {
        let low = index - 1
        let high = index

        while (low >= 0 && high <= length && s[low] === s[high]) {
            if (high - low + 1 > maxLength) {
                maxLength = high - low + 1
                startIndex = low
            }
            low--
            high++
        }

        low = index - 1
        high = index + 1

        while (low >= 0 && high <= length && s[low] === s[high]) {
            if (high - low + 1 > maxLength) {
                maxLength = high - low + 1
                startIndex = low
            }
            low--
            high++
        }
    }

    return s.substr(startIndex, maxLength)
}
