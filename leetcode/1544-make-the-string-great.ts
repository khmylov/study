// Naive recursive
function makeGood(s: string): string {
    for (let i = 0; i < s.length - 1; i++) {
        const c1 = s.charCodeAt(i);
        const c2 = s.charCodeAt(i+1);
        if (Math.abs(c2 - c1) === 32) {
            return makeGood(s.substr(0, i) + s.substr(i+2));
        }
    }
    
    return s;
}

// Stack to compare last 2
function makeGood(s: string): string {
    const chars = [];
    for (let index = 0; index < s.length; index++) {
        chars.push(s[index]);
        const c1 = chars[chars.length - 1];
        const c2 = chars[chars.length - 2];
        if (c2 && Math.abs(c1.charCodeAt(0) - c2.charCodeAt(0)) === 32) {
            chars.pop();
            chars.pop();
        }
    }
    
    return chars.join('');
}
