function multiply(a, b) {
    const out = new Array(16).fill(0);
    for (let col = 0; col < 4; col++) {
        for (let row = 0; row < 4; row++) {
            for (let k = 0; k < 4; k++) {
                out[col * 4 + row] += b[k * 4 + row] * a[col * 4 + k];
            }
        }
    }
    return out;
}

function translate(x, y, z) {
    return [1, 0, 0, 0,  0, 1, 0, 0,  0, 0, 1, 0,  x, y, z, 1];
}

if (typeof module !== 'undefined') module.exports = { multiply, translate };
