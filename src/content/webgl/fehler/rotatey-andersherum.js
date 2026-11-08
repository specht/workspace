// 4x4-Matrizen als Arrays aus 16 Zahlen, Spalte für Spalte, wie WebGL sie will

function multiply(a, b) {
    const out = new Array(16).fill(0);
    for (let col = 0; col < 4; col++) {
        for (let row = 0; row < 4; row++) {
            for (let k = 0; k < 4; k++) {
                out[col * 4 + row] += a[k * 4 + row] * b[col * 4 + k];
            }
        }
    }
    return out;
}

function translate(x, y, z) {
    return [1, 0, 0, 0,  0, 1, 0, 0,  0, 0, 1, 0,  x, y, z, 1];
}

function rotateX(angle) {
    const c = Math.cos(angle);
    const s = Math.sin(angle);
    return [1, 0, 0, 0,  0, c, s, 0,  0, -s, c, 0,  0, 0, 0, 1];
}

function rotateY(angle) {
    const c = Math.cos(angle);
    const s = Math.sin(angle);
    return [c, 0, s, 0,  0, 1, 0, 0,  -s, 0, c, 0,  0, 0, 0, 1];
}

function perspective(fov, aspect, near, far) {
    const f = 1 / Math.tan(fov / 2);
    return [f / aspect, 0, 0, 0,  0, f, 0, 0,  0, 0, (far + near) / (near - far), -1,  0, 0, 2 * far * near / (near - far), 0];
}

if (typeof module !== 'undefined') module.exports = { multiply, translate, rotateX, rotateY, perspective };
