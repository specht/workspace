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
    return [c, 0, -s, 0,  0, 1, 0, 0,  s, 0, c, 0,  0, 0, 0, 1];
}

function perspective(fov, aspect, near, far) {
    const f = 1 / Math.tan(fov / 2);
    return [f / aspect, 0, 0, 0,  0, f, 0, 0,  0, 0, (far + near) / (near - far), -1,  0, 0, 2 * far * near / (near - far), 0];
}

function faceNormal(a, b, c) {
    const u = [b[0] - a[0], b[1] - a[1], b[2] - a[2]];
    const v = [c[0] - a[0], c[1] - a[1], c[2] - a[2]];
    const n = [u[1] * v[2] - u[2] * v[1], u[2] * v[0] - u[0] * v[2], u[0] * v[1] - u[1] * v[0]];
    const length = Math.hypot(n[0], n[1], n[2]);
    return [n[0] / length, n[1] / length, n[2] / length];
}

// die Ecken eines Modells, acht Zahlen je Ecke, mit der Normale ihres Dreiecks
// statt der Normale aus der Datei
function flatNormals(vertices) {
    const out = vertices.slice();
    for (let i = 0; i < vertices.length; i += 24) {
        const a = vertices.slice(i, i + 3);
        const b = vertices.slice(i + 8, i + 11);
        const c = vertices.slice(i + 16, i + 19);
        const n = faceNormal(a, b, c);
        out[i + 5] = n[0];
        out[i + 6] = n[1];
        out[i + 7] = n[2];
    }
    return out;
}

if (typeof module !== 'undefined') module.exports = { multiply, translate, rotateX, rotateY, perspective, faceNormal, flatNormals };
