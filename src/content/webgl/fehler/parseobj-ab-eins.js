// Ein Modell im OBJ-Format: für jede Ecke jedes Dreiecks acht Zahlen,
// die Lage (x, y, z), die Stelle in der Textur (u, v) und die Normale

function parseObj(text) {
    const positions = [];
    const uvs = [];
    const normals = [];
    const vertices = [];
    for (const line of text.split('\n')) {
        const parts = line.trim().split(/\s+/);
        if (parts[0] === 'v') {
            positions.push(parts.slice(1, 4).map(Number));
        } else if (parts[0] === 'vt') {
            uvs.push(parts.slice(1, 3).map(Number));
        } else if (parts[0] === 'vn') {
            normals.push(parts.slice(1, 4).map(Number));
        } else if (parts[0] === 'f') {
            for (const corner of parts.slice(1, 4)) {
                const [p, t, n] = corner.split('/').map(Number);
                vertices.push(...positions[p], ...uvs[t], ...normals[n]);
            }
        }
    }
    return vertices;
}

if (typeof module !== 'undefined') module.exports = { parseObj };
