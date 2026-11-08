const canvas = document.getElementById('canvas');
const gl = canvas.getContext('webgl2');

function compile(type, source) {
    const shader = gl.createShader(type);
    gl.shaderSource(shader, source);
    gl.compileShader(shader);
    if (!gl.getShaderParameter(shader, gl.COMPILE_STATUS)) {
        throw new Error(gl.getShaderInfoLog(shader));
    }
    return shader;
}

// lädt die beiden Shader aus ihren Dateien und verbindet sie zu einem Programm
async function loadProgram(vertexFile, fragmentFile) {
    const vertexSource = await (await fetch(vertexFile, { cache: 'no-store' })).text();
    const fragmentSource = await (await fetch(fragmentFile, { cache: 'no-store' })).text();
    const program = gl.createProgram();
    gl.attachShader(program, compile(gl.VERTEX_SHADER, vertexSource));
    gl.attachShader(program, compile(gl.FRAGMENT_SHADER, fragmentSource));
    gl.linkProgram(program);
    gl.useProgram(program);
    return program;
}

async function start() {
    const program = await loadProgram('shader.vert', 'shader.frag');

    // sechs Seiten, jede aus zwei Dreiecken und in einer eigenen Farbe; die
    // Ecken jeder Seite gegen den Uhrzeigersinn, von außen gesehen
    const faces = [
        { corners: [[-1, -1, 1], [1, -1, 1], [1, 1, 1], [-1, 1, 1]], color: [0.98, 0.36, 0.45] },
        { corners: [[1, -1, -1], [-1, -1, -1], [-1, 1, -1], [1, 1, -1]], color: [0.2, 0.75, 0.68] },
        { corners: [[1, -1, 1], [1, -1, -1], [1, 1, -1], [1, 1, 1]], color: [1.0, 0.82, 0.38] },
        { corners: [[-1, -1, -1], [-1, -1, 1], [-1, 1, 1], [-1, 1, -1]], color: [0.36, 0.6, 0.95] },
        { corners: [[-1, 1, 1], [1, 1, 1], [1, 1, -1], [-1, 1, -1]], color: [0.62, 0.88, 0.46] },
        { corners: [[-1, -1, -1], [1, -1, -1], [1, -1, 1], [-1, -1, 1]], color: [0.76, 0.52, 0.92] },
    ];
    const vertices = [];
    for (const face of faces) {
        const [a, b, c, d] = face.corners;
        for (const corner of [a, b, c, a, c, d]) {
            vertices.push(...corner, ...face.color);
        }
    }
    const buffer = gl.createBuffer();
    gl.bindBuffer(gl.ARRAY_BUFFER, buffer);
    gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(vertices), gl.STATIC_DRAW);
    const attributes = [['position', 3, 0], ['color', 3, 3]];
    for (const [name, size, offset] of attributes) {
        const location = gl.getAttribLocation(program, name);
        gl.enableVertexAttribArray(location);
        gl.vertexAttribPointer(location, size, gl.FLOAT, false, 6 * 4, offset * 4);
    }
    gl.enable(gl.DEPTH_TEST);

    function frame(time) {
        canvas.width = canvas.clientWidth;
        canvas.height = canvas.clientHeight;
        gl.viewport(0, 0, canvas.width, canvas.height);
        gl.clearColor(0.12, 0.13, 0.16, 1);
        gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);

        const t = time / 1000;
        const projection = perspective(0.8, canvas.width / canvas.height, 0.1, 100);
        const view = translate(0, 0, -5);
        const model = multiply(rotateX(0.5), rotateY(0.5 * t));
        gl.uniformMatrix4fv(gl.getUniformLocation(program, 'projection'), false, projection);
        gl.uniformMatrix4fv(gl.getUniformLocation(program, 'view'), false, view);
        gl.uniformMatrix4fv(gl.getUniformLocation(program, 'model'), false, model);
        gl.drawArrays(gl.TRIANGLES, 0, vertices.length / 6);
        requestAnimationFrame(frame);
    }
    requestAnimationFrame(frame);
}

start();
