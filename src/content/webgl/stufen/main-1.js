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

    // drei Ecken: x, y und eine Farbe (Rot, Grün, Blau)
    const vertices = [
        -0.6, -0.5,   1.0, 0.0, 0.0,
         0.6, -0.5,   0.0, 1.0, 0.0,
         0.0,  0.6,   0.0, 0.0, 1.0,
    ];
    const buffer = gl.createBuffer();
    gl.bindBuffer(gl.ARRAY_BUFFER, buffer);
    gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(vertices), gl.STATIC_DRAW);
    const attributes = [['position', 2, 0], ['color', 3, 2]];
    for (const [name, size, offset] of attributes) {
        const location = gl.getAttribLocation(program, name);
        gl.enableVertexAttribArray(location);
        gl.vertexAttribPointer(location, size, gl.FLOAT, false, 5 * 4, offset * 4);
    }

    canvas.width = canvas.clientWidth;
    canvas.height = canvas.clientHeight;
    gl.viewport(0, 0, canvas.width, canvas.height);
    gl.clearColor(0.12, 0.13, 0.16, 1);
    gl.clear(gl.COLOR_BUFFER_BIT);
    gl.drawArrays(gl.TRIANGLES, 0, 3);
}

start();
