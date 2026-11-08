const canvas = document.getElementById('canvas');
const gl = canvas.getContext('webgl2');

const vertexShader = `#version 300 es
in vec2 position;
in vec3 color;
out vec3 v_color;
void main() {
    gl_Position = vec4(position, 0.0, 1.0);
    v_color = color;
}`;

const fragmentShader = `#version 300 es
precision highp float;
in vec3 v_color;
out vec4 color;
void main() {
    color = vec4(v_color, 1.0);
}`;

function compile(type, source) {
    const shader = gl.createShader(type);
    gl.shaderSource(shader, source);
    gl.compileShader(shader);
    if (!gl.getShaderParameter(shader, gl.COMPILE_STATUS)) {
        throw new Error(gl.getShaderInfoLog(shader));
    }
    return shader;
}

const program = gl.createProgram();
gl.attachShader(program, compile(gl.VERTEX_SHADER, vertexShader));
gl.attachShader(program, compile(gl.FRAGMENT_SHADER, fragmentShader));
gl.linkProgram(program);
gl.useProgram(program);

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
