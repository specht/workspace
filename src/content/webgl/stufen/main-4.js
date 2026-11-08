const canvas = document.getElementById('canvas');
const gl = canvas.getContext('webgl2');

const vertexShader = `#version 300 es
in vec3 position;
in vec3 color;
in vec3 normal;
uniform mat4 projection;
uniform mat4 view;
uniform mat4 model;
out vec3 v_color;
out vec3 v_normal;
void main() {
    gl_Position = projection * view * model * vec4(position, 1.0);
    v_color = color;
    v_normal = mat3(model) * normal;
}`;

const fragmentShader = `#version 300 es
precision highp float;
in vec3 v_color;
in vec3 v_normal;
uniform vec3 light;
out vec4 color;
void main() {
    float diffuse = max(dot(normalize(v_normal), normalize(light)), 0.0);
    color = vec4(v_color * (0.3 + 0.7 * diffuse), 1.0);
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

// sechs Seiten, jede aus zwei Dreiecken und in einer eigenen Farbe; die
// Ecken jeder Seite gegen den Uhrzeigersinn, von außen gesehen
const faces = [
    { corners: [[-1, -1, 1], [1, -1, 1], [1, 1, 1], [-1, 1, 1]], color: [0.9, 0.3, 0.2] },
    { corners: [[1, -1, -1], [-1, -1, -1], [-1, 1, -1], [1, 1, -1]], color: [0.2, 0.6, 0.9] },
    { corners: [[1, -1, 1], [1, -1, -1], [1, 1, -1], [1, 1, 1]], color: [0.3, 0.8, 0.3] },
    { corners: [[-1, -1, -1], [-1, -1, 1], [-1, 1, 1], [-1, 1, -1]], color: [0.95, 0.8, 0.2] },
    { corners: [[-1, 1, 1], [1, 1, 1], [1, 1, -1], [-1, 1, -1]], color: [0.8, 0.4, 0.9] },
    { corners: [[-1, -1, -1], [1, -1, -1], [1, -1, 1], [-1, -1, 1]], color: [0.9, 0.9, 0.9] },
];
const vertices = [];
for (const face of faces) {
    const [a, b, c, d] = face.corners;
    const normal = faceNormal(a, b, c);
    for (const corner of [a, b, c, a, c, d]) {
        vertices.push(...corner, ...face.color, ...normal);
    }
}
const buffer = gl.createBuffer();
gl.bindBuffer(gl.ARRAY_BUFFER, buffer);
gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(vertices), gl.STATIC_DRAW);
const attributes = [['position', 3, 0], ['color', 3, 3], ['normal', 3, 6]];
for (const [name, size, offset] of attributes) {
    const location = gl.getAttribLocation(program, name);
    gl.enableVertexAttribArray(location);
    gl.vertexAttribPointer(location, size, gl.FLOAT, false, 9 * 4, offset * 4);
}
gl.uniform3fv(gl.getUniformLocation(program, 'light'), [0.4, 0.8, 0.45]);

gl.enable(gl.DEPTH_TEST);

function frame(time) {
    canvas.width = canvas.clientWidth;
    canvas.height = canvas.clientHeight;
    gl.viewport(0, 0, canvas.width, canvas.height);
    gl.clearColor(0.12, 0.13, 0.16, 1);
    gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);

    const t = time / 1000;
    const projection = perspective(0.8, canvas.width / canvas.height, 0.1, 100);
    const view = translate(0, 0, -6);
    const model = multiply(rotateY(t), rotateX(0.7 * t));
    gl.uniformMatrix4fv(gl.getUniformLocation(program, 'projection'), false, projection);
    gl.uniformMatrix4fv(gl.getUniformLocation(program, 'view'), false, view);
    gl.uniformMatrix4fv(gl.getUniformLocation(program, 'model'), false, model);
    gl.drawArrays(gl.TRIANGLES, 0, vertices.length / 9);
    requestAnimationFrame(frame);
}
requestAnimationFrame(frame);
