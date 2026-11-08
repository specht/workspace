const canvas = document.getElementById('canvas');
const gl = canvas.getContext('webgl2');

const vertexShader = `#version 300 es
in vec3 position;
in vec2 uv;
in vec3 normal;
uniform mat4 projection;
uniform mat4 view;
out vec2 v_uv;
out vec3 v_normal;
void main() {
    gl_Position = projection * view * vec4(position, 1.0);
    v_uv = uv;
    v_normal = normal;
}`;

const fragmentShader = `#version 300 es
precision highp float;
in vec2 v_uv;
in vec3 v_normal;
uniform sampler2D colormap;
uniform vec3 light;
out vec4 color;
void main() {
    vec3 base = texture(colormap, v_uv).rgb;
    float diffuse = max(dot(normalize(v_normal), normalize(light)), 0.0);
    color = vec4(base * (0.3 + 0.7 * diffuse), 1.0);
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

let yaw = 0.6;
let pitch = 0.3;
let distance = 20;
let center = [0, 0, 0];

canvas.addEventListener('pointermove', (e) => {
    if (e.buttons) {
        yaw += e.movementX * 0.01;
        pitch = Math.max(-1.5, Math.min(1.5, pitch + e.movementY * 0.01));
    }
});
canvas.addEventListener('wheel', (e) => {
    distance *= Math.exp(e.deltaY * 0.001);
});

async function start() {
    const text = await (await fetch('ship-pirate-large.obj')).text();
    const vertices = parseObj(text);
    const count = vertices.length / 8;

    // die Kiste um das Modell: die Kamera schaut auf ihre Mitte
    const low = [Infinity, Infinity, Infinity];
    const high = [-Infinity, -Infinity, -Infinity];
    for (let i = 0; i < vertices.length; i += 8) {
        for (let k = 0; k < 3; k++) {
            low[k] = Math.min(low[k], vertices[i + k]);
            high[k] = Math.max(high[k], vertices[i + k]);
        }
    }
    center = [0, 1, 2].map((k) => (low[k] + high[k]) / 2);
    distance = 0.9 * Math.hypot(high[0] - low[0], high[1] - low[1], high[2] - low[2]);

    const buffer = gl.createBuffer();
    gl.bindBuffer(gl.ARRAY_BUFFER, buffer);
    gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(vertices), gl.STATIC_DRAW);
    const attributes = [['position', 3, 0], ['uv', 2, 3], ['normal', 3, 5]];
    for (const [name, size, offset] of attributes) {
        const location = gl.getAttribLocation(program, name);
        gl.enableVertexAttribArray(location);
        gl.vertexAttribPointer(location, size, gl.FLOAT, false, 8 * 4, offset * 4);
    }

    const image = new Image();
    image.src = 'colormap.png';
    await image.decode();
    gl.bindTexture(gl.TEXTURE_2D, gl.createTexture());
    gl.pixelStorei(gl.UNPACK_FLIP_Y_WEBGL, true);
    gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, gl.RGBA, gl.UNSIGNED_BYTE, image);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.NEAREST);

    gl.uniform3fv(gl.getUniformLocation(program, 'light'), [0.4, 0.8, 0.45]);
    gl.enable(gl.DEPTH_TEST);

    function frame() {
        canvas.width = canvas.clientWidth;
        canvas.height = canvas.clientHeight;
        gl.viewport(0, 0, canvas.width, canvas.height);
        gl.clearColor(0.12, 0.13, 0.16, 1);
        gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);

        const projection = perspective(0.8, canvas.width / canvas.height, 0.1, 100);
        const view = multiply(translate(0, 0, -distance), multiply(rotateX(pitch), multiply(rotateY(yaw), translate(-center[0], -center[1], -center[2]))));
        gl.uniformMatrix4fv(gl.getUniformLocation(program, 'projection'), false, projection);
        gl.uniformMatrix4fv(gl.getUniformLocation(program, 'view'), false, view);
        gl.drawArrays(gl.TRIANGLES, 0, count);
        requestAnimationFrame(frame);
    }
    frame();
}

start();
