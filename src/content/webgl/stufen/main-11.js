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

let yaw = -0.4;
let pitch = 0.15;
let distance = 1;
let center = [0, 0, 0];
let wireframe = false;
let flat = false;
const maps = { useColor: true, useNormal: true, useRoughness: true, useAo: true, useMetal: true };

canvas.addEventListener('pointermove', (e) => {
    if (e.buttons) {
        yaw += e.movementX * 0.01;
        pitch = Math.max(-1.5, Math.min(1.5, pitch + e.movementY * 0.01));
    }
});
canvas.addEventListener('wheel', (e) => {
    distance *= Math.exp(e.deltaY * 0.001);
});
window.addEventListener('keydown', (e) => {
    if (e.key === 'w') wireframe = !wireframe;
    if (e.key === 'f') flat = !flat;
    const names = { 1: 'useColor', 2: 'useNormal', 3: 'useRoughness', 4: 'useAo', 5: 'useMetal' };
    if (names[e.key]) maps[names[e.key]] = !maps[names[e.key]];
});

// wo die Kamera steht: dort, wohin die Matrix view den Ursprung schiebt
function cameraPosition() {
    return [
        center[0] - distance * Math.sin(yaw) * Math.cos(pitch),
        center[1] + distance * Math.sin(pitch),
        center[2] + distance * Math.cos(yaw) * Math.cos(pitch),
    ];
}

// ein Bild als Textur auf der Grafikkarte, in der Einheit unit (0, 1, 2, …)
async function loadTexture(file, unit) {
    const image = new Image();
    image.src = file;
    await image.decode();
    gl.activeTexture(gl.TEXTURE0 + unit);
    gl.bindTexture(gl.TEXTURE_2D, gl.createTexture());
    gl.pixelStorei(gl.UNPACK_FLIP_Y_WEBGL, true);
    gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, gl.RGBA, gl.UNSIGNED_BYTE, image);
    gl.generateMipmap(gl.TEXTURE_2D);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR_MIPMAP_LINEAR);
}

async function start() {
    const program = await loadProgram('shader.vert', 'shader.frag');
    const text = await (await fetch('clock.obj')).text();
    const vertices = parseObj(text);
    const count = vertices.length / 8;
    const flatVertices = flatNormals(vertices);

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
    distance = 1.4 * Math.hypot(high[0] - low[0], high[1] - low[1], high[2] - low[2]);

    const buffer = gl.createBuffer();
    gl.bindBuffer(gl.ARRAY_BUFFER, buffer);
    gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(vertices), gl.STATIC_DRAW);
    const attributes = [['position', 3, 0], ['uv', 2, 3], ['normal', 3, 5]];
    for (const [name, size, offset] of attributes) {
        const location = gl.getAttribLocation(program, name);
        if (location < 0) continue;   // ein Attribut, das der Shader nicht benutzt
        gl.enableVertexAttribArray(location);
        gl.vertexAttribPointer(location, size, gl.FLOAT, false, 8 * 4, offset * 4);
    }

    const lines = gl.createBuffer();
    gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, lines);
    gl.bufferData(gl.ELEMENT_ARRAY_BUFFER, new Uint32Array(edges(count / 3)), gl.STATIC_DRAW);

    const textures = [['colorMap', 'clock-color.jpg'], ['normalMap', 'clock-normal.jpg'],
        ['roughnessMap', 'clock-roughness.jpg'], ['aoMap', 'clock-ao.jpg'],
        ['metalMap', 'clock-metal.jpg'], ['environment', 'hotel-room.jpg']];
    for (let unit = 0; unit < textures.length; unit++) {
        const [name, file] = textures[unit];
        await loadTexture(file, unit);
        gl.uniform1i(gl.getUniformLocation(program, name), unit);
    }
    let shownFlat = false;

    gl.uniform3fv(gl.getUniformLocation(program, 'light'), [0.4, 0.8, 0.45]);
    gl.enable(gl.DEPTH_TEST);

    function frame() {
        canvas.width = canvas.clientWidth;
        canvas.height = canvas.clientHeight;
        gl.viewport(0, 0, canvas.width, canvas.height);
        gl.clearColor(0.12, 0.13, 0.16, 1);
        gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);

        if (flat !== shownFlat) {
            gl.bindBuffer(gl.ARRAY_BUFFER, buffer);
            gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(flat ? flatVertices : vertices), gl.STATIC_DRAW);
            shownFlat = flat;
        }

        const projection = perspective(0.8, canvas.width / canvas.height, 0.01, 100);
        const view = multiply(translate(0, 0, -distance), multiply(rotateX(pitch), multiply(rotateY(yaw), translate(-center[0], -center[1], -center[2]))));
        gl.uniformMatrix4fv(gl.getUniformLocation(program, 'projection'), false, projection);
        gl.uniformMatrix4fv(gl.getUniformLocation(program, 'view'), false, view);
        gl.uniform3fv(gl.getUniformLocation(program, 'eye'), cameraPosition());
        for (const name in maps) {
            gl.uniform1i(gl.getUniformLocation(program, name), maps[name]);
        }
        gl.uniform1i(gl.getUniformLocation(program, 'wire'), wireframe);
        if (wireframe) {
            gl.drawElements(gl.LINES, count * 2, gl.UNSIGNED_INT, 0);
        } else {
            gl.drawArrays(gl.TRIANGLES, 0, count);
        }
        requestAnimationFrame(frame);
    }
    frame();
}

start();
