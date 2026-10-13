let anaglyph;
let camera;
let models = {};
let tex = {};
let sceneDescription;
let firstFrame = true;

// Extruded text support
const DEFAULT_TEXT_FONT = 'OpenSans';
const DEFAULT_TEXT_FONT_PATH = 'include/OpenSans-Regular.ttf';
const DEFAULT_TEXT_DETAIL = 8;
const DEFAULT_TEAPOT_DETAIL = 8;

let fontBytes = {};
let textFonts = {};
let extrudedTextCache = new Map();
let teapotGeometryCache = new Map();
let shownRuntimeErrors = new Set();

const BUILTIN_SHAPES = new Set([
    'box',
    'sphere',
    'cylinder',
    'cone',
    'torus',
    'teapot',
    'plane',
    'grid'
]);

function preprocessSceneINI(source) {
    const errors = [];
    const definitions = Object.create(null);

    const IDENT = `[A-Za-z_][A-Za-z0-9_-]*`;
    const DEFINE_RE = new RegExp(`^define\\s+(${IDENT})\\s*$`);
    const LOOP_RE = /^loop\s+(\w+)\s+from\s+(-?\d+)\s+to\s+(-?\d+)(?:\s+step\s+(-?\d+))?\s*$/;

    function normalizeAssignment(trimmed) {
        let line = trimmed;

        if (!line.includes('=') && line.includes(' ')) {
            const firstSpace = line.indexOf(' ');
            line = line.slice(0, firstSpace) + '=' + line.slice(firstSpace + 1);
        }

        const eq = line.indexOf('=');
        if (eq < 0) return null;

        return {
            key: line.slice(0, eq).trim(),
            value: line.slice(eq + 1).trim()
        };
    }

    function getShapeName(trimmed) {
        const assignment = normalizeAssignment(trimmed);
        if (!assignment) return null;
        if (assignment.key !== 'shape') return null;
        return assignment.value;
    }

    function isLoopStart(trimmed) {
        return LOOP_RE.test(trimmed);
    }

    function isGroupStart(trimmed) {
        return trimmed === 'group';
    }

    function isDefineStart(trimmed) {
        return DEFINE_RE.test(trimmed);
    }

    function isBlockStart(trimmed) {
        return isLoopStart(trimmed) || isGroupStart(trimmed) || isDefineStart(trimmed);
    }

    function isCustomShapeContinuationBoundary(trimmed) {
        if (trimmed === '') return true;
        if (trimmed === 'end') return true;
        if (isLoopStart(trimmed)) return true;
        if (isGroupStart(trimmed)) return true;
        if (isDefineStart(trimmed)) return true;

        const assignment = normalizeAssignment(trimmed);
        if (!assignment) return false;

        return assignment.key === 'shape' ||
            assignment.key === 'model' ||
            assignment.key === 'text' ||
            assignment.key === 'command';
    }

    function substituteLoopVariables(line, scope) {
        return line.replace(/\b\w+\b/g, word =>
            scope[word] !== undefined ? scope[word] : word
        );
    }

    function collectDefinitions(lines) {
        const mainLines = [];
        let i = 0;

        while (i < lines.length) {
            const line = lines[i];
            const trimmed = line.text.trim();
            const defineMatch = trimmed.match(DEFINE_RE);

            if (!defineMatch) {
                mainLines.push(line);
                i++;
                continue;
            }

            const name = defineMatch[1];

            if (BUILTIN_SHAPES.has(name)) {
                errors.push(`Ungültige Definition in Zeile ${line.number}: "${name}" ist bereits eine eingebaute Form.`);
            }

            if (definitions[name]) {
                errors.push(`Doppelte Definition in Zeile ${line.number}: "${name}" wurde bereits definiert.`);
            }

            const body = [];
            let depth = 1;
            i++;

            while (i < lines.length && depth > 0) {
                const innerLine = lines[i];
                const innerTrimmed = innerLine.text.trim();

                if (isBlockStart(innerTrimmed)) {
                    if (isDefineStart(innerTrimmed)) {
                        errors.push(`Definitionen dürfen nicht verschachtelt werden (Zeile ${innerLine.number}).`);
                    }
                    depth++;
                } else if (innerTrimmed === 'end') {
                    depth--;
                }

                if (depth > 0) {
                    body.push(innerLine);
                }

                i++;
            }

            if (depth !== 0) {
                errors.push(`Fehlendes Schlüsselwort 'end' für Definition "${name}" ab Zeile ${line.number}`);
                continue;
            }

            if (!BUILTIN_SHAPES.has(name) && !definitions[name]) {
                definitions[name] = {
                    name,
                    line: line.number,
                    body
                };
            }
        }

        return mainLines;
    }

    function expand(lines, scope = {}, callStack = []) {
        const result = [];
        let i = 0;
        const stack = [];

        while (i < lines.length) {
            const rawLine = lines[i].text;
            const trimmed = rawLine.trim();
            const lineNumber = lines[i].number;

            const loopMatch = trimmed.match(LOOP_RE);

            if (loopMatch) {
                const [, varName, fromStr, toStr, stepStr] = loopMatch;
                const from = parseInt(fromStr, 10);
                const to = parseInt(toStr, 10);
                const step = stepStr ? parseInt(stepStr, 10) : (to >= from ? 1 : -1);

                if (step === 0) {
                    errors.push(`Ungültige Schrittgröße (step) 0 in Zeile ${lineNumber}`);
                    i++;
                    continue;
                }

                const body = [];
                let depth = 1;
                i++;

                while (i < lines.length && depth > 0) {
                    const innerLine = lines[i];
                    const innerTrimmed = innerLine.text.trim();

                    if (isLoopStart(innerTrimmed) || isGroupStart(innerTrimmed)) {
                        depth++;
                    } else if (innerTrimmed === 'end') {
                        depth--;
                    }

                    if (depth > 0) {
                        body.push(innerLine);
                    }

                    i++;
                }

                if (depth !== 0) {
                    errors.push(`Fehlendes Schlüsselwort 'end' für Schleife (loop) ab Zeile ${lineNumber}`);
                    continue;
                }

                for (let val = from; step > 0 ? val <= to : val >= to; val += step) {
                    const newScope = { ...scope, [varName]: val };
                    result.push(...expand(body, newScope, callStack));
                }

                continue;
            }

            const shapeName = getShapeName(trimmed);

            if (shapeName && definitions[shapeName]) {
                const instanceLine = lines[i];
                const instanceLines = [];
                i++;

                while (i < lines.length) {
                    const nextTrimmed = lines[i].text.trim();

                    if (isCustomShapeContinuationBoundary(nextTrimmed)) {
                        break;
                    }

                    instanceLines.push(lines[i]);
                    i++;
                }

                if (callStack.includes(shapeName)) {
                    errors.push(
                        `Rekursive Definition in Zeile ${instanceLine.number}: ` +
                        `${[...callStack, shapeName].join(' → ')}`
                    );
                    continue;
                }

                const indent = rawLine.match(/^\s*/)?.[0] ?? '';

                result.push({
                    text: `${indent}command = push`,
                    number: instanceLine.number
                });

                for (const instancePart of instanceLines) {
                    result.push({
                        text: substituteLoopVariables(instancePart.text, scope),
                        number: instancePart.number
                    });
                }

                result.push(
                    ...expand(
                        definitions[shapeName].body,
                        scope,
                        [...callStack, shapeName]
                    )
                );

                result.push({
                    text: `${indent}command = pop`,
                    number: instanceLine.number
                });

                continue;
            }

            if (trimmed === 'group') {
                const indent = rawLine.match(/^\s*/)?.[0] ?? '';

                result.push({
                    text: `${indent}command = push`,
                    number: lineNumber
                });

                stack.push(lineNumber);
                i++;
                continue;
            }

            if (trimmed === 'end') {
                const indent = rawLine.match(/^\s*/)?.[0] ?? '';

                if (stack.length === 0) {
                    errors.push(`Überzähliges Schlüsselwort 'end' in Zeile ${lineNumber}`);
                    result.push({
                        text: rawLine,
                        number: lineNumber
                    });
                } else {
                    stack.pop();

                    result.push({
                        text: `${indent}command = pop`,
                        number: lineNumber
                    });
                }

                i++;
                continue;
            }

            result.push({
                text: substituteLoopVariables(rawLine, scope),
                number: lineNumber
            });

            i++;
        }

        if (stack.length > 0) {
            for (const lineStart of stack) {
                errors.push(`Fehlendes Schlüsselwort 'end' für Gruppe (group) ab Zeile ${lineStart}`);
            }
        }

        return result;
    }

    const numberedLines = source
        .split(/\r?\n/)
        .map((text, index) => ({
            text,
            number: index + 1
        }));

    const mainLines = collectDefinitions(numberedLines);
    const expandedLines = expand(mainLines);

    return {
        output: expandedLines.map(line => line.text).join('\n'),
        errors,
        lineMap: expandedLines.map(line => line.number)
    };
}

function splitArgs(line) {
    const args = [];
    let current = '';
    let depth = 0;

    for (let char of line) {
        if (char === ',' && depth === 0) {
            args.push(current.trim());
            current = '';
        } else {
            if (char === '(') depth++;
            if (char === ')') depth--;
            current += char;
        }
    }

    if (current.trim()) args.push(current.trim());
    return args;
}

function replaceGroupsWithPushPop(text) {
    const lines = text.split(/\r?\n/);
    const stack = [];
    const output = [];
    let errors = [];

    for (let i = 0; i < lines.length; i++) {
        const originalLine = lines[i];
        const trimmed = originalLine.trim();

        if (trimmed === 'group') {
            stack.push(i);
            const indent = originalLine.match(/^\s*/)[0] ?? '';
            output.push(`${indent}command = push`);
        } else if (trimmed === 'end') {
            if (stack.length === 0) {
                errors.push(`Überzähliges Schlüsselwort 'end' in Zeile ${i + 1}`);
                output.push(originalLine); // keep original
            } else {
                stack.pop();
                const indent = originalLine.match(/^\s*/)[0] ?? '';
                output.push(`${indent}command = pop`);
            }
        } else {
            output.push(originalLine);
        }
    }

    if (stack.length > 0) {
        for (const lineIndex of stack) {
            errors.push(`Unclosed 'group' starting at line ${lineIndex + 1}`);
        }
    }

    return {
        output: output.join('\n'),
        errors
    };
}

function parseSceneINI(text) {
    let errors = [];

    let temp = preprocessSceneINI(text);
    text = temp.output;
    errors = temp.errors;
    const lineMap = temp.lineMap;

    const objects = [];
    let current = { transform: [], _lineStart: 1 };

    function currentHasContent() {
        const keys = Object.keys(current).filter(key => key !== 'transform' && key !== '_lineStart');
        return keys.length > 0 || current.transform.length > 0;
    }

    function pushCurrentAndStartNew(lineNumber) {
        if (currentHasContent()) {
            objects.push(current);
        }
        current = { transform: [], _lineStart: lineNumber };
    }

    const lines = text.split(/\r?\n/);

    lines.forEach((line, index) => {
        const lineNumber = lineMap?.[index] ?? (index + 1);
        let trimmed = line.trim();

        if (trimmed.startsWith('#') || trimmed.startsWith(';')) return;

        if (!trimmed) {
            if (currentHasContent()) {
                objects.push(current);
                current = { transform: [], _lineStart: lineNumber + 1 };
            }
            return;
        }

        if (!trimmed.includes('=') && trimmed.includes(' ')) {
            const firstSpace = trimmed.indexOf(' ');
            line = trimmed.slice(0, firstSpace) + '=' + trimmed.slice(firstSpace + 1);
            trimmed = line.trim();
        }

        const [keyRaw, ...rest] = trimmed.split('=');
        if (!keyRaw || rest.length === 0) {
            if (keyRaw !== 'group' && keyRaw !== 'end') {
                errors.push(`Syntaxfehler in Zeile ${lineNumber}: fehlendes '='`);
                return;
            }
        }

        const key = keyRaw.trim();
        const rawValue = rest.join('=').trim();

        let value;

        if (key === 'text') {
            // Text may contain commas, so do not split it into arguments.
            value = rawValue;
        } else {
            value = splitArgs(rawValue);
            if (value.length === 1) value = value[0];
        }

        if (key === 'command' && (value === 'push' || value === 'pop')) {
            pushCurrentAndStartNew(lineNumber);
            current.command = value;
            current._command_line = lineNumber;
            return;
        }

        if (key === 'shape') {
            if (!BUILTIN_SHAPES.has(value)) {
                errors.push(
                    `Ungültige Form (shape) in Zeile ${lineNumber}: "${value}". ` +
                    `Gültige Werte sind: ${[...BUILTIN_SHAPES].join(', ')}.`
                );
            }

            pushCurrentAndStartNew(lineNumber);
        }

        if (key === 'model') {
            pushCurrentAndStartNew(lineNumber);
        }

        if (key === 'text') {
            pushCurrentAndStartNew(lineNumber);
        }

        if (key === 'shade') {
            if (value !== 'off' && value !== 'on') {
                try {
                    eval(value);
                } catch (e) {
                    errors.push(`Ungültiger Wert für Schattierung (shade) in Zeile ${lineNumber}: "${value}". Gültige Werte sind: off, on.`);
                }
            }
        }

        if (key === 'background') {
            if (!isValidSceneColorValue(value)) {
                errors.push(`Ungültiger Wert für Hintergrundfarbe (background) in Zeile ${lineNumber}: "${Array.isArray(value) ? value.join(', ') : value}".`);
            }
        }

        if (key === 'fill') {
            if (value !== 'off' && value !== 'shade' && !isValidSceneColorValue(value)) {
                errors.push(`Ungültiger Wert für Füllfarbe (fill) in Zeile ${lineNumber}: "${Array.isArray(value) ? value.join(', ') : value}".`);
            }
        }

        if (key === 'stroke') {
            if (value !== 'off' && !isValidSceneColorValue(value)) {
                errors.push(`Ungültiger Wert für Strichfarbe (stroke) in Zeile ${lineNumber}: "${Array.isArray(value) ? value.join(', ') : value}".`);
            }
        }

        if (key === 'move' || key === 'rotate' || key === 'scale') {
            if (typeof value === 'string') {
                value = [value, value, value];
            }
            current.transform.push({ type: key, value, _line: lineNumber });
        } else {
            current[key] = value;
            current[`_${key}_line`] = lineNumber;
        }
    });

    if (currentHasContent()) {
        objects.push(current);
    }

    objects.forEach(obj => {
        obj.transform ??= [];
        obj.transform.reverse();
    });

    return { objects, errors };
}

function normalizeSceneFileName(name) {
    let file = (name || '').trim();

    if (file === '') {
        file = 'scene.ini';
    }

    file = file.replace(/\\/g, '/');
    file = file.replace(/[?#].*$/, '');
    file = file.replace(/^\/+/, '');

    if (!file.toLowerCase().endsWith('.ini')) {
        file += '.ini';
    }

    return file;
}

function preload() {
    window.anaglyph_fonts = {};
    window.anaglyph_fonts.OpenSans = loadFont(DEFAULT_TEXT_FONT_PATH);
    fontBytes.OpenSans = loadBytes(DEFAULT_TEXT_FONT_PATH);

    const params = new URLSearchParams(window.location.search);
    const sceneFile = normalizeSceneFileName(params.get('scene') || 'scene.ini');

    fetch(sceneFile)
    .then(response => {
        if (!response.ok) {
            throw new Error(`not-found`);
        }
        return response.text();
    })
    .then(text => {
        const x = parseSceneINI(text);
        sceneDescription = x.objects;

        for (let entry of sceneDescription) {
            if (entry.model) {
                if (!models[entry.model]) {
                    let path = entry.model;
                    if (path.indexOf('.') < 0)
                        path = path + '.obj';
                    models[entry.model] = loadModel(path, false);
                }

                let kit = entry.model.split('/')[0];
                let model = entry.model.split('/')[1].split('.')[0];

                if (!tex[kit]) {
                    tex[kit] = loadImage(`${kit}/textures/${model}.png`);
                }

                entry.model = models[entry.model];
                entry.tex = tex[kit];
            }
        }

        let errors = x.errors;
        if (errors.length > 0) {
            document.getElementById('errors').style.display = 'block';
            document.getElementById('errors').innerHTML = errors.map(e => `<p>${e}</p>`).join('');
        }
    })
    .catch(error => {
        sceneDescription = [];
        reportRuntimeError(`Die Szene "${sceneFile}" konnte nicht geladen werden. Prüfe, ob die Datei existiert und richtig geschrieben ist.`);
        console.error(`Fehler in ${sceneFile}!`, error);
    });
}

function setup() {
    createCanvas(windowWidth, windowHeight, WEBGL);
    anaglyph = createAnaglyph(this);
    camera = new OrbitCamera();
    // camera = new FlyCamera();

    if (typeof opentype === 'undefined') {
        reportRuntimeError(`opentype.js fehlt. Bitte include/opentype.min.js einbinden.`);
    } else {
        try {
            textFonts.OpenSans = parseOpenTypeFont(fontBytes.OpenSans);
        } catch (error) {
            reportRuntimeError(`Die Schrift ${DEFAULT_TEXT_FONT_PATH} konnte nicht gelesen werden: ${error.message}`);
        }
    }

    if (typeof earcut === 'undefined') {
        reportRuntimeError(`earcut fehlt. Bitte include/earcut.min.js einbinden.`);
    }
}

function windowResized() {
    resizeCanvas(windowWidth, windowHeight);
    anaglyph.init();
}

function draw() {
    anaglyph.draw(scene);
}

function drawGrid(pg) {
    pg.push();
    pg.stroke(180);
    for (let i = -500; i <= 500; i += 100) {
        pg.line(i, 0, -500, i, 0, 500);
        pg.line(-500, 0, i, 500, 0, i);
    }
    pg.pop();
}

function scene(pg) {
    camera.apply(pg);
    renderScene(pg);
}

function drawAxes(pg) {
    pg.push();
    pg.fill(0);
    pg.textFont(window.anaglyph_fonts.OpenSans, 10);

    pg.push();
    pg.strokeWeight(5);
    pg.stroke('#d5291a');
    pg.line(0, 0, 0, 100, 0, 0);
    pg.translate(110, 0, 0);
    pg.rotateY(-camera.rotationY);
    pg.rotateX(-camera.rotationX);
    pg.scale(1, -1, 1);
    pg.text("x", -3, 3);
    pg.pop();

    pg.push();
    pg.strokeWeight(5);
    pg.stroke('#4aa03f');
    pg.line(0, 0, 0, 0, 100, 0);
    pg.translate(0, 110, 0);
    pg.rotateY(-camera.rotationY);
    pg.rotateX(-camera.rotationX);
    pg.scale(1, -1, 1);
    pg.text("y", -3, 3);
    pg.pop();

    pg.push();
    pg.strokeWeight(5);
    pg.stroke('#0d60ae');
    pg.line(0, 0, 0, 0, 0, 100);
    pg.translate(0, 0, 110);
    pg.rotateY(-camera.rotationY);
    pg.rotateX(-camera.rotationX);
    pg.scale(1, -1, 1);
    pg.text("z", -3, 3);
    pg.pop();

    pg.pop();
}

function parseOpenTypeFont(bytesObject) {
    const bytes = bytesObject.bytes;
    const uint8 = bytes instanceof Uint8Array ? bytes : new Uint8Array(bytes);
    const buffer = uint8.buffer.slice(uint8.byteOffset, uint8.byteOffset + uint8.byteLength);
    return opentype.parse(buffer);
}

function reportRuntimeError(message) {
    if (shownRuntimeErrors.has(message)) return;
    shownRuntimeErrors.add(message);

    console.error(message);

    const errors = document.getElementById('errors');
    if (errors) {
        errors.style.display = 'block';
        const p = document.createElement('p');
        p.textContent = message;
        errors.appendChild(p);
    }
}

function evalSceneValue(expression, t) {
    return Function('t', `return (${expression});`)(t);
}

function applyTransforms(pg, transforms, t) {
    for (let tr of transforms ?? []) {
        if (tr.type === 'move') {
            pg.translate(
                evalSceneValue(tr.value[0], t),
                evalSceneValue(tr.value[1], t),
                evalSceneValue(tr.value[2], t)
            );
        } else if (tr.type === 'rotate') {
            pg.rotateX(evalSceneValue(tr.value[0], t) / 180 * Math.PI);
            pg.rotateY(evalSceneValue(tr.value[1], t) / 180 * Math.PI);
            pg.rotateZ(evalSceneValue(tr.value[2], t) / 180 * Math.PI);
        } else if (tr.type === 'scale') {
            pg.scale(
                evalSceneValue(tr.value[0], t),
                evalSceneValue(tr.value[1], t),
                evalSceneValue(tr.value[2], t)
            );
        }
    }
}

function renderExtrudedText(pg, entry, t) {
    const text = String(entry.text ?? '');
    if (text.length === 0) return;

    const fontName = entry.font || DEFAULT_TEXT_FONT;
    const font = textFonts[fontName];

    if (!font) {
        throw new Error(`Schriftart "${fontName}" wurde nicht geladen.`);
    }

    if (typeof earcut === 'undefined') {
        throw new Error(`earcut wurde nicht geladen.`);
    }

    const size = entry.size ? evalSceneValue(entry.size, t) : 100;
    const depth = entry.depth ? evalSceneValue(entry.depth, t) : 20;
    const align = entry.align || 'center';

    const cacheKey = JSON.stringify([fontName, text, size, depth, align]);
    let geometry = extrudedTextCache.get(cacheKey);

    if (!geometry) {
        geometry = createExtrudedTextGeometry(font, text, size, depth, align);
        geometry.gid = `extruded-text-${fontName}-${text}-${size}-${depth}-${align}`;
        extrudedTextCache.set(cacheKey, geometry);
    }

    pg.model(geometry);
}

function createExtrudedTextGeometry(font, text, size, depth, align, detail = DEFAULT_TEXT_DETAIL) {
    const path = font.getPath(text, 0, 0, size);
    let contours = pathToContours(path.commands, detail);

    contours = contours
        .map(cleanContour)
        .filter(contour => contour.length >= 3 && Math.abs(signedArea(contour)) > 0.001);

    const geometry = new p5.Geometry();

    if (contours.length === 0) {
        return geometry;
    }

    normalizeContours(contours, align);

    const infos = computeContourNesting(contours);
    const allPoints = [];

    for (const info of infos) {
        info.start = allPoints.length;
        for (const p of info.points) {
            allPoints.push(p);
        }
    }

    const frontZ = depth / 2;
    const backZ = -depth / 2;

    for (const p of allPoints) {
        geometry.vertices.push(createVector(p.x, p.y, frontZ));
    }

    const backOffset = allPoints.length;

    for (const p of allPoints) {
        geometry.vertices.push(createVector(p.x, p.y, backZ));
    }

    // Front and back faces. Every even-depth contour is treated as a solid island.
    // Its direct odd-depth children are passed to earcut as holes.
    for (const outer of infos.filter(info => info.depth % 2 === 0)) {
        const holes = infos.filter(info => info.parent === outer.index && info.depth === outer.depth + 1);

        const localPoints = [];
        const localToGlobal = [];
        const holeStarts = [];

        for (let i = 0; i < outer.points.length; i++) {
            localPoints.push(outer.points[i]);
            localToGlobal.push(outer.start + i);
        }

        for (const hole of holes) {
            holeStarts.push(localPoints.length);

            for (let i = 0; i < hole.points.length; i++) {
                localPoints.push(hole.points[i]);
                localToGlobal.push(hole.start + i);
            }
        }

        const flat = [];
        for (const p of localPoints) {
            flat.push(p.x, p.y);
        }

        const triangles = earcut(flat, holeStarts, 2);

        for (let i = 0; i < triangles.length; i += 3) {
            const a = triangles[i];
            const b = triangles[i + 1];
            const c = triangles[i + 2];

            const ia = localToGlobal[a];
            const ib = localToGlobal[b];
            const ic = localToGlobal[c];

            const triArea = triangleSignedArea(localPoints[a], localPoints[b], localPoints[c]);

            if (triArea >= 0) {
                geometry.faces.push([ia, ib, ic]);
                geometry.faces.push([backOffset + ia, backOffset + ic, backOffset + ib]);
            } else {
                geometry.faces.push([ia, ic, ib]);
                geometry.faces.push([backOffset + ia, backOffset + ib, backOffset + ic]);
            }
        }
    }

    // Side faces
    // Use separate vertices for the side walls. Otherwise p5 averages normals
    // between front/back faces and side walls, which makes extruded text look melted.
    for (const info of infos) {
        const points = info.points;
        const area = signedArea(points);

        // Outward direction depends on whether this contour is an outer boundary or a hole.
        const reverse = (area > 0) === (info.depth % 2 === 0);

        for (let i = 0; i < points.length; i++) {
            const j = (i + 1) % points.length;

            const p0 = points[i];
            const p1 = points[j];

            const base = geometry.vertices.length;

            geometry.vertices.push(createVector(p0.x, p0.y, frontZ));
            geometry.vertices.push(createVector(p1.x, p1.y, frontZ));
            geometry.vertices.push(createVector(p1.x, p1.y, backZ));
            geometry.vertices.push(createVector(p0.x, p0.y, backZ));

            if (reverse) {
                geometry.faces.push([base + 0, base + 2, base + 1]);
                geometry.faces.push([base + 0, base + 3, base + 2]);
            } else {
                geometry.faces.push([base + 0, base + 1, base + 2]);
                geometry.faces.push([base + 0, base + 2, base + 3]);
            }
        }
    }

    geometry.computeNormals();
    return geometry;
}

function pathToContours(commands, detail = DEFAULT_TEXT_DETAIL) {
    const contours = [];
    let current = [];
    let pen = { x: 0, y: 0 };

    function addPoint(x, y) {
        current.push({ x, y: -y });
    }

    function finishContour() {
        if (current.length > 0) {
            contours.push(current);
            current = [];
        }
    }

    for (const cmd of commands) {
        if (cmd.type === 'M') {
            finishContour();
            addPoint(cmd.x, cmd.y);
            pen = { x: cmd.x, y: cmd.y };
        } else if (cmd.type === 'L') {
            addPoint(cmd.x, cmd.y);
            pen = { x: cmd.x, y: cmd.y };
        } else if (cmd.type === 'Q') {
            const steps = Math.max(1, Math.round(detail));
            for (let i = 1; i <= steps; i++) {
                const t = i / steps;
                const x = quadratic(pen.x, cmd.x1, cmd.x, t);
                const y = quadratic(pen.y, cmd.y1, cmd.y, t);
                addPoint(x, y);
            }
            pen = { x: cmd.x, y: cmd.y };
        } else if (cmd.type === 'C') {
            const steps = Math.max(1, Math.round(detail * 1.5));
            for (let i = 1; i <= steps; i++) {
                const t = i / steps;
                const x = cubic(pen.x, cmd.x1, cmd.x2, cmd.x, t);
                const y = cubic(pen.y, cmd.y1, cmd.y2, cmd.y, t);
                addPoint(x, y);
            }
            pen = { x: cmd.x, y: cmd.y };
        } else if (cmd.type === 'Z') {
            finishContour();
        }
    }

    finishContour();
    return contours;
}

function quadratic(a, b, c, t) {
    return (1 - t) * (1 - t) * a + 2 * (1 - t) * t * b + t * t * c;
}

function cubic(a, b, c, d, t) {
    return (1 - t) ** 3 * a +
        3 * (1 - t) ** 2 * t * b +
        3 * (1 - t) * t ** 2 * c +
        t ** 3 * d;
}

function cleanContour(points) {
    const cleaned = [];

    for (const p of points) {
        const last = cleaned[cleaned.length - 1];

        if (!last || distanceSquared(last, p) > 0.0001) {
            cleaned.push(p);
        }
    }

    if (cleaned.length > 1 && distanceSquared(cleaned[0], cleaned[cleaned.length - 1]) <= 0.0001) {
        cleaned.pop();
    }

    return cleaned;
}

function normalizeContours(contours, align) {
    let minX = Infinity;
    let maxX = -Infinity;
    let minY = Infinity;
    let maxY = -Infinity;

    for (const contour of contours) {
        for (const p of contour) {
            minX = Math.min(minX, p.x);
            maxX = Math.max(maxX, p.x);
            minY = Math.min(minY, p.y);
            maxY = Math.max(maxY, p.y);
        }
    }

    let offsetX;

    if (align === 'left') {
        offsetX = minX;
    } else if (align === 'right') {
        offsetX = maxX;
    } else {
        offsetX = (minX + maxX) / 2;
    }

    const offsetY = (minY + maxY) / 2;

    for (const contour of contours) {
        for (const p of contour) {
            p.x -= offsetX;
            p.y -= offsetY;
        }
    }
}

function computeContourNesting(contours) {
    const infos = contours.map((points, index) => ({
        points,
        index,
        parent: null,
        depth: 0,
        area: signedArea(points)
    }));

    for (const info of infos) {
        const p = info.points[0];
        let bestParent = null;
        let bestArea = Infinity;

        for (const candidate of infos) {
            if (candidate === info) continue;

            const candidateArea = Math.abs(candidate.area);
            const ownArea = Math.abs(info.area);

            if (candidateArea <= ownArea) continue;

            if (pointInPolygon(p, candidate.points) && candidateArea < bestArea) {
                bestParent = candidate.index;
                bestArea = candidateArea;
            }
        }

        info.parent = bestParent;
    }

    for (const info of infos) {
        let depth = 0;
        let parent = info.parent;

        while (parent !== null) {
            depth++;
            parent = infos[parent].parent;
        }

        info.depth = depth;
    }

    return infos;
}

function signedArea(points) {
    let area = 0;

    for (let i = 0; i < points.length; i++) {
        const a = points[i];
        const b = points[(i + 1) % points.length];
        area += a.x * b.y - b.x * a.y;
    }

    return area / 2;
}

function triangleSignedArea(a, b, c) {
    return (b.x - a.x) * (c.y - a.y) - (b.y - a.y) * (c.x - a.x);
}

function pointInPolygon(point, polygon) {
    let inside = false;

    for (let i = 0, j = polygon.length - 1; i < polygon.length; j = i++) {
        const a = polygon[i];
        const b = polygon[j];

        const intersect =
            ((a.y > point.y) !== (b.y > point.y)) &&
            (point.x < (b.x - a.x) * (point.y - a.y) / (b.y - a.y) + a.x);

        if (intersect) inside = !inside;
    }

    return inside;
}

function distanceSquared(a, b) {
    const dx = a.x - b.x;
    const dy = a.y - b.y;
    return dx * dx + dy * dy;
}


function renderTeapot(pg, entry, t) {
    const detail = entry.detail
        ? Math.max(2, Math.min(32, Math.round(evalSceneValue(entry.detail, t))))
        : DEFAULT_TEAPOT_DETAIL;

    const cacheKey = `teapot-${detail}`;
    let geometry = teapotGeometryCache.get(cacheKey);

    if (!geometry) {
        geometry = createTeapotGeometry(detail);
        geometry.gid = cacheKey;
        teapotGeometryCache.set(cacheKey, geometry);
    }

    pg.model(geometry);
}

function createTeapotGeometry(detail = DEFAULT_TEAPOT_DETAIL) {
    const geometry = new p5.Geometry();
    const patches = createTeapotPatches();
    const rawVertices = [];
    const faces = [];

    for (const patch of patches) {
        const base = rawVertices.length;

        for (let uStep = 0; uStep <= detail; uStep++) {
            const u = uStep / detail;

            for (let vStep = 0; vStep <= detail; vStep++) {
                const v = vStep / detail;
                rawVertices.push(evaluateBezierPatch(patch, u, v));
            }
        }

        const row = detail + 1;
        const patchFlip = patchNeedsFlip(patch, detail, rawVertices, base);

        for (let uStep = 0; uStep < detail; uStep++) {
            for (let vStep = 0; vStep < detail; vStep++) {
                const a = base + uStep * row + vStep;
                const b = base + (uStep + 1) * row + vStep;
                const c = base + (uStep + 1) * row + (vStep + 1);
                const d = base + uStep * row + (vStep + 1);

                if (patchFlip) {
                    faces.push([a, c, b]);
                    faces.push([a, d, c]);
                } else {
                    faces.push([a, b, c]);
                    faces.push([a, c, d]);
                }
            }
        }
    }

    const bounds = boundsForPoints(rawVertices);
    const center = {
        x: (bounds.minX + bounds.maxX) / 2,
        y: (bounds.minY + bounds.maxY) / 2,
        z: (bounds.minZ + bounds.maxZ) / 2
    };
    const maxDimension = Math.max(
        bounds.maxX - bounds.minX,
        bounds.maxY - bounds.minY,
        bounds.maxZ - bounds.minZ
    );
    const scale = maxDimension > 0 ? 100 / maxDimension : 1;

    const normalizedVertices = rawVertices.map(p => ({
        x: (p.x - center.x) * scale,
        y: (p.y - center.y) * scale,
        z: (p.z - center.z) * scale
    }));

    // Different Bézier patches touch each other along shared edges, but if each
    // patch keeps its own copy of the seam vertices, p5 computes normals per patch
    // and visible shading seams appear. Weld coincident vertices first so normals
    // are averaged smoothly across patch boundaries.
    const welded = weldVertices(normalizedVertices, 1e-4);

    for (const p of welded.vertices) {
        geometry.vertices.push(createVector(p.x, p.y, p.z));
    }

    for (const face of faces) {
        const remapped = [
            welded.remap[face[0]],
            welded.remap[face[1]],
            welded.remap[face[2]]
        ];

        if (remapped[0] === remapped[1] || remapped[1] === remapped[2] || remapped[0] === remapped[2]) {
            continue;
        }

        geometry.faces.push(remapped);
    }

    geometry.computeNormals();
    return geometry;
}

function createTeapotPatches() {
    const patches = [];

    for (let i = 0; i < TEAPOT_PATCH_DATA.length; i++) {
        patches.push(teapotPatchFromIndices(i, 'none'));
        patches.push(teapotPatchFromIndices(i, 'mirrorY'));

        // Rim, body, lid, and bottom are mirrored in both x and y.
        // Handle and spout are only mirrored across y.
        if (i < 6) {
            patches.push(teapotPatchFromIndices(i, 'mirrorX'));
            patches.push(teapotPatchFromIndices(i, 'mirrorXY'));
        }
    }

    return patches;
}

function teapotPatchFromIndices(patchIndex, mirrorMode) {
    const indices = TEAPOT_PATCH_DATA[patchIndex];
    const patch = [];

    for (let row = 0; row < 4; row++) {
        const patchRow = [];

        for (let col = 0; col < 4; col++) {
            let sourceCol = col;

            if (mirrorMode === 'mirrorY' || mirrorMode === 'mirrorX') {
                sourceCol = 3 - col;
            }

            const source = TEAPOT_CONTROL_POINTS[indices[row * 4 + sourceCol]];
            let x = source[0];
            let y = source[1];
            let z = source[2];

            if (mirrorMode === 'mirrorY' || mirrorMode === 'mirrorXY') y = -y;
            if (mirrorMode === 'mirrorX' || mirrorMode === 'mirrorXY') x = -x;

            // The original data uses z as the vertical axis. The scene system uses y.
            // Keep z positive as positive y so the teapot stands upright in this scene.
            patchRow.push({ x, y: z, z: y });
        }

        patch.push(patchRow);
    }

    return patch;
}

function evaluateBezierPatch(patch, u, v) {
    const bu = bernstein3(u);
    const bv = bernstein3(v);
    const p = { x: 0, y: 0, z: 0 };

    for (let i = 0; i < 4; i++) {
        for (let j = 0; j < 4; j++) {
            const weight = bu[i] * bv[j];
            p.x += patch[i][j].x * weight;
            p.y += patch[i][j].y * weight;
            p.z += patch[i][j].z * weight;
        }
    }

    return p;
}

function bernstein3(t) {
    const mt = 1 - t;
    return [
        mt * mt * mt,
        3 * t * mt * mt,
        3 * t * t * mt,
        t * t * t
    ];
}


function patchNeedsFlip(patch, detail, rawVertices, base) {
    const u = 0.5;
    const v = 0.5;
    const deriv = evaluateBezierPatchDerivatives(patch, u, v);
    const surfaceNormal = cross3(deriv.du, deriv.dv);

    const row = detail + 1;
    const midU = Math.max(0, Math.min(detail - 1, Math.floor(detail / 2)));
    const midV = Math.max(0, Math.min(detail - 1, Math.floor(detail / 2)));
    const a = rawVertices[base + midU * row + midV];
    const b = rawVertices[base + (midU + 1) * row + midV];
    const c = rawVertices[base + (midU + 1) * row + (midV + 1)];
    const triNormal = cross3(sub3(b, a), sub3(c, a));

    return dot3(surfaceNormal, triNormal) < 0;
}

function evaluateBezierPatchDerivatives(patch, u, v) {
    const bu = bernstein3(u);
    const bv = bernstein3(v);
    const dbu = bernstein3Derivative(u);
    const dbv = bernstein3Derivative(v);
    const du = { x: 0, y: 0, z: 0 };
    const dv = { x: 0, y: 0, z: 0 };

    for (let i = 0; i < 4; i++) {
        for (let j = 0; j < 4; j++) {
            const p = patch[i][j];
            const wu = dbu[i] * bv[j];
            const wv = bu[i] * dbv[j];
            du.x += p.x * wu;
            du.y += p.y * wu;
            du.z += p.z * wu;
            dv.x += p.x * wv;
            dv.y += p.y * wv;
            dv.z += p.z * wv;
        }
    }

    return { du, dv };
}

function bernstein3Derivative(t) {
    const omt = 1 - t;
    return [
        -3 * omt * omt,
        3 * omt * omt - 6 * omt * t,
        6 * omt * t - 3 * t * t,
        3 * t * t
    ];
}

function sub3(a, b) {
    return { x: a.x - b.x, y: a.y - b.y, z: a.z - b.z };
}

function cross3(a, b) {
    return {
        x: a.y * b.z - a.z * b.y,
        y: a.z * b.x - a.x * b.z,
        z: a.x * b.y - a.y * b.x
    };
}

function dot3(a, b) {
    return a.x * b.x + a.y * b.y + a.z * b.z;
}

function boundsForPoints(points) {
    const bounds = {
        minX: Infinity, maxX: -Infinity,
        minY: Infinity, maxY: -Infinity,
        minZ: Infinity, maxZ: -Infinity
    };

    for (const p of points) {
        bounds.minX = Math.min(bounds.minX, p.x);
        bounds.maxX = Math.max(bounds.maxX, p.x);
        bounds.minY = Math.min(bounds.minY, p.y);
        bounds.maxY = Math.max(bounds.maxY, p.y);
        bounds.minZ = Math.min(bounds.minZ, p.z);
        bounds.maxZ = Math.max(bounds.maxZ, p.z);
    }

    return bounds;
}

function weldVertices(vertices, epsilon = 1e-4) {
    const map = new Map();
    const unique = [];
    const remap = new Array(vertices.length);
    const inv = 1 / epsilon;

    for (let i = 0; i < vertices.length; i++) {
        const p = vertices[i];
        const key = [
            Math.round(p.x * inv),
            Math.round(p.y * inv),
            Math.round(p.z * inv)
        ].join(',');

        let index = map.get(key);
        if (index === undefined) {
            index = unique.length;
            unique.push(p);
            map.set(key, index);
        }

        remap[i] = index;
    }

    return { vertices: unique, remap };
}

function facePointsOutward(face, vertices) {
    const a = vertices[face[0]];
    const b = vertices[face[1]];
    const c = vertices[face[2]];

    const ux = b.x - a.x;
    const uy = b.y - a.y;
    const uz = b.z - a.z;
    const vx = c.x - a.x;
    const vy = c.y - a.y;
    const vz = c.z - a.z;

    const nx = uy * vz - uz * vy;
    const ny = uz * vx - ux * vz;
    const nz = ux * vy - uy * vx;

    const cx = (a.x + b.x + c.x) / 3;
    const cy = (a.y + b.y + c.y) / 3;
    const cz = (a.z + b.z + c.z) / 3;

    return nx * cx + ny * cy + nz * cz >= 0;
}

/*
 * Utah teapot patch/control-point data adapted from the classic GLUT/freeglut
 * teapot. Keep the attribution if you copy this data elsewhere.
 *
 * freeglut_teapot_data.h copyright notice:
 * Copyright (c) 1999-2000 Pawel W. Olszta. All Rights Reserved.
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions: The above copyright notice and this
 * permission notice shall be included in all copies or substantial portions of
 * the Software. THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND.
 *
 * Original GLUT teapot code copyright notice:
 * (c) Copyright 1993, Silicon Graphics, Inc. ALL RIGHTS RESERVED.
 * Permission to use, copy, modify, and distribute this software for any purpose
 * and without fee is hereby granted, provided that the above copyright notice
 * appear in all copies and that both the copyright notice and this permission
 * notice appear in supporting documentation, and that the name of Silicon
 * Graphics, Inc. not be used in advertising or publicity pertaining to
 * distribution of the software without specific, written prior permission.
 * THE MATERIAL EMBODIED ON THIS SOFTWARE IS PROVIDED TO YOU "AS-IS" AND WITHOUT
 * WARRANTY OF ANY KIND.
 */
const TEAPOT_PATCH_DATA = [
    [102, 103, 104, 105, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15],
    [12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27],
    [24, 25, 26, 27, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40],
    [96, 96, 96, 96, 97, 98, 99, 100, 101, 101, 101, 101, 0, 1, 2, 3],
    [0, 1, 2, 3, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117],
    [118, 118, 118, 118, 124, 122, 119, 121, 123, 126, 125, 120, 40, 39, 38, 37],
    [41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56],
    [53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 28, 65, 66, 67],
    [68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83],
    [80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95]
];

const TEAPOT_CONTROL_POINTS = [
    [0.2, 0, 2.7], [0.2, -0.112, 2.7], [0.112, -0.2, 2.7], [0, -0.2, 2.7],
    [1.3375, 0, 2.53125], [1.3375, -0.749, 2.53125], [0.749, -1.3375, 2.53125], [0, -1.3375, 2.53125],
    [1.4375, 0, 2.53125], [1.4375, -0.805, 2.53125], [0.805, -1.4375, 2.53125], [0, -1.4375, 2.53125],
    [1.5, 0, 2.4], [1.5, -0.84, 2.4], [0.84, -1.5, 2.4], [0, -1.5, 2.4],
    [1.75, 0, 1.875], [1.75, -0.98, 1.875], [0.98, -1.75, 1.875], [0, -1.75, 1.875],
    [2, 0, 1.35], [2, -1.12, 1.35], [1.12, -2, 1.35], [0, -2, 1.35],
    [2, 0, 0.9], [2, -1.12, 0.9], [1.12, -2, 0.9], [0, -2, 0.9],
    [-2, 0, 0.9], [2, 0, 0.45], [2, -1.12, 0.45], [1.12, -2, 0.45], [0, -2, 0.45],
    [1.5, 0, 0.225], [1.5, -0.84, 0.225], [0.84, -1.5, 0.225], [0, -1.5, 0.225],
    [1.5, 0, 0.15], [1.5, -0.84, 0.15], [0.84, -1.5, 0.15], [0, -1.5, 0.15],
    [-1.6, 0, 2.025], [-1.6, -0.3, 2.025], [-1.5, -0.3, 2.25], [-1.5, 0, 2.25],
    [-2.3, 0, 2.025], [-2.3, -0.3, 2.025], [-2.5, -0.3, 2.25], [-2.5, 0, 2.25],
    [-2.7, 0, 2.025], [-2.7, -0.3, 2.025], [-3, -0.3, 2.25], [-3, 0, 2.25],
    [-2.7, 0, 1.8], [-2.7, -0.3, 1.8], [-3, -0.3, 1.8], [-3, 0, 1.8],
    [-2.7, 0, 1.575], [-2.7, -0.3, 1.575], [-3, -0.3, 1.35], [-3, 0, 1.35],
    [-2.5, 0, 1.125], [-2.5, -0.3, 1.125], [-2.65, -0.3, 0.9375], [-2.65, 0, 0.9375],
    [-2, -0.3, 0.9], [-1.9, -0.3, 0.6], [-1.9, 0, 0.6],
    [1.7, 0, 1.425], [1.7, -0.66, 1.425], [1.7, -0.66, 0.6], [1.7, 0, 0.6],
    [2.6, 0, 1.425], [2.6, -0.66, 1.425], [3.1, -0.66, 0.825], [3.1, 0, 0.825],
    [2.3, 0, 2.1], [2.3, -0.25, 2.1], [2.4, -0.25, 2.025], [2.4, 0, 2.025],
    [2.7, 0, 2.4], [2.7, -0.25, 2.4], [3.3, -0.25, 2.4], [3.3, 0, 2.4],
    [2.8, 0, 2.475], [2.8, -0.25, 2.475], [3.525, -0.25, 2.49375], [3.525, 0, 2.49375],
    [2.9, 0, 2.475], [2.9, -0.15, 2.475], [3.45, -0.15, 2.5125], [3.45, 0, 2.5125],
    [2.8, 0, 2.4], [2.8, -0.15, 2.4], [3.2, -0.15, 2.4], [3.2, 0, 2.4],
    [0, 0, 3.15], [0.8, 0, 3.15], [0.8, -0.45, 3.15], [0.45, -0.8, 3.15], [0, -0.8, 3.15],
    [0, 0, 2.85], [1.4, 0, 2.4], [1.4, -0.784, 2.4], [0.784, -1.4, 2.4], [0, -1.4, 2.4],
    [0.4, 0, 2.55], [0.4, -0.224, 2.55], [0.224, -0.4, 2.55], [0, -0.4, 2.55],
    [1.3, 0, 2.55], [1.3, -0.728, 2.55], [0.728, -1.3, 2.55], [0, -1.3, 2.55],
    [1.3, 0, 2.4], [1.3, -0.728, 2.4], [0.728, -1.3, 2.4], [0, -1.3, 2.4],
    [0, 0, 0], [1.425, -0.798, 0], [1.5, 0, 0.075], [1.425, 0, 0],
    [0.798, -1.425, 0], [0, -1.5, 0.075], [0, -1.425, 0], [1.5, -0.84, 0.075], [0.84, -1.5, 0.075]
];

function renderScene(pg) {
    pg.background(255);

    if (camera.enableAxes) {
        drawAxes(pg);
    }

    pg.strokeWeight(2);
    pg.stroke(0);
    pg.fill(255);

    let t = millis() / 1000;
    if (typeof sceneDescription === 'undefined') return;

    for (let entry of sceneDescription) {
        try {
            if (entry.command === 'push') {
                pg.push();
                applyTransforms(pg, entry.transform ?? [], t);
            }

            if (entry.background) {
                applySceneColor(pg, 'background', entry.background, t);
            }

            if (entry.strokeWeight) {
                pg.strokeWeight(eval(entry.strokeWeight));
            }

            if (entry.stroke) {
                if (entry.stroke === 'off') {
                    pg.noStroke();
                } else {
                    applySceneColor(pg, 'stroke', entry.stroke, t);
                }
            }

            if (entry.fill) {
                if (entry.fill === 'off') {
                    pg.noFill();
                } else if (entry.fill === 'shade') {
                    pg.noLights();
                    pg.ambientLight(64);
                    pg.directionalLight(255, 255, 255, 0.5, 0.5, -1);
                    pg.fill(255);
                } else {
                    applySceneColor(pg, 'fill', entry.fill, t);
                }
            }

            if (entry.shade) {
                pg.noLights();
                if (entry.shade === 'off') {
                    // no lights
                } else {
                    pg.ambientLight(64);
                    pg.directionalLight(255, 255, 255, 0.5, 0.5, -1);
                }
            }

            if (entry.anaglyph) {
                if (firstFrame) {
                    camera.enableAnaglyph = (entry.anaglyph === 'on');
                }
            }

            anaglyph.shaderLoaded = camera.enableAnaglyph;

            if (entry.shape || entry.model) {
                pg.push();
                applyTransforms(pg, entry.transform ?? [], t);

                if (entry.shape === 'sphere') {
                    pg.sphere();
                } else if (entry.shape === 'box') {
                    pg.box();
                } else if (entry.shape === 'torus') {
                    pg.torus(50, 20);
                } else if (entry.shape === 'cone') {
                    pg.cone(50, 100);
                } else if (entry.shape === 'cylinder') {
                    pg.cylinder(50, 100);
                } else if (entry.shape === 'plane') {
                    pg.plane(100);
                } else if (entry.shape === 'grid') {
                    drawGrid(pg);
                } else if (entry.shape === 'teapot') {
                    renderTeapot(pg, entry, t);
                }

                if (entry.model) {
                    if (entry.model instanceof p5.Geometry) {
                        pg.texture(entry.tex);
                        pg.scale(100, 100, 100);
                        pg.model(entry.model);
                    }
                }

                pg.pop();
            }

            if (entry.text !== undefined) {
                pg.push();
                applyTransforms(pg, entry.transform ?? [], t);
                renderExtrudedText(pg, entry, t);
                pg.pop();
            }

            if (entry.command === 'pop') {
                pg.pop();
            }
        } catch (error) {
            const lineInfo = entry._lineStart ? `Zeile ${entry._lineStart}: ` : '';
            reportRuntimeError(`${lineInfo}${error.message ?? error}`);
        }
    }

    firstFrame = false;
}

function isValidSceneColorValue(value) {
    if (Array.isArray(value)) {
        return value.every(part => canEvaluateSceneExpression(part));
    }

    return isValidColor(value) || canEvaluateSceneExpression(value);
}

function canEvaluateSceneExpression(expression) {
    try {
        evalSceneValue(expression, 0);
        return true;
    } catch (e) {
        return false;
    }
}

function applySceneColor(pg, method, value, t) {
    const args = sceneColorArgs(value, t);
    pg[method](...args);
}

function sceneColorArgs(value, t) {
    if (Array.isArray(value)) {
        return value.map(part => evalSceneValue(part, t) * 255);
    }

    if (isValidColor(value)) {
        return [value];
    }

    return [evalSceneValue(value, t) * 255];
}

function isValidColor(str) {
    if (typeof str !== 'string') return false;

    const s = new Option().style;
    s.color = str;
    return s.color !== '';
}

function normalizeSceneFileName(name) {
    let file = (name || '').trim();

    if (file === '') {
        file = 'scene.ini';
    }

    file = file.replace(/\\/g, '/');
    file = file.replace(/^\/+/, '');

    if (!file.toLowerCase().endsWith('.ini')) {
        file += '.ini';
    }

    return file;
}

window.addEventListener('DOMContentLoaded', function(e) {
    const params = new URLSearchParams(window.location.search);
    const sceneFile = normalizeSceneFileName(params.get('scene') || 'scene.ini');

    if (params.get('scene') === null) {
        params.set('scene', sceneFile);
        window.history.replaceState({}, '', `${window.location.pathname}?${params.toString()}`);
    }

    const sceneInput = document.querySelector('#scene-file');
    const sceneLoader = document.querySelector('#scene-loader');

    function loadSceneFromInput() {
        const nextSceneFile = normalizeSceneFileName(sceneInput.value);

        const nextParams = new URLSearchParams(window.location.search);
        nextParams.set('scene', nextSceneFile);

        window.location.search = `?${nextParams.toString()}`;
    }

    if (sceneInput) {
        sceneInput.value = sceneFile;

        sceneInput.addEventListener('focus', function() {
            setTimeout(() => sceneInput.select(), 0);
        });

        sceneInput.addEventListener('click', function() {
            setTimeout(() => sceneInput.select(), 0);
        });

        sceneInput.addEventListener('keydown', function(e) {
            if (e.key === 'Enter') {
                e.preventDefault();
                loadSceneFromInput();
            }
        });
    }

    if (sceneLoader) {
        sceneLoader.addEventListener('submit', function(e) {
            e.preventDefault();
            loadSceneFromInput();
        });
    }

    document.querySelector('#bu-anaglyph').addEventListener('click', function(e) {
        camera.enableAnaglyph = !camera.enableAnaglyph;
        anaglyph.shaderLoaded = camera.enableAnaglyph;
    });

    document.querySelector('#bu-axes').addEventListener('click', function(e) {
        camera.enableAxes = !camera.enableAxes;
    });
});