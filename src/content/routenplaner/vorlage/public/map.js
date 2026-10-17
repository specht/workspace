// Die Karte: gezeichnet aus den Daten, die server.rb unter map.json liefert.
// Ziehen verschiebt sie, das Mausrad, + / − und die Pfeiltasten bewegen sie,
// ein Klick fragt den Server nach dem Knoten an dieser Stelle.

const canvas = document.getElementById('map');
const context = canvas.getContext('2d');
const info = document.getElementById('info');

// Wie eine Art von Weg aussieht: Farbe, Breite in Pixeln, gestrichelt
const STYLES = {
    motorway: ['#f2a07b', 4.4], trunk: ['#f7b98b', 4], primary: ['#fdd889', 3.6], secondary: ['#fdeaa8', 3.2],
    tertiary: ['#ffffff', 2.8], residential: ['#ffffff', 2.4], living_street: ['#ffffff', 2.4],
    unclassified: ['#ffffff', 2.4], pedestrian: ['#f4f0f7', 2.4], service: ['#ffffff', 1.5],
    footway: ['#c9a497', 0.9, [2, 2]], path: ['#88b083', 0.9, [2, 2]], cycleway: ['#7b9fdc', 0.9, [3, 2]],
    steps: ['#b98576', 1.6, [1, 1.2]], track: ['#b59a6d', 1, [4, 2]], bridleway: ['#88b083', 0.9, [3, 2]],
};
// Straßen haben einen Rand, damit die weißen sich vom Grund abheben
const CASING = { motorway: '#d0714f', trunk: '#d78e5c', primary: '#d9a54a', secondary: '#dcc072',
    tertiary: '#bdc3cb', residential: '#c7ccd3', living_street: '#c7ccd3', unclassified: '#c7ccd3',
    pedestrian: '#cbc5d6', service: '#d3d8de' };
// Auffahrten (motorway_link, …) wie ihre Straße, nur schmaler
for (const kind of ['motorway', 'trunk', 'primary', 'secondary', 'tertiary']) {
    STYLES[`${kind}_link`] = [STYLES[kind][0], STYLES[kind][1] * 0.7];
    CASING[`${kind}_link`] = CASING[kind];
}
const OTHER = ['#b8bec7', 1];
const BACKGROUND = '#eef0f2';
// Wichtige Straßen zuletzt, damit sie oben liegen
const ORDER = ['footway', 'path', 'cycleway', 'steps', 'track', 'bridleway', 'service', 'pedestrian',
    'living_street', 'unclassified', 'residential', 'tertiary_link', 'tertiary', 'secondary_link', 'secondary',
    'primary_link', 'primary', 'trunk_link', 'trunk', 'motorway_link', 'motorway'];
const LEGEND = [['primary', 'Hauptstraße'], ['secondary', 'Nebenstraße'], ['residential', 'Wohnstraße'],
    ['footway', 'Fußweg'], ['path', 'Weg im Grünen'], ['cycleway', 'Radweg'], ['steps', 'Treppe'], ['track', 'Feldweg']];
const START = '#2f9e44';
const GOAL = '#e03131';
const METRES_PER_DEGREE = 2 * Math.PI * 6371000 / 360;

let data = null;
let view = { lon: 13.33, lat: 52.455, scale: 40000 }; // scale: Pixel je Grad Breite
let marks = []; // [[lon, lat, Farbe, Radius], …]
let chosen = { start: null, goal: null }; // die gewählten Orte
let lines = []; // [[[lon, lat], [lon, lat]], …] hervorgehoben
let fitted = null; // was ganz zu sehen sein soll, bis jemand die Karte bewegt: [West, Süd, Ost, Nord, Zoom höchstens]
let search = 'bfs'; // welche Suche aus routes.rb
let graph = null; // { nodes: [lon, lat, …], edges: [Nummer, Nummer, …] } aus build_graph, wenn »Graph zeigen« an ist
let route = null; // { path: [[lon, lat], …], visited: [lon, lat, lon, lat, …], edges: [Nummer, Nummer, …] in visited, shown: wie viele besuchte schon zu sehen }

// Länge und Breite auf Pixel: ein Grad Länge ist um cos(Breite) kürzer
function project(lon, lat) {
    const k = Math.cos(view.lat * Math.PI / 180);
    return [canvas.clientWidth / 2 + (lon - view.lon) * k * view.scale, canvas.clientHeight / 2 - (lat - view.lat) * view.scale];
}

function unproject(x, y) {
    const k = Math.cos(view.lat * Math.PI / 180);
    return [view.lon + (x - canvas.clientWidth / 2) / (k * view.scale), view.lat - (y - canvas.clientHeight / 2) / view.scale];
}

// project() als Zahlen zum Selberrechnen, für die Schleifen über
// zehntausende Punkte: x = ox + Länge · ax, y = oy − Breite · ay
function projection() {
    const ax = Math.cos(view.lat * Math.PI / 180) * view.scale;
    const ay = view.scale;
    return { ax, ay, ox: canvas.clientWidth / 2 - view.lon * ax, oy: canvas.clientHeight / 2 + view.lat * ay };
}

// Was zu sehen ist, als Länge und Breite, mit etwas Rand
function viewBox() {
    const [west, north] = unproject(-20, -20);
    const [east, south] = unproject(canvas.clientWidth + 20, canvas.clientHeight + 20);
    return [west, south, east, north];
}

// Von weitem bleiben die kleinen Wege weg: Sie wären kaum zu sehen und
// kosten beim Zeichnen am meisten
function shown(kind) {
    if (STYLES[kind]?.[2]) return view.scale > 40000;
    if (kind === 'service') return view.scale > 20000;
    return true;
}

// Die Karte selbst (Grund, Wege, Graph) liegt in einem Bild für sich: Solange
// sich nichts an ihr ändert, wird sie nur kopiert, etwa während die Suche läuft
const base = document.createElement('canvas');
const baseContext = base.getContext('2d');
let baseKey = '';
let baseView = null; // die Ansicht, für die die Karte zuletzt gezeichnet wurde

function fitSize(target, ratio) {
    const width = Math.round(canvas.clientWidth * ratio);
    const height = Math.round(canvas.clientHeight * ratio);
    if (target.width !== width || target.height !== height) {
        target.width = width;
        target.height = height;
    }
}

// Beim Ziehen und Zoomen: höchstens einmal je Bild des Bildschirms, und nur
// das letzte Bild der Karte verschoben und vergrößert. Ganz neu gezeichnet
// wird sie erst, wenn die Bewegung kurz innehält.
let drawing = false;
let settle = null;
function requestDraw() {
    clearTimeout(settle);
    settle = setTimeout(draw, 150);
    if (drawing) return;
    drawing = true;
    requestAnimationFrame(() => {
        drawing = false;
        draw(true);
    });
}

function draw(quick = false) {
    // so viele Pixel, wie der Bildschirm hat: sonst wird die Karte unscharf
    const ratio = window.devicePixelRatio || 1;
    fitSize(canvas, ratio);
    drawScale();
    const key = [view.lon, view.lat, view.scale, canvas.width, canvas.height, ratio,
        Boolean(data), Boolean(route), graph ? graph.nodes.length : 0].join();
    const same = baseView && base.width === canvas.width && base.height === canvas.height;
    if (quick && same && key !== baseKey) {
        // das alte Bild dorthin, wo seine Punkte jetzt liegen
        context.setTransform(ratio, 0, 0, ratio, 0, 0);
        context.fillStyle = BACKGROUND;
        context.fillRect(0, 0, canvas.clientWidth, canvas.clientHeight);
        const w = canvas.clientWidth, h = canvas.clientHeight;
        const k0 = Math.cos(baseView.lat * Math.PI / 180), k1 = Math.cos(view.lat * Math.PI / 180);
        const fx = k1 * view.scale / (k0 * baseView.scale), fy = view.scale / baseView.scale;
        const cx = w / 2 - fx * w / 2 + (baseView.lon - view.lon) * k1 * view.scale;
        const cy = h / 2 - fy * h / 2 - (baseView.lat - view.lat) * view.scale;
        context.drawImage(base, cx, cy, w * fx, h * fy);
    } else {
        if (key !== baseKey) {
            fitSize(base, ratio);
            baseContext.setTransform(ratio, 0, 0, ratio, 0, 0);
            drawBase(baseContext);
            baseKey = key;
            baseView = { ...view };
        }
        context.setTransform(1, 0, 0, 1, 0, 0);
        context.drawImage(base, 0, 0);
        context.setTransform(ratio, 0, 0, ratio, 0, 0);
    }
    if (!data) return;
    if (route) drawRoute();
    context.strokeStyle = '#d6336c';
    context.lineWidth = 3;
    for (const [a, b] of lines) {
        context.beginPath();
        context.moveTo(...project(...a));
        context.lineTo(...project(...b));
        context.stroke();
    }
    for (const [lon, lat, color, radius] of marks) {
        dot(project(lon, lat), radius, color);
    }
    drawPlaces();
    saveSoon();
}

function drawBase(c) {
    c.fillStyle = BACKGROUND;
    c.fillRect(0, 0, canvas.clientWidth, canvas.clientHeight);
    if (!data) return;
    // die Wege werden mit dem Zoom breiter und schmaler, aber nur gemäßigt
    const thin = Math.min(2.6, Math.max(0.45, Math.sqrt(view.scale / 30000)));
    // Fußwege und Radwege treten von weitem zurück, damit die Straßen zu sehen sind
    const faint = Math.min(1, Math.max(0.45, view.scale / 120000));
    // unter dem Graphen und der Suche bleibt die Karte blass im Hintergrund
    const pale = graph ? 0.35 : route ? 0.6 : 1;
    const { ax, ay, ox, oy } = projection();
    const [west, south, east, north] = viewBox();
    // jeder Weg ein Strich, aber nur, wenn er zu sehen ist
    const trace = (group) => {
        const { ways, boxes } = group;
        for (let w = 0; w < ways.length; w++) {
            if (boxes[4 * w] > east || boxes[4 * w + 2] < west || boxes[4 * w + 1] > north || boxes[4 * w + 3] < south) continue;
            const points = ways[w];
            c.beginPath();
            let x = ox + points[0] * ax, y = oy - points[1] * ay;
            c.moveTo(x, y);
            // Punkte, die nicht einmal einen Pixel weiter liegen, braucht der Strich nicht
            const last = points.length - 2;
            for (let i = 2; i <= last; i += 2) {
                const nx = ox + points[i] * ax, ny = oy - points[i + 1] * ay;
                if (i < last && Math.abs(nx - x) + Math.abs(ny - y) < 1) continue;
                c.lineTo(nx, ny);
                x = nx;
                y = ny;
            }
            c.stroke();
        }
    };
    c.lineCap = 'round';
    c.lineJoin = 'round';
    // erst die Ränder aller Straßen, dann ihre Füllung darüber
    c.setLineDash([]);
    c.globalAlpha = pale;
    for (const group of data.groups) {
        const casing = CASING[group.kind];
        if (!casing || !shown(group.kind)) continue;
        c.strokeStyle = casing;
        c.lineWidth = Math.max(0.6, STYLES[group.kind][1] * thin) + Math.min(2, 0.8 + 0.4 * thin);
        trace(group);
    }
    for (const group of data.groups) {
        if (!shown(group.kind)) continue;
        const [color, width, dash] = STYLES[group.kind] || OTHER;
        // gestrichelte Wege (Fußwege, Radwege) wachsen nur halb so stark
        const grow = dash ? Math.min(thin, 2) : thin;
        c.globalAlpha = (dash ? faint : 1) * pale;
        c.strokeStyle = color;
        c.lineWidth = Math.max(0.6, width * grow);
        // die Striche wachsen mit der Breite, sonst laufen sie beim Zoomen zusammen
        c.setLineDash(dash && view.scale > 60000 ? dash.map((d) => d * grow) : []);
        trace(group);
    }
    c.globalAlpha = 1;
    c.setLineDash([]);
    if (graph) drawGraph(c);
}

// Der Graph, wie build_graph ihn gebaut hat: jede Kante eine Linie, jeder
// Knoten ein Punkt
function drawGraph(c) {
    const { nodes, edges } = graph;
    const { ax, ay, ox, oy } = projection();
    const width = canvas.clientWidth;
    const height = canvas.clientHeight;
    c.strokeStyle = 'rgba(54, 79, 199, 0.75)';
    c.lineWidth = Math.min(2, Math.max(0.7, view.scale / 200000));
    c.beginPath();
    for (let i = 0; i < edges.length; i += 2) {
        const a = 2 * edges[i];
        const b = 2 * edges[i + 1];
        const x1 = ox + nodes[a] * ax, y1 = oy - nodes[a + 1] * ay;
        const x2 = ox + nodes[b] * ax, y2 = oy - nodes[b + 1] * ay;
        if ((x1 < 0 && x2 < 0) || (y1 < 0 && y2 < 0) || (x1 > width && x2 > width) || (y1 > height && y2 > height)) continue;
        c.moveTo(x1, y1);
        c.lineTo(x2, y2);
    }
    c.stroke();
    const radius = Math.min(4, Math.max(0.9, view.scale / 80000));
    c.beginPath();
    for (let i = 0; i < nodes.length; i += 2) {
        const x = ox + nodes[i] * ax, y = oy - nodes[i + 1] * ay;
        if (x < -5 || y < -5 || x > width + 5 || y > height + 5) continue;
        if (radius > 2) {
            c.moveTo(x + radius, y);
            c.arc(x, y, radius, 0, 2 * Math.PI);
        } else {
            c.rect(x - radius, y - radius, 2 * radius, 2 * radius);
        }
    }
    if (radius > 2) {
        c.lineWidth = 1.5;
        c.strokeStyle = '#fff';
        c.stroke();
    }
    c.fillStyle = '#364fc7';
    c.fill();
}

async function showGraph(on) {
    const toggle = document.getElementById('graph-view');
    if (!on) {
        graph = null;
        return draw();
    }
    const answer = await (await fetch('graph.json')).json();
    if (answer.missing) {
        toggle.checked = false;
        info.textContent = `Für den Graphen fehlt noch ${answer.missing} in routes.rb.`;
        return;
    }
    if (!toggle.checked) return;
    graph = answer;
    info.textContent = `Der Graph: ${NUMBER.format(graph.nodes.length / 2)} Knoten, ${NUMBER.format(graph.edges.length / 2)} Kanten`;
    draw();
}

// Die Suche: jeder besuchte Knoten ein Punkt, von Gelb (früh besucht) bis
// Rot (spät), und darüber der gefundene Weg
function drawRoute() {
    const { visited, path, shown: count } = route;
    const total = visited.length / 2;
    const radius = Math.min(3, Math.max(1.1, view.scale / 90000));
    const { ax, ay, ox, oy } = projection();
    const width = canvas.clientWidth;
    const height = canvas.clientHeight;
    // in 20 Farben, jede ein Pfad: viel schneller als jeder Punkt für sich
    const shades = 20;
    const color = (shade, alpha) => `hsla(${52 - 50 * shade / (shades - 1)}, 95%, 50%, ${alpha})`;
    // erst die Kanten, die die Suche angesehen hat, in der Farbe des Knotens,
    // bei dem sie aufleuchten
    const { edges } = route;
    context.lineWidth = Math.min(3.5, Math.max(1.5, view.scale / 70000));
    context.lineCap = 'round';
    let e = 0;
    for (let shade = 0; shade < shades; shade++) {
        const to = Math.min(count, Math.floor(total * (shade + 1) / shades));
        context.strokeStyle = color(shade, 0.7);
        context.beginPath();
        for (; e < edges.length && edges[e + 1] < to; e += 2) {
            const a = 2 * edges[e], b = 2 * edges[e + 1];
            const x1 = ox + visited[a] * ax, y1 = oy - visited[a + 1] * ay;
            const x2 = ox + visited[b] * ax, y2 = oy - visited[b + 1] * ay;
            if ((x1 < 0 && x2 < 0) || (y1 < 0 && y2 < 0) || (x1 > width && x2 > width) || (y1 > height && y2 > height)) continue;
            context.moveTo(x1, y1);
            context.lineTo(x2, y2);
        }
        context.stroke();
        if (to >= count) break;
    }
    for (let shade = 0; shade < shades; shade++) {
        const from = Math.floor(total * shade / shades);
        const to = Math.min(count, Math.floor(total * (shade + 1) / shades));
        if (from >= to) break;
        context.fillStyle = color(shade, 0.85);
        context.beginPath();
        for (let i = from; i < to; i++) {
            const x = ox + visited[2 * i] * ax, y = oy - visited[2 * i + 1] * ay;
            if (x < -5 || y < -5 || x > width + 5 || y > height + 5) continue;
            context.moveTo(x + radius, y);
            context.arc(x, y, radius, 0, 2 * Math.PI);
        }
        context.fill();
    }
    // der Knoten, bei dem die Suche gerade ist
    if (count > 0 && count < total) {
        const i = count - 1;
        const at = [ox + visited[2 * i] * ax, oy - visited[2 * i + 1] * ay];
        context.lineWidth = 2.5;
        context.strokeStyle = '#212529';
        context.beginPath();
        context.arc(at[0], at[1], radius + 5, 0, 2 * Math.PI);
        context.stroke();
        dot(at, radius + 1.5, '#212529');
    }
    if (!path || count < total) return;
    // vom Ort zum nächsten Knoten des Graphen, gestrichelt
    context.setLineDash([3, 4]);
    context.lineWidth = 2;
    context.strokeStyle = '#868e96';
    for (const [place, end] of [[chosen.start, path[0]], [chosen.goal, path[path.length - 1]]]) {
        if (!place) continue;
        context.beginPath();
        context.moveTo(...project(place.lon, place.lat));
        context.lineTo(...project(...end));
        context.stroke();
    }
    context.setLineDash([]);
    for (const [color, width] of [['#fff', 8], ['#5f3dc4', 4.5]]) {
        context.strokeStyle = color;
        context.lineWidth = width;
        context.beginPath();
        path.forEach(([lon, lat], i) => (i ? context.lineTo : context.moveTo).call(context, ...project(lon, lat)));
        context.stroke();
    }
}

function dot([x, y], radius, color, border = 0) {
    context.beginPath();
    context.arc(x, y, radius, 0, 2 * Math.PI);
    if (border) {
        context.lineWidth = border;
        context.strokeStyle = '#fff';
        context.stroke();
    }
    context.fillStyle = color;
    context.fill();
}

// Die Orte: Sehenswürdigkeiten mit Namen, Schulen erst von nah. Ein Name,
// der einen anderen verdecken würde, bleibt weg.
function drawPlaces() {
    const taken = [];
    const label = (text, [x, y], font, color, below = 0) => {
        context.font = font;
        const width = context.measureText(text).width;
        const box = [x - width / 2 - 2, y - 16 + below, x + width / 2 + 2, y - 4 + below];
        if (taken.some((b) => box[0] < b[2] && b[0] < box[2] && box[1] < b[3] && b[1] < box[3])) return;
        taken.push(box);
        context.textAlign = 'center';
        context.lineWidth = 3.5;
        context.strokeStyle = 'rgba(255, 255, 255, 0.9)';
        context.strokeText(text, x, y - 6 + below);
        context.fillStyle = color;
        context.fillText(text, x, y - 6 + below);
    };
    // Start und Ziel zuerst, damit ihre Namen immer zu sehen sind
    const picked = [[chosen.start, START], [chosen.goal, GOAL]].filter(([place]) => place);
    for (const [place, color] of picked) {
        label(place.name, project(place.lon, place.lat), '600 13px system-ui, sans-serif', color, -6);
    }
    for (const kind of ['sight', 'school']) {
        for (const place of data.places) {
            if (place.kind !== kind || place === chosen.start || place === chosen.goal) continue;
            const sight = kind === 'sight';
            if (!sight && view.scale < 60000) continue;
            const at = project(place.lon, place.lat);
            if (at[0] < -100 || at[1] < -20 || at[0] > canvas.clientWidth + 100 || at[1] > canvas.clientHeight + 20) continue;
            dot(at, sight ? 4 : 3, sight ? '#7048e8' : '#4c6ef5', 1.5);
            if (sight) label(place.name, at, '600 12px system-ui, sans-serif', '#5f3dc4');
            else if (view.scale > 120000) label(place.name, at, '11px system-ui, sans-serif', '#364fc7');
        }
    }
    for (const [place, color] of picked) {
        dot(project(place.lon, place.lat), 7, color, 2.5);
    }
}

// Der Maßstab unten links: eine runde Strecke, höchstens 120 Pixel lang
function drawScale() {
    const metresPerPixel = METRES_PER_DEGREE / view.scale;
    const most = 120 * metresPerPixel;
    const power = 10 ** Math.floor(Math.log10(most));
    const metres = [5, 2, 1].map((f) => f * power).find((m) => m <= most);
    const bar = document.querySelector('#scale span');
    bar.style.width = `${metres / metresPerPixel}px`;
    bar.textContent = metres >= 1000 ? `${metres / 1000} km` : `${metres} m`;
}

// Verschieben und zoomen
function zoom(factor, x = canvas.clientWidth / 2, y = canvas.clientHeight / 2) {
    // der Punkt unter der Maus bleibt, wo er ist
    moved();
    const [lon, lat] = unproject(x, y);
    view.scale = Math.min(2000000, Math.max(2000, view.scale * factor));
    const [lon2, lat2] = unproject(x, y);
    view.lon += lon - lon2;
    view.lat += lat - lat2;
    requestDraw();
}

function moved() {
    fitted = null;
}

function pan(dx, dy) {
    moved();
    const k = Math.cos(view.lat * Math.PI / 180);
    view.lon -= dx / (k * view.scale);
    view.lat += dy / view.scale;
    requestDraw();
}

// Zeigt ein Rechteck aus Länge und Breite ganz, mit etwas Rand, rechts vom
// Kasten oben links, wenn das Fenster breit genug ist. Mit cover füllt das
// Rechteck stattdessen das ganze Fenster, auch wenn dafür etwas wegfällt.
function fit(west, south, east, north, most = 2000000, cover = false) {
    fitted = [west, south, east, north, most, cover];
    view.lon = (west + east) / 2;
    view.lat = (south + north) / 2;
    const k = Math.cos(view.lat * Math.PI / 180);
    const across = Math.max(1e-6, (east - west) * k);
    const down = Math.max(1e-6, north - south);
    if (cover) {
        view.scale = Math.min(most, Math.max(canvas.clientWidth / across, canvas.clientHeight / down));
        return draw();
    }
    const panel = document.getElementById('panel').getBoundingClientRect();
    const left = canvas.clientWidth > 2 * panel.width + 200 ? panel.right : 0;
    const width = canvas.clientWidth - left - 200;
    const height = canvas.clientHeight - 160;
    view.scale = Math.min(most, width / across, height / down);
    view.lon -= left / 2 / (k * view.scale);
    draw();
}

// Das Gebiet, so dass die Karte das ganze Fenster füllt
function fitArea() {
    fit(...data.core, 2000000, true);
}

let drag = null;
canvas.addEventListener('pointerdown', (e) => {
    drag = { x: e.clientX, y: e.clientY, moved: false };
    canvas.setPointerCapture(e.pointerId);
});
canvas.addEventListener('pointermove', (e) => {
    if (!drag) return;
    const dx = e.clientX - drag.x;
    const dy = e.clientY - drag.y;
    if (!drag.moved && Math.abs(dx) + Math.abs(dy) < 4) return;
    drag.moved = true;
    canvas.classList.add('dragging');
    drag.x = e.clientX;
    drag.y = e.clientY;
    pan(dx, dy);
});
canvas.addEventListener('pointerup', (e) => {
    canvas.classList.remove('dragging');
    if (drag && !drag.moved && data) clicked(...unproject(e.offsetX, e.offsetY));
    drag = null;
});
canvas.addEventListener('dblclick', (e) => zoom(2, e.offsetX, e.offsetY));
canvas.addEventListener('wheel', (e) => {
    e.preventDefault();
    // ein Mausrad macht große Schritte, ein Touchpad viele kleine
    const step = e.deltaMode === 1 ? e.deltaY * 40 : e.deltaY;
    zoom(Math.exp(-Math.max(-200, Math.min(200, step)) / 400), e.offsetX, e.offsetY);
}, { passive: false });
canvas.addEventListener('keydown', (e) => {
    const moves = { ArrowLeft: [120, 0], ArrowRight: [-120, 0], ArrowUp: [0, 120], ArrowDown: [0, -120] };
    if (moves[e.key]) pan(...moves[e.key]);
    else if (e.key === '+' || e.key === '=') zoom(1.5);
    else if (e.key === '-') zoom(1 / 1.5);
    else if (e.key === '0' && data) fitArea();
    else return;
    e.preventDefault();
});
document.querySelectorAll('#zoom [data-zoom]').forEach((b) => b.addEventListener('click', () => zoom(Number(b.dataset.zoom))));
document.getElementById('fit').addEventListener('click', () => data && fitArea());
// Ein größeres Fenster, ein anderer Bildschirm oder der Zoom des Browsers
// ändert, wie viele Pixel die Karte hat: neu zeichnen, damit sie scharf bleibt
// Ist die Karte noch so, wie sie eingepasst wurde, passt sie sich an die neue
// Größe wieder ein
function resized() {
    if (fitted) fit(...fitted); else draw();
}
window.addEventListener('resize', resized);
new ResizeObserver(() => resized()).observe(canvas, { box: 'device-pixel-content-box' });

// Ein Klick: der Knoten dort und seine Nachbarn (build_graph in routes.rb)
async function clicked(lon, lat) {
    const answer = await (await fetch(`neighbours?lon=${lon}&lat=${lat}`)).json();
    if (answer.missing) {
        info.textContent = `Für Knoten und Nachbarn fehlt noch ${answer.missing} in routes.rb.`;
        return;
    }
    marks = [[...answer.at, '#d6336c', 5], ...answer.neighbours.map(([x, y]) => [x, y, '#1971c2', 4])];
    lines = answer.neighbours.map(([x, y]) => [answer.at, [x, y]]);
    const metres = answer.neighbours.map(([, , m]) => `${Math.round(m)} m`).join(', ');
    info.textContent = `Knoten ${answer.id}: ${answer.neighbours.length} Nachbarn${metres ? ` (${metres})` : ''}`;
    draw();
}

// Start und Ziel: ein Feld zum Suchen, mit allen Orten zur Auswahl
function choose(which, place) {
    const input = document.getElementById(which);
    input.value = place ? place.name : '';
    input.closest('.place').classList.remove('unknown');
    // derselbe Ort noch einmal (etwa wenn das Feld den Fokus verliert): nichts Neues
    if (chosen[which] === place) return;
    chosen[which] = place;
    showChoice();
    findRoute();
    // beide zu sehen, oder der eine in der Mitte
    const both = [chosen.start, chosen.goal].filter((p) => p);
    if (both.length === 2) {
        const lons = both.map((p) => p.lon), lats = both.map((p) => p.lat);
        fit(Math.min(...lons), Math.min(...lats), Math.max(...lons), Math.max(...lats), 200000);
    } else if (place) {
        moved();
        view.lon = place.lon;
        view.lat = place.lat;
        view.scale = Math.max(view.scale, 120000);
        draw();
    } else {
        draw();
    }
}

function length(metres) {
    return metres < 1000 ? `${Math.round(metres)} m` : `${NUMBER.format(Math.round(metres / 100) / 10)} km`;
}

// Wie weit Start und Ziel in Luftlinie auseinander sind
function beeline() {
    const { start, goal } = chosen;
    const k = Math.cos((start.lat + goal.lat) / 2 * Math.PI / 180);
    return Math.hypot((goal.lon - start.lon) * k, goal.lat - start.lat) * METRES_PER_DEGREE;
}

// Was gewählt ist, und wie weit es in Luftlinie ist
function showChoice() {
    const { start, goal } = chosen;
    if (start && goal) {
        info.textContent = `Luftlinie: ${length(beeline())}`;
    } else {
        info.textContent = start || goal ? `Wähle noch ${start ? 'das Ziel' : 'den Start'}.` : 'Wähle Start und Ziel oder klicke auf die Karte.';
    }
}

// Die Route mit der gewählten Suche: erst die Suche Knoten für Knoten, dann der Weg
const SEARCH_NAMES = { bfs: 'Breitensuche', dijkstra: 'Dijkstra' };
let asked = 0;
async function findRoute() {
    pause();
    route = null;
    document.getElementById('player').hidden = true;
    const { start, goal } = chosen;
    if (!start || !goal) return draw();
    const ask = ++asked;
    const name = SEARCH_NAMES[search];
    info.textContent = `${name} sucht …`;
    let answer;
    try {
        const response = await fetch(`route?search=${search}&from=${start.lon},${start.lat}&to=${goal.lon},${goal.lat}`);
        if (!response.ok) throw new Error(response.status);
        answer = await response.json();
    } catch {
        if (ask === asked) info.textContent = `${name}: Fehler in routes.rb. Was schiefging, steht im Terminal.`;
        return;
    }
    if (ask !== asked) return; // inzwischen anders gewählt
    if (answer.missing) {
        info.textContent = `Für ${name === 'Dijkstra' ? 'Dijkstra' : `die ${name}`} fehlt noch ${answer.missing} in routes.rb.`;
        return draw();
    }
    route = { path: answer.path, visited: answer.visited, edges: answer.edges || [], shown: 0, position: 0 };
    showRoute(name, answer);
    const total = answer.visited.length / 2;
    const slider = document.getElementById('progress');
    slider.max = total;
    document.getElementById('player').hidden = false;
    // Wer schon selbst angehalten oder geschoben hat, vergleicht: dieselbe
    // Stelle bei der anderen Suche. Sonst läuft die Suche einmal ab.
    if (player.manual) {
        show(Math.min(player.step, total));
    } else {
        show(0);
        play();
    }
}

function showRoute(name, answer) {
    const seconds = answer.seconds < 0.01 ? NUMBER.format(Number(answer.seconds.toFixed(4)))
        : NUMBER.format(Number(answer.seconds.toFixed(2)));
    const title = document.createElement('strong');
    title.textContent = name;
    const way = document.createElement('span');
    way.textContent = answer.path
        ? `Weg: ${length(answer.metres)} über ${NUMBER.format(answer.path.length)} Knoten (Luftlinie ${length(beeline())})`
        : 'Kein Weg gefunden.';
    const visited = document.createElement('span');
    visited.className = 'note';
    visited.textContent = `Besucht: ${NUMBER.format(answer.visited.length / 2)} Knoten in ${seconds} s, von Gelb (früh) bis Rot (spät)`;
    info.replaceChildren(title, way, visited);
}

// Die Suche zum Zusehen: abspielen, anhalten, Schritt für Schritt oder mit
// dem Schieber an jede Stelle
const player = { playing: false, manual: false, step: 0, last: 0 };
const SPEEDS = { slow: 25, medium: 500 }; // Knoten je Sekunde; schnell: alles in zwei Sekunden

function show(step) {
    if (!route) return;
    const total = route.visited.length / 2;
    route.shown = Math.max(0, Math.min(total, Math.round(step)));
    route.position = route.shown;
    player.step = route.shown;
    document.getElementById('progress').value = route.shown;
    document.getElementById('step').textContent = route.shown >= total
        ? `Fertig: ${NUMBER.format(total)} Knoten besucht`
        : `Schritt ${NUMBER.format(route.shown)} von ${NUMBER.format(total)}`;
    draw();
}

function play() {
    if (!route) return;
    const total = route.visited.length / 2;
    if (route.shown >= total) show(0);
    player.playing = true;
    player.last = performance.now();
    setPlayIcon();
    requestAnimationFrame(tick);
}

function pause() {
    player.playing = false;
    setPlayIcon();
}

function tick(now) {
    if (!player.playing || !route) return;
    const total = route.visited.length / 2;
    const speed = document.getElementById('speed').value;
    const rate = SPEEDS[speed] || Math.max(total / 2, 50);
    route.position = Math.min(total, route.position + rate * (now - player.last) / 1000);
    player.last = now;
    const position = route.position;
    show(Math.floor(position));
    route.position = position; // show rundet, die Bruchteile laufen weiter
    if (route.shown >= total) pause(); else requestAnimationFrame(tick);
}

function setPlayIcon() {
    const button = document.getElementById('play');
    button.querySelector('.icon-play').style.display = player.playing ? 'none' : '';
    button.querySelector('.icon-pause').style.display = player.playing ? '' : 'none';
    button.title = player.playing ? 'Anhalten' : 'Abspielen';
    button.setAttribute('aria-label', button.title);
}

function setUpPlayer() {
    document.getElementById('play').addEventListener('click', () => {
        if (player.playing) {
            pause();
            player.manual = true;
            saveSoon();
        } else {
            play();
        }
    });
    document.querySelectorAll('#player [data-step]').forEach((button) => button.addEventListener('click', () => {
        if (!route) return;
        pause();
        player.manual = true;
        const step = button.dataset.step;
        if (step === 'first') show(0);
        else if (step === 'last') show(route.visited.length / 2);
        else show(route.shown + Number(step));
    }));
    document.getElementById('progress').addEventListener('input', (e) => {
        pause();
        player.manual = true;
        show(Number(e.target.value));
    });
    setPlayIcon();
}

function setUpSearches() {
    document.querySelectorAll('#searches button').forEach((button) => button.addEventListener('click', () => {
        search = button.dataset.search;
        document.querySelectorAll('#searches button').forEach((b) => b.setAttribute('aria-pressed', b === button));
        findRoute();
    }));
}

function setUpPlaces() {
    const list = document.getElementById('place-list');
    const kinds = { sight: 'Sehenswürdigkeit', school: 'Schule' };
    for (const kind of ['sight', 'school']) {
        for (const place of data.places) {
            if (place.kind !== kind) continue;
            const option = document.createElement('option');
            option.value = place.name;
            option.label = kinds[kind];
            list.appendChild(option);
        }
    }
    for (const which of ['start', 'goal']) {
        const input = document.getElementById(which);
        input.addEventListener('change', () => {
            const text = input.value.trim().toLowerCase();
            if (!text) return choose(which, null);
            const place = data.places.find((p) => p.name.toLowerCase() === text)
                || data.places.find((p) => p.name.toLowerCase().includes(text));
            if (place) return choose(which, place);
            input.closest('.place').classList.add('unknown');
            info.textContent = `Keinen Ort »${input.value}« gefunden.`;
        });
    }
    document.getElementById('swap').addEventListener('click', () => {
        const { start, goal } = chosen;
        chosen.start = null;
        choose('goal', start);
        choose('start', goal);
    });
}

function setUpLegend() {
    const ratio = window.devicePixelRatio || 1;
    const list = document.querySelector('#legend ul');
    for (const [kind, name] of LEGEND) {
        const [color, width, dash] = STYLES[kind];
        const item = document.createElement('li');
        const sample = document.createElement('canvas');
        sample.width = 28 * ratio;
        sample.height = 10 * ratio;
        const c = sample.getContext('2d');
        c.scale(ratio, ratio);
        c.lineCap = 'round';
        const line = (style, w, d = []) => {
            c.strokeStyle = style;
            c.lineWidth = w;
            c.setLineDash(d);
            c.beginPath();
            c.moveTo(3, 5);
            c.lineTo(25, 5);
            c.stroke();
        };
        if (CASING[kind]) line(CASING[kind], width * 1.6 + 2);
        line(color, dash ? 2 : width * 1.6, dash ? dash.map((d) => d * 1.6) : []);
        item.append(sample, name);
        list.appendChild(item);
    }
}

const NUMBER = new Intl.NumberFormat('de-DE');

// Was gewählt ist, steht in der Adresse der Seite: Start, Ziel, Suche, Graph,
// der Ausschnitt der Karte und der Schritt der Suche. Neu laden oder die
// Adresse weitergeben zeigt dasselbe.
let saving = null;
function saveSoon() {
    if (!data || restoring) return;
    clearTimeout(saving);
    saving = setTimeout(save, 300);
}

function save() {
    const state = new URLSearchParams();
    if (chosen.start) state.set('start', chosen.start.name);
    if (chosen.goal) state.set('goal', chosen.goal.name);
    if (search !== 'bfs') state.set('search', search);
    if (document.getElementById('graph-view').checked) state.set('graph', '1');
    if (!fitted) state.set('map', `${view.lon.toFixed(5)},${view.lat.toFixed(5)},${Math.round(view.scale)}`);
    if (route && player.manual) state.set('step', player.step);
    const hash = state.toString();
    if (hash !== location.hash.slice(1)) {
        try {
            history.replaceState(null, '', hash ? `#${hash}` : location.pathname + location.search);
        } catch {
            // zu oft hintereinander: dann eben beim nächsten Mal
        }
    }
}

let restoring = false;
function restore() {
    const state = new URLSearchParams(location.hash.slice(1));
    if (![...state.keys()].length) return;
    restoring = true;
    const named = (name) => data.places.find((p) => p.name === name) || null;
    const wanted = state.get('search');
    if (SEARCH_NAMES[wanted]) {
        search = wanted;
        document.querySelectorAll('#searches button').forEach((b) => b.setAttribute('aria-pressed', b.dataset.search === search));
    }
    if (state.has('step')) {
        player.manual = true;
        player.step = Number(state.get('step')) || 0;
    }
    if (state.get('graph') === '1') {
        document.getElementById('graph-view').checked = true;
        showGraph(true);
    }
    if (state.get('start')) choose('start', named(state.get('start')));
    if (state.get('goal')) choose('goal', named(state.get('goal')));
    const map = (state.get('map') || '').split(',').map(Number);
    if (map.length === 3 && map.every(Number.isFinite)) {
        moved();
        [view.lon, view.lat, view.scale] = map;
    }
    restoring = false;
    draw();
}

fetch('map.json').then((r) => r.json()).then((map) => {
    data = map;
    // die Wege nach Art, gezeichnet in der Reihenfolge der Wichtigkeit; zu
    // jedem Weg sein Rechteck (West, Süd, Ost, Nord), damit nur gezeichnet
    // wird, was zu sehen ist
    const byKind = map.kinds.map((kind) => ({ kind, ways: [], boxes: [] }));
    for (const [kind, points] of map.ways) {
        const coordinates = Float64Array.from(points);
        let w = Infinity, e = -Infinity, s = Infinity, n = -Infinity;
        for (let i = 0; i < coordinates.length; i += 2) {
            w = Math.min(w, coordinates[i]); e = Math.max(e, coordinates[i]);
            s = Math.min(s, coordinates[i + 1]); n = Math.max(n, coordinates[i + 1]);
        }
        byKind[kind].ways.push(coordinates);
        byKind[kind].boxes.push(w, s, e, n);
    }
    data.groups = byKind.filter((group) => group.ways.length)
        .map((group) => ({ ...group, boxes: Float64Array.from(group.boxes) }))
        .sort((a, b) => ORDER.indexOf(a.kind) - ORDER.indexOf(b.kind));
    // Für den Anfang die dichte Mitte: ohne die Wege, die über den Rand des
    // Gebiets hinausragen, ist das Fenster überall voller Karte
    const lons = new Float64Array(map.ways.reduce((sum, [, points]) => sum + points.length / 2, 0));
    const lats = new Float64Array(lons.length);
    let n = 0;
    for (const [, points] of map.ways) {
        for (let i = 0; i < points.length; i += 2, n++) {
            lons[n] = points[i];
            lats[n] = points[i + 1];
        }
    }
    lons.sort();
    lats.sort();
    const at = (sorted, share) => sorted[Math.floor(share * (sorted.length - 1))];
    data.core = [at(lons, 0.02), at(lats, 0.02), at(lons, 0.98), at(lats, 0.98)];
    const name = map.area.replace(/(^|-)\p{L}/gu, (s) => s.toUpperCase());
    document.getElementById('area').textContent =
        `${name}: ${NUMBER.format(map.ways.length)} Wege, ${NUMBER.format(map.places.length)} Orte`;
    setUpPlaces();
    setUpSearches();
    setUpPlayer();
    const toggle = document.getElementById('graph-view');
    toggle.addEventListener('change', () => {
        showGraph(toggle.checked);
        saveSoon();
    });
    fitArea();
    restore();
}).catch(() => {
    document.getElementById('area').textContent = 'Keine Karte: Läuft ruby server.rb noch?';
});
setUpLegend();
draw();
