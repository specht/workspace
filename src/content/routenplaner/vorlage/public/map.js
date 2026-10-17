// Die Karte: gezeichnet aus den Daten, die server.rb unter map.json liefert.
// Ziehen verschiebt sie, das Mausrad und + / − zoomen, ein Klick fragt den
// Server nach dem Knoten an dieser Stelle.

const canvas = document.getElementById('map');
const context = canvas.getContext('2d');
const info = document.getElementById('info');

// Wie eine Art von Weg aussieht: Farbe, Breite in Pixeln, gestrichelt
const STYLES = {
    motorway: ['#e892a2', 4], trunk: ['#f9b29c', 4], primary: ['#fcd6a4', 3.5], secondary: ['#f7fabf', 3],
    tertiary: ['#ffffff', 2.6], residential: ['#ffffff', 2.2], living_street: ['#ededed', 2.2],
    unclassified: ['#ffffff', 2.2], pedestrian: ['#dddde8', 2.2], service: ['#ffffff', 1.4],
    footway: ['#e9897e', 0.9, [2, 2]], path: ['#e9897e', 0.9, [2, 2]], cycleway: ['#4a6fe3', 0.9, [3, 2]],
    steps: ['#e9897e', 2, [1, 1.5]], track: ['#a67c39', 1, [4, 2]], bridleway: ['#3f8f3f', 0.9, [3, 2]],
};
// Straßen haben einen grauen Rand, damit die weißen sich vom Grund abheben
const CASING = { motorway: '#c24e6b', trunk: '#c84e2f', primary: '#a06b00', secondary: '#707d05',
    tertiary: '#8f8f8f', residential: '#a8a8a8', living_street: '#a8a8a8', unclassified: '#a8a8a8',
    pedestrian: '#a8a8a8', service: '#b8b8b8' };
const OTHER = ['#bbbbbb', 1];
// Wichtige Straßen zuletzt, damit sie oben liegen
const ORDER = ['footway', 'path', 'cycleway', 'steps', 'track', 'bridleway', 'service', 'pedestrian',
    'living_street', 'unclassified', 'residential', 'tertiary', 'secondary', 'primary', 'trunk', 'motorway'];

let data = null;
let view = { lon: 13.33, lat: 52.455, scale: 40000 }; // scale: Pixel je Grad Breite
let marks = []; // [[lon, lat, Farbe, Radius], …]
let chosen = { start: null, goal: null }; // die gewählten Orte
let lines = []; // [[[lon, lat], [lon, lat]], …] hervorgehoben

// Länge und Breite auf Pixel: ein Grad Länge ist um cos(Breite) kürzer
function project(lon, lat) {
    const k = Math.cos(view.lat * Math.PI / 180);
    return [canvas.width / 2 + (lon - view.lon) * k * view.scale, canvas.height / 2 - (lat - view.lat) * view.scale];
}

function unproject(x, y) {
    const k = Math.cos(view.lat * Math.PI / 180);
    return [view.lon + (x - canvas.width / 2) / (k * view.scale), view.lat - (y - canvas.height / 2) / view.scale];
}

function draw() {
    const ratio = window.devicePixelRatio || 1;
    canvas.width = canvas.clientWidth * ratio;
    canvas.height = canvas.clientHeight * ratio;
    context.setTransform(1, 0, 0, 1, 0, 0);
    context.fillStyle = '#f2efe9';
    context.fillRect(0, 0, canvas.width, canvas.height);
    if (!data) return;
    // die Wege werden mit dem Zoom breiter und schmaler, aber nur gemäßigt
    const thin = Math.min(4, Math.max(0.4, Math.sqrt(view.scale / 40000))) * ratio;
    context.lineCap = 'round';
    context.lineJoin = 'round';
    const path = (points) => {
        context.beginPath();
        for (let i = 0; i < points.length; i += 2) {
            const [x, y] = project(points[i], points[i + 1]);
            if (i === 0) context.moveTo(x, y); else context.lineTo(x, y);
        }
    };
    // erst die Ränder aller Straßen, dann ihre Füllung darüber
    context.setLineDash([]);
    for (const [kind, points] of data.sorted) {
        const casing = CASING[data.kinds[kind]];
        if (!casing) continue;
        const [, width] = STYLES[data.kinds[kind]];
        context.strokeStyle = casing;
        context.lineWidth = Math.max(0.6 * ratio, width * thin) + 1.6 * ratio;
        path(points);
        context.stroke();
    }
    for (const [kind, points] of data.sorted) {
        const [color, width, dash] = STYLES[data.kinds[kind]] || OTHER;
        context.strokeStyle = color;
        context.lineWidth = Math.max(0.6 * ratio, width * thin);
        // die Striche wachsen mit der Breite, sonst laufen sie beim Zoomen zusammen
        context.setLineDash(dash && view.scale > 20000 ? dash.map((d) => d * thin) : []);
        path(points);
        context.stroke();
    }
    context.setLineDash([]);
    context.strokeStyle = '#d6336c';
    context.lineWidth = 3 * ratio;
    for (const [a, b] of lines) {
        context.beginPath();
        context.moveTo(...project(...a));
        context.lineTo(...project(...b));
        context.stroke();
    }
    // die Orte: Sehenswürdigkeiten mit Namen, Schulen erst von nah
    context.textAlign = 'center';
    for (const place of data.places) {
        const sight = place.kind === 'sight';
        if (!sight && view.scale < 80000) continue;
        const [x, y] = project(place.lon, place.lat);
        context.fillStyle = sight ? '#7048e8' : '#868e96';
        context.beginPath();
        context.arc(x, y, (sight ? 4 : 2.5) * ratio, 0, 2 * Math.PI);
        context.fill();
        if (sight || view.scale > 160000) {
            context.font = `${(sight ? 12 : 10) * ratio}px system-ui, sans-serif`;
            context.lineWidth = 3 * ratio;
            context.strokeStyle = 'rgba(255, 255, 255, 0.85)';
            context.strokeText(place.name, x, y - 7 * ratio);
            context.fillText(place.name, x, y - 7 * ratio);
        }
    }
    for (const [which, color] of [['start', '#2f9e44'], ['goal', '#e03131']]) {
        const place = chosen[which];
        if (!place) continue;
        const [x, y] = project(place.lon, place.lat);
        context.fillStyle = color;
        context.strokeStyle = '#fff';
        context.lineWidth = 2 * ratio;
        context.beginPath();
        context.arc(x, y, 7 * ratio, 0, 2 * Math.PI);
        context.fill();
        context.stroke();
    }
    for (const [lon, lat, color, radius] of marks) {
        const [x, y] = project(lon, lat);
        context.fillStyle = color;
        context.beginPath();
        context.arc(x, y, radius * ratio, 0, 2 * Math.PI);
        context.fill();
    }
}

// Verschieben und zoomen
let drag = null;
canvas.addEventListener('pointerdown', (e) => {
    drag = { x: e.clientX, y: e.clientY, moved: false };
    canvas.setPointerCapture(e.pointerId);
});
canvas.addEventListener('pointermove', (e) => {
    if (!drag) return;
    const ratio = window.devicePixelRatio || 1;
    const dx = (e.clientX - drag.x) * ratio;
    const dy = (e.clientY - drag.y) * ratio;
    if (Math.abs(dx) + Math.abs(dy) > 3) drag.moved = true;
    if (!drag.moved) return;
    canvas.classList.add('dragging');
    const k = Math.cos(view.lat * Math.PI / 180);
    view.lon -= dx / (k * view.scale);
    view.lat += dy / view.scale;
    drag.x = e.clientX;
    drag.y = e.clientY;
    draw();
});
canvas.addEventListener('pointerup', (e) => {
    canvas.classList.remove('dragging');
    if (drag && !drag.moved) {
        const ratio = window.devicePixelRatio || 1;
        clicked(...unproject(e.offsetX * ratio, e.offsetY * ratio));
    }
    drag = null;
});
function zoom(factor) {
    view.scale = Math.min(2000000, Math.max(1000, view.scale * factor));
    draw();
}
canvas.addEventListener('wheel', (e) => {
    e.preventDefault();
    zoom(e.deltaY < 0 ? 1.2 : 1 / 1.2);
}, { passive: false });
document.querySelectorAll('#zoom button').forEach((b) => b.addEventListener('click', () => zoom(Number(b.dataset.zoom))));
window.addEventListener('resize', draw);

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

// Start und Ziel: Sehenswürdigkeiten zuerst, dann die Schulen
function fillMenus() {
    for (const which of ['start', 'goal']) {
        const select = document.getElementById(which);
        select.innerHTML = '<option value="">–</option>';
        for (const [kind, label] of [['sight', 'Sehenswürdigkeiten'], ['school', 'Schulen']]) {
            const group = document.createElement('optgroup');
            group.label = label;
            data.places.forEach((place, i) => {
                if (place.kind !== kind) return;
                const option = document.createElement('option');
                option.value = i;
                option.textContent = place.name;
                group.appendChild(option);
            });
            if (group.children.length) select.appendChild(group);
        }
        select.addEventListener('change', () => {
            chosen[which] = select.value === '' ? null : data.places[Number(select.value)];
            draw();
        });
    }
}

fetch('map.json').then((r) => r.json()).then((map) => {
    data = map;
    // gezeichnet in der Reihenfolge der Wichtigkeit
    const rank = (kind) => ORDER.indexOf(map.kinds[kind]);
    data.sorted = map.ways.slice().sort((a, b) => rank(a[0]) - rank(b[0]));
    // die Mitte des Gebiets
    let west = Infinity, east = -Infinity, south = Infinity, north = -Infinity;
    for (const [, points] of map.ways) {
        for (let i = 0; i < points.length; i += 2) {
            west = Math.min(west, points[i]); east = Math.max(east, points[i]);
            south = Math.min(south, points[i + 1]); north = Math.max(north, points[i + 1]);
        }
    }
    view.lon = (west + east) / 2;
    view.lat = (south + north) / 2;
    const k = Math.cos(view.lat * Math.PI / 180);
    const ratio = window.devicePixelRatio || 1;
    view.scale = 0.9 * Math.min(canvas.clientWidth * ratio / ((east - west) * k), canvas.clientHeight * ratio / (north - south));
    document.getElementById('area').textContent = `${map.area}: ${map.ways.length} Wege, ${map.places.length} Orte`;
    fillMenus();
    draw();
});
draw();
