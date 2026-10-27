// Pip unter Funkelbach (PipCheck, docs/plan-katas.md): the map of a Pip
// task and the playback of a run. The server ran the plan against the world
// and sends every frame (PipWorld#trace); this only draws them. What stays
// the same thing moves (Pip, the Glibbers walk from tile to tile, a stone
// that is taken fades), and each step marks its line in the editor
// (onLine). Tiles are the 24-pixel sprites of /images/pip/, scaled so the
// map fits the card.
(function () {
    const MAX_TILE = 48;
    // the scene round a map: at least so many fields across, about so much
    // wider than high
    const SCENE_COLUMNS = 10;
    const SCENE_RATIO = 1.5;
    // What Pip says, sentence by sentence, as in the 2D Game Studio
    // (speech.js): a sentence ends after . ! ? … when the next one begins,
    // not after »z. B.«, »Nr.« or »3.«, nor before a word in small letters;
    // each stays long enough to read, at least 1.5 seconds.
    const ABBREVIATIONS = new Set(['z', 'b', 'd', 'h', 'u', 'a', 'o', 's', 'v', 'nr', 'dr', 'bzw', 'usw', 'ca', 'evtl', 'ggf', 'hr', 'fr', 'st', 'str', 'vgl', 'bspw', 'inkl', 'zb', 'etc', 'max', 'min']);
    function sentences(text) {
        const line = String(text).replace(/\s+/g, ' ').trim();
        const parts = [];
        const end = /[.!?…]+["“”„»«'’)\]]*(?=\s+\S)/g;
        let start = 0;
        let m;
        while ((m = end.exec(line))) {
            const word = ((line.slice(start, m.index).match(/(\S+)$/) || [])[1] || '').replace(/^["“„»«'(\[]+/, '');
            const next = line.slice(end.lastIndex).trimStart()[0] || '';
            const stopOnly = m[0][0] === '.' && !/[!?…]/.test(m[0]) && m[0].replace(/[^.]/g, '').length === 1;
            if (stopOnly && (/^\d+$/.test(word) || /^\p{L}$/u.test(word) || ABBREVIATIONS.has(word.toLowerCase()))) continue;
            if (/\p{Ll}/u.test(next)) continue;
            parts.push(line.slice(start, end.lastIndex).trim());
            start = end.lastIndex;
        }
        parts.push(line.slice(start).trim());
        return parts.filter((p) => p.length > 0);
    }
    const READ_TIME = (sentence) => 1000 * Math.max(1.5, 1.0 + 0.065 * sentence.length);
    const SAY = { wall: 'Autsch!', gate: 'Lieber nicht!', puddle: 'Platsch!', bello: 'Bello!', caught: 'Iiih, Glibber!', chomped: 'Mampf!', no_stone: 'Hier ist nichts …', no_lever: 'Kein Hebel?', steps: 'Puh …', lamp: 'Licht!' };

    function el(tag, className, parent) {
        const node = document.createElement(tag);
        if (className) node.className = className;
        if (parent) parent.appendChild(node);
        return node;
    }

    const key = (p) => p[0] + ',' + p[1];

    // Painting the rock pixel by pixel is the slow part of a page with many
    // scenes (pip-1 has over forty): a scene waits until it comes near the
    // window, and until it is shown at all (the cards of the other
    // language are hidden).
    const NEAR = '1000px 0px';
    const waiting = new WeakMap();
    let watcher = null;
    function whenNear(node) {
        if (!window.IntersectionObserver) return Promise.resolve();
        watcher = watcher || new IntersectionObserver((entries) => entries.forEach((entry) => {
            if (!entry.isIntersecting) return;
            watcher.unobserve(entry.target);
            const go = waiting.get(entry.target);
            waiting.delete(entry.target);
            if (go) go();
        }), { rootMargin: NEAR });
        return new Promise((resolve) => {
            waiting.set(node, resolve);
            watcher.observe(node);
        });
    }

    // A number from 0 to 1 for a tile, always the same: where decoration
    // goes depends on the map alone, so a map looks the same every time.
    function hash(x, y, salt) {
        let h = (Math.imul(x, 374761393) + Math.imul(y, 668265263) + Math.imul(salt, 2246822519)) | 0;
        h = Math.imul(h ^ (h >>> 13), 1274126177);
        return ((h ^ (h >>> 16)) >>> 0) / 4294967296;
    }

    // The four quarters of a tile: where each sits, its two neighbours and
    // the one diagonal to it.
    const QUARTERS = [
        { name: 'nw', at: [0, 0], sides: [[0, -1], [-1, 0]], diagonal: [-1, -1] },
        { name: 'ne', at: [1, 0], sides: [[0, -1], [1, 0]], diagonal: [1, -1] },
        { name: 'sw', at: [0, 1], sides: [[0, 1], [-1, 0]], diagonal: [-1, 1] },
        { name: 'se', at: [1, 1], sides: [[0, 1], [1, 0]], diagonal: [1, 1] },
    ];

    // How many variants the strip of the ground has (make_sprites.py); the
    // rock's top and face are one picture each that wraps at its edges.
    const FLOORS = 4;
    const IMG = '/images/pip/';
    // pixels of a tile in the pictures, half of it, the height of a face
    const PX = 24;
    const HALF = 12;
    const FACE = 12;
    // the radius of a tight corner (a wide one has HALF, a round one PX)
    const TIGHT = 4;
    // the light edge of the rock's top (the palette of make_sprites.py, ROCK)
    const RIM_LIGHT = [100, 114, 158];
    const RIM_SOFT = [74, 84, 126];
    // the view comes from the front and a little from the left: the left
    // walls of a room show a narrow face, SIDE pixels wide; which rows of the
    // face's picture its columns take, from the ground inwards: only its lit
    // rock (the picture's dark foot row would be a black line along the wall)
    const SIDE = 4;
    const SIDE_ROWS = [7, 5, 4, 3];
    const RIM_FACE = [140, 156, 196];
    const VERSION = '?v=14';
    // toward the edges of the scene the rock fades into the dark, dithered,
    // over EDGE_FADE fields at most, outside the map (fadeEdges)
    const EDGE_FADE = 3;
    const FADE_STEPS = [1, 0.78, 0.56, 0.36, 0.18];
    // ordered dithering in map pixels, as in make_sprites.py
    const BAYER = [[0, 8, 2, 10], [12, 4, 14, 6], [3, 11, 1, 9], [15, 7, 13, 5]];

    // The pictures the rock and the ground are drawn from, loaded once for
    // every map on the page and read into pixels: { top, face, ground }.
    let loaded = null;
    function textures() {
        if (loaded) return loaded;
        const read = (name) => new Promise((resolve, reject) => {
            const image = new Image();
            image.onload = () => {
                const c = document.createElement('canvas');
                c.width = image.naturalWidth;
                c.height = image.naturalHeight;
                const g = c.getContext('2d');
                g.drawImage(image, 0, 0);
                resolve(g.getImageData(0, 0, c.width, c.height));
            };
            image.onerror = reject;
            image.src = IMG + name + VERSION;
        });
        loaded = Promise.all(['rock-top.png', 'rock-face.png', 'floor.png'].map(read))
            .then(([top, face, ground]) => ({ top, face, ground }));
        return loaded;
    }

    class PipView {
        // box: the element the map goes into; layout: PipWorld#layout;
        // start: the first frame (where Pip stands, the Glibbers).
        //
        // Seen from above at a slant (the 3/4 view), only drawn, the world
        // does not know it. Each open tile has its ground; a wall tile is the
        // top of the rock, with a light edge along its outline, and where an
        // open tile lies below it, its front face fills the lower half.
        // About half the corners are round, and the faces and edges follow
        // them (walls()). What stands upright reaches into the tile above and is
        // drawn row by row, the lower row in front (z-index by --y). A light
        // layer darkens the cave by multiplying and brightens it where
        // something glows; what shines by itself (Pip with its lantern, the
        // Funkensteine, the old lamp, glowing mushrooms and crystals) is
        // drawn above it, the rest in the dark. Fireflies drift and now and
        // then a bat flies past (not with prefers-reduced-motion). When Pip
        // lights the old lamp, it flares up, sparks fly, the cave brightens
        // and Pip hops (pip-done).
        // scene: { width, height }, the largest map of the task, so that the
        // scenes of all its maps are the same size
        constructor(box, map, first, scene) {
            this.box = box;
            const { layout, start } = this.frame(map, first, scene || map);
            this.layout = layout;
            this.world = el('div', 'pip-map', box);
            if (!box.closest('[role=img]')) {
                this.world.setAttribute('role', 'img');
                this.world.setAttribute('aria-label', 'Die Karte: Pip, die Wände, die Funkensteine und die Lampe.');
            }
            this.stones = {};
            this.stoneGlows = {};
            this.nooks = {};
            this.resize();
            const walls = new Set((layout.walls || []).map(key));
            const wall = (x, y) => x < 0 || y < 0 || x >= layout.width || y >= layout.height || walls.has(x + ',' + y);
            const lights = [];
            this.floor(wall);
            const put = (list, name) => (list || []).forEach((p) => this.tile(name, p));
            (layout.trail || []).forEach((p) => {
                // a corner of the trail has prints of its own, pointing north-east
                const diagonal = p[2] % 90 === 45;
                this.tile(diagonal ? 'trail pip-trail-diagonal' : 'trail', [p[0], p[1], diagonal ? p[2] - 45 : p[2]]);
            });
            put(layout.puddles, 'puddle');
            this.walls(wall, lights);
            // in the dark, sorted by row
            // spikes in the floor (a gate in the world: T), lowered once the
            // levers have been pulled three times
            this.spikes = (layout.gates || []).map((p) => this.tile('spikes pip-sort pip-tall', p));
            this.levers = (layout.levers || []).map((p) => this.tile('lever pip-sort pip-tall', p));
            if (layout.bello) this.tile('bello pip-sort', layout.bello);
            this.glibbers = (start.glibbers || []).map((p) => this.tile('glibber pip-sort actor', p));
            this.light = el('div', 'pip-light', this.world);
            // the spots of light, on the floor and the faces of the walls only
            this.glows = el('div', 'pip-glows', this.light);
            lights.forEach(([name, p, r]) => this.glow(name, p, r));
            // the old lamp's post stands against a wall, its lantern hangs out
            // over the open ground: mirrored where the wall is on the right
            const mirrored = layout.lamp && wall(layout.lamp[0] + 1, layout.lamp[1]) && !wall(layout.lamp[0] - 1, layout.lamp[1]);
            if (layout.lamp) this.lampGlow = this.glow('lamp', [layout.lamp[0] + (mirrored ? -0.15 : 0.15), layout.lamp[1] - 0.6], 3);
            (layout.stones || []).forEach((p) => { this.stoneGlows[key(p)] = this.glow('stone', p, 0.75); });
            // the lamps Pip has lit before (Teil 3): burning, and the nine
            // fields of their light, which the Mampfer keeps out of, tinted
            (layout.burning || []).forEach((p) => {
                const right = wall(p[0] + 1, p[1]) && !wall(p[0] - 1, p[1]);
                this.glow('lamp', [p[0] + (right ? -0.15 : 0.15), p[1] - 0.6], 3).classList.add('pip-burning');
                this.tile('lamp pip-lit pip-sort pip-tall pip-bright' + (right ? ' pip-mirrored' : ''), p);
            });
            if ((layout.burning || []).length) this.safeLight(layout.burning, wall);
            this.lantern = this.glow('lantern actor', [start.pip.x, start.pip.y], 3.5);
            // shining by themselves, above the light, sorted by row
            (layout.stones || []).forEach((p) => { this.stones[key(p)] = this.tile('stone', p); });
            this.shining.forEach(([name, p]) => this.tile(name + ' pip-sort pip-bright', p));
            if (layout.lamp) {
                this.lamp = this.tile('lamp pip-sort pip-tall pip-bright' + (mirrored ? ' pip-mirrored' : ''), layout.lamp);
                this.sparks = el('div', 'pip-sparks', this.world);
                this.sparks.style.setProperty('--x', layout.lamp[0] + (mirrored ? -0.34 : 0));
                this.sparks.style.setProperty('--y', layout.lamp[1]);
                for (let i = 0; i < 10; i++) el('i', null, this.sparks).style.setProperty('--a', (i * 36 + 8) + 'deg');
            }
            this.pip = this.tile('pip pip-sort pip-bright actor', [start.pip.x, start.pip.y]);
            // the Mampfer, above the light: in the dark it would not be seen
            this.mampfers = (start.mampfers || []).map((p) => this.tile('mampfer pip-bright actor', p));
            this.creatures(wall);
            (layout.labels || []).forEach((label) => { this.tile('label', label.at).textContent = label.text; });
            this.bubble = el('div', 'pip-bubble', this.world);
            this.bubble.hidden = true;
            this.show(first, false);
            if (window.ResizeObserver) new ResizeObserver(() => this.resize()).observe(box);
        }

        // Soft darker patches on the open ground, so that it does not look
        // tiled (the ground itself is drawn with the rock, walls()).
        floor(wall) {
            for (let y = 0; y < this.layout.height; y++) {
                for (let x = 0; x < this.layout.width; x++) {
                    const h = hash(x, y, 1);
                    if (!wall(x, y) && h < 0.22) this.tile('shade', [x, y]).style.opacity = (0.15 + h).toFixed(2);
                }
            }
        }

        // The rock, drawn as one picture of the whole map (a canvas of 24
        // pixels per tile, scaled up sharp), which throws the shadow. First
        // its shape at ground level: every wall tile, without the corners
        // that are round, with the nooks it fills in the corners of open
        // tiles (a corner is a quarter circle in the corner of its tile; no
        // corner is sharp: tight (TIGHT pixels), wide (half a tile) or round
        // (a whole tile, where the tile has no other corner to round, so
        // that a 2 × 2 room or pillar can be a circle); the corner point
        // decides, so the same corner stays the same). Then the
        // faces: the view comes from the front and a little from the left,
        // so a pixel of rock is face where open ground lies in the box below
        // it and to its right (FACE pixels down, SIDE across: the front face,
        // and the narrow face of a room's left wall); the rest is its top. So
        // the faces stand straight, keep the curve of a round corner, meet in
        // an upright seam at a square one, and are one band. The
        // top gets a light edge along its outline where it borders open
        // ground, the face one along its upper edge. The ground under it all is a picture of its own, one of the
        // variants of floor.png on each tile, under the rock too, so that a
        // cut corner shows ground; as one picture it has no seams between
        // tiles, whatever the screen's pixel ratio. The textures are the
        // strips floor.png, rock-top.png and rock-face.png (make_sprites.py).
        walls(wall, lights) {
            const W = this.layout.width * PX;
            const H = this.layout.height * PX;
            // the ground in a picture of its own under everything, the rock
            // in one above it that throws the shadow
            const groundNode = el('canvas', 'pip-ground', this.world);
            const canvasNode = el('canvas', 'pip-rock', this.world);
            groundNode.width = canvasNode.width = W;
            groundNode.height = canvasNode.height = H;
            const open = (x, y) => !wall(x, y);
            // the radius of a corner: tight, wide (half a tile) or round (a
            // whole tile, where the tile has no other corner to round, so
            // that a 2 × 2 room or pillar can be a circle); the corner point
            // decides. PipWorld.corners ('tight', 'wide' or 'round') makes
            // them all one kind, for looking at them (scratch tests).
            // A corner with rock above the open ground (the lower ones of a
            // wall, the upper ones of a nook) shows the face along its curve:
            // a round one would draw it as a long slant, so it is wide at most.
            const radius = (x, y, q, alone, solid) => {
                const kind = window.PipWorld.corners;
                const h = hash(x + q.at[0], y + q.at[1], 8);
                const choice = kind || (h < 0.4 ? 'tight' : h < 0.75 ? 'wide' : 'round');
                const face = solid ? q.at[1] === 1 : q.at[1] === 0;
                return choice === 'tight' ? TIGHT : choice === 'round' && alone && !face ? PX : HALF;
            };
            const draw = ({ top, face, ground }) => {
                // the outline of the rock, a pixel a field's PX
                const shape = new Uint8Array(W * H);
                const inQuarter = (q, px, py) => (px < HALF) === (q.at[0] === 0) && (py < HALF) === (q.at[1] === 0);
                // outside the quarter circle of radius r in the tile's corner
                // that quarter q holds (r up to a whole tile)
                const outsideRound = (q, r, px, py) => {
                    const cx = q.at[0] ? PX - r : r;
                    const cy = q.at[1] ? PX - r : r;
                    const dx = px + 0.5 - cx;
                    const dy = py + 0.5 - cy;
                    return (q.at[0] ? dx > 0 : dx < 0) && (q.at[1] ? dy > 0 : dy < 0) && dx * dx + dy * dy > r * r;
                };
                for (let ty = 0; ty < this.layout.height; ty++) {
                    for (let tx = 0; tx < this.layout.width; tx++) {
                        const corners = QUARTERS.filter((q) => wall(tx, ty)
                            ? q.sides.every(([dx, dy]) => open(tx + dx, ty + dy)) && open(tx + q.diagonal[0], ty + q.diagonal[1])
                            : q.sides.every(([dx, dy]) => wall(tx + dx, ty + dy)) && wall(tx + q.diagonal[0], ty + q.diagonal[1]));
                        const solid = wall(tx, ty);
                        const radii = corners.map((q) => radius(tx, ty, q, corners.length === 1, solid));
                        for (let py = 0; py < PX; py++) {
                            for (let px = 0; px < PX; px++) {
                                // a round corner takes the whole tile, the others
                                // their quarter
                                const n = corners.findIndex((c, k) => radii[k] === PX || inQuarter(c, px, py));
                                const cut = n >= 0 && outsideRound(corners[n], radii[n], px, py);
                                // a wall loses its round corner, an open tile gains its nook
                                const rock = solid ? !cut : Boolean(cut);
                                if (rock) shape[(ty * PX + py) * W + tx * PX + px] = 1;
                            }
                        }
                    }
                }
                const at = (x, y) => (x < 0 || y < 0 || x >= W || y >= H) ? 1 : shape[y * W + x];
                const grid = Boolean(this.box.closest('.pip-static'));
                const g = canvasNode.getContext('2d');
                const groundGraphics = groundNode.getContext('2d');
                const below = groundGraphics.createImageData(W, H);
                for (let y = 0; y < H; y++) {
                    for (let x = 0; x < W; x++) {
                        const variant = Math.floor(hash(Math.floor(x / PX), Math.floor(y / PX), 9) * FLOORS);
                        const j = (y % PX * ground.width + variant * PX + x % PX) * 4;
                        const i = (y * W + x) * 4;
                        for (let k = 0; k < 4; k++) below.data[i + k] = ground.data[j + k];
                        // a map to count fields on (a prediction) has faint lines between them
                        if (grid && (x % PX === 0 || y % PX === 0)) {
                            for (let k = 0; k < 3; k++) below.data[i + k] = Math.round(below.data[i + k] * 0.72);
                        }
                    }
                }
                groundGraphics.putImageData(below, 0, 0);
                const out = g.createImageData(W, H);
                const copy = (source, sx, sy, i) => {
                    const j = (sy * source.width + sx) * 4;
                    out.data[i] = source.data[j];
                    out.data[i + 1] = source.data[j + 1];
                    out.data[i + 2] = source.data[j + 2];
                    out.data[i + 3] = 255;
                };
                const setColour = (i, c) => {
                    out.data[i] = c[0];
                    out.data[i + 1] = c[1];
                    out.data[i + 2] = c[2];
                    out.data[i + 3] = 255;
                };
                // the face: a pixel of rock is face where open ground lies in
                // the box below it and to its right, FACE pixels down and SIDE
                // across. With open ground straight below, it is the front
                // face (the distance its row in the face's picture, the foot at
                // the ground); otherwise the narrow face of a room's left wall
                // (how far across its column). So a square inner corner is an
                // upright seam between the two, and a round one keeps its curve.
                // down[]: how far straight down to open ground.
                const down = new Int16Array(W * H).fill(99);
                for (let x = 0; x < W; x++) {
                    let d = 99;
                    for (let y = H - 1; y >= 0; y--) {
                        d = at(x, y) ? Math.min(99, d + 1) : 0;
                        down[y * W + x] = d;
                    }
                }
                const front = new Int8Array(W * H);
                const across = new Int8Array(W * H);
                for (let y = 0; y < H; y++) {
                    for (let x = 0; x < W; x++) {
                        if (!at(x, y)) continue;
                        if (down[y * W + x] <= FACE) {
                            front[y * W + x] = down[y * W + x];
                            continue;
                        }
                        for (let dx = 1; dx <= SIDE && x + dx < W; dx++) {
                            if (down[y * W + x + dx] <= FACE) {
                                across[y * W + x] = dx;
                                break;
                            }
                        }
                    }
                }
                const isFace = (x, y) => x >= 0 && y >= 0 && x < W && y < H && (front[y * W + x] > 0 || across[y * W + x] > 0);
                for (let y = 0; y < H; y++) {
                    for (let x = 0; x < W; x++) {
                        if (!at(x, y)) continue;
                        const i = (y * W + x) * 4;
                        if (isFace(x, y)) {
                            const k = front[y * W + x];
                            if (k > 0) {
                                copy(face, x % face.width, FACE - k, i);
                            } else {
                                // the side face: the face's rock turned upright, as
                                // light as the front face, so that it reads as a wall
                                copy(face, y % face.width, SIDE_ROWS[across[y * W + x] - 1], i);
                            }
                            // its upper edge, where the top of the rock begins
                            if ((at(x, y - 1) && !isFace(x, y - 1)) || (at(x - 1, y) && !isFace(x - 1, y))) setColour(i, RIM_FACE);
                            continue;
                        }
                        copy(top, x % top.width, y % top.height, i);
                        // the light edge of the top where open ground lies
                        // behind it, a soft one where it lies to the left: the
                        // edge of a room's right wall, which goes on up beside
                        // the face of the wall behind, to the top of that face
                        // (and where the narrow face of a left wall ends on it,
                        // so that the outline goes round the corner unbroken)
                        if (!at(x, y - 1) || isFace(x, y - 1)) setColour(i, RIM_LIGHT);
                        else if (!at(x, y - 2) || isFace(x, y - 2)) setColour(i, RIM_SOFT);
                        else if (!at(x - 1, y) || isFace(x - 1, y)) setColour(i, RIM_SOFT);
                    }
                }
                this.floorMask(W, H, at, isFace);
                this.fadeEdges(out, W, H);
                g.putImageData(out, 0, 0);
                // the rock in the lower half of an open field (a round
                // corner's nook) stands in front of what stands there: a
                // piece of its own, sorted with the row (.pip-nook)
                for (let ty = 0; ty < this.layout.height; ty++) {
                    for (let tx = 0; tx < this.layout.width; tx++) {
                        if (wall(tx, ty)) continue;
                        let piece = null;
                        for (let py = HALF; py < PX; py++) {
                            for (let px = 0; px < PX; px++) {
                                const x = tx * PX + px;
                                const y = ty * PX + py;
                                if (!at(x, y)) continue;
                                piece = piece || new ImageData(PX, PX);
                                const i = (y * W + x) * 4;
                                piece.data.set(out.data.subarray(i, i + 4), (py * PX + px) * 4);
                            }
                        }
                        if (!piece) continue;
                        const node = el('canvas', 'pip-tile pip-nook', this.world);
                        node.width = node.height = PX;
                        node.getContext('2d').putImageData(piece, 0, 0);
                        node.style.setProperty('--x', tx);
                        node.style.setProperty('--y', ty);
                        this.nooks[tx + ',' + ty] = node;
                    }
                }
                this.near();
            };
            // until the rock is drawn, the map stays hidden: no flash of a
            // cave without walls
            this.world.classList.add('pip-loading');
            Promise.all([textures(), whenNear(this.world)]).then(([pictures]) => draw(pictures), () => null).then(() => this.world.classList.remove('pip-loading'));
            // at the foot of a face: glowing mushrooms and crystals (with
            // their own light, above the dark), red mushrooms, stalagmites
            this.shining = [];
            for (let y = 0; y < this.layout.height; y++) {
                for (let x = 0; x < this.layout.width; x++) {
                    // on a straight piece of face only, not on a corner's curve
                    if (!wall(x, y) || wall(x, y + 1) || !wall(x - 1, y) || !wall(x + 1, y)) continue;
                    const h = hash(x, y, 2);
                    if (h < 0.1) {
                        this.shining.push(['glowshroom', [x, y]]);
                        lights.push(['shroom', [x, y], 1.3]);
                    } else if (h < 0.15) {
                        this.shining.push(['crystals', [x, y]]);
                        lights.push(['crystal', [x, y], 1.2]);
                    } else if (h < 0.21) this.tile('deco pip-mushroom pip-sort', [x, y]);
                    else if (h < 0.3) this.tile('deco pip-stalagmite pip-sort', [x, y]);
                }
            }
        }

        // Light falls on the floor and on the faces of the walls, not on the
        // top of the rock: the layer of the glows gets a mask of them, in map
        // pixels as the rock.
        floorMask(W, H, at, isFace) {
            const node = document.createElement('canvas');
            node.width = W;
            node.height = H;
            const g = node.getContext('2d');
            const image = g.createImageData(W, H);
            for (let y = 0; y < H; y++) {
                for (let x = 0; x < W; x++) {
                    if (!at(x, y) || isFace(x, y)) image.data[(y * W + x) * 4 + 3] = 255;
                }
            }
            g.putImageData(image, 0, 0);
            this.glows.style.maskImage = this.glows.style.webkitMaskImage = 'url(' + node.toDataURL() + ')';
        }

        // The rock darker toward the edges of the scene, in FADE_STEPS,
        // dithered with BAYER, so the cave has no hard border: over the last
        // EDGE_FADE fields before each edge, or the room between the map and
        // the edge where that is less; the map itself, its walls too, stays
        // as it is.
        fadeEdges(image, W, H) {
            const [x0, y0, x1, y1] = (this.inner || [0, 0, W / PX, H / PX]).map((v) => v * PX);
            const top = FADE_STEPS.length - 1;
            // how far into the fade a pixel lies: 0 at its start, 1 at the edge
            const side = (gap, room) => room <= 0 || gap >= room ? 0 : 1 - gap / Math.min(EDGE_FADE * PX, room);
            for (let y = 0; y < H; y++) {
                const ty = y < y0 ? side(y, y0) : y >= y1 ? side(H - 1 - y, H - y1) : 0;
                for (let x = 0; x < W; x++) {
                    const tx = x < x0 ? side(x, x0) : x >= x1 ? side(W - 1 - x, W - x1) : 0;
                    const amount = Math.max(0, Math.min(1, Math.max(tx, ty)));
                    if (amount <= 0) continue;
                    const level = amount * top;
                    const step = Math.min(top, Math.floor(level) + (level % 1 > (BAYER[y % 4][x % 4] + 0.5) / 16 ? 1 : 0));
                    const i = (y * W + x) * 4;
                    for (let k = 0; k < 3; k++) image.data[i + k] = Math.round(image.data[i + k] * FADE_STEPS[step]);
                }
            }
        }

        // The light of the lamps that burn on the ground, one picture in map
        // pixels (PX a tile) under the rock: brightest round the lamp, falling
        // off with the distance from it, measured round (a square with round
        // corners, SHAPE), so that the light is round and still covers the
        // nine fields round the lamp, where the Mampfer does not go: up to
        // their edge it stays bright (EDGE), only their outer corners lie a
        // little in the fade. Outside them it fades over FADE tiles, softly to
        // nothing, dithered with the Bayer pattern of make_sprites.py. Where
        // lights meet, the brighter counts, so there is no seam; only open
        // fields get it.
        safeLight(burning, wall) {
            const W = this.layout.width * PX;
            const H = this.layout.height * PX;
            const node = el('canvas', 'pip-safe', this.world);
            node.width = W;
            node.height = H;
            const g = node.getContext('2d');
            const image = g.createImageData(W, H);
            // the tint at each level of light (the last: round the lamp)
            const LEVELS = [0, 0.025, 0.05, 0.08, 0.11, 0.14, 0.18, 0.22, 0.26, 0.3, 0.34];
            const SHAPE = 4;     // 2 a circle, larger: more like a square
            const INSIDE = 1.5;  // tiles from the middle of the lamp's field to the edge of the nine
            const EDGE = 0.55;
            const FADE = 1.1;
            const top = LEVELS.length - 1;
            for (let y = 0; y < H; y++) {
                for (let x = 0; x < W; x++) {
                    const tx = Math.floor(x / PX);
                    const ty = Math.floor(y / PX);
                    if (wall(tx, ty)) continue;
                    const u = (x + 0.5) / PX;
                    const v = (y + 0.5) / PX;
                    let light = 0;
                    burning.forEach(([bx, by]) => {
                        const dx = Math.abs(u - bx - 0.5), dy = Math.abs(v - by - 0.5);
                        const d = (dx ** SHAPE + dy ** SHAPE) ** (1 / SHAPE);
                        let here;
                        if (d <= INSIDE) here = EDGE + (1 - EDGE) * (1 + Math.cos(Math.PI * d / INSIDE)) / 2;
                        else here = d < INSIDE + FADE ? EDGE * (1 - (d - INSIDE) / FADE) ** 2 : 0;
                        light = Math.max(light, here);
                    });
                    if (!light) continue;
                    const level = light * top;
                    const low = Math.floor(level);
                    const step = low < top && level - low > (BAYER[y % 4][x % 4] + 0.5) / 16 ? low + 1 : low;
                    const i = (y * W + x) * 4;
                    image.data[i] = 255;
                    image.data[i + 1] = 190;
                    image.data[i + 2] = 100;
                    image.data[i + 3] = Math.round(LEVELS[step] * 255);
                }
            }
            g.putImageData(image, 0, 0);
        }

        // A spot of light in the light layer, r tiles across from its middle.
        glow(name, p, r) {
            const node = el('div', 'pip-glow pip-glow-' + name.split(' ').join(' pip-glow-'), this.glows);
            node.style.setProperty('--x', p[0]);
            node.style.setProperty('--y', p[1]);
            node.style.setProperty('--r', r);
            return node;
        }

        // Fireflies over open tiles, and a bat that crosses the map now and then.
        creatures(wall) {
            const open = [];
            for (let y = 0; y < this.layout.height; y++) {
                for (let x = 0; x < this.layout.width; x++) if (!wall(x, y)) open.push([x, y]);
            }
            const count = Math.min(3, Math.floor(open.length / 8));
            for (let i = 0; i < count; i++) {
                const p = open[Math.floor(hash(i, open.length, 3) * open.length)];
                const fly = this.tile('firefly', p);
                fly.style.animationDelay = (-hash(p[0], p[1], 4) * 9).toFixed(1) + 's';
                fly.style.animationDuration = (7 + hash(p[0], p[1], 5) * 5).toFixed(1) + 's';
            }
            if (open.length > 20) {
                const bat = this.tile('bat', [0, Math.floor(hash(this.layout.width, this.layout.height, 6) * this.layout.height)]);
                bat.style.animationDelay = (4 + hash(this.layout.height, 1, 7) * 10).toFixed(1) + 's, 0s';
            }
        }

        // The scene round the map: rock on every side, at least one field
        // all round, at least SCENE_COLUMNS across (or scene.columns, as
        // many as fill the box, PipPlayer) and rows for about SCENE_RATIO,
        // the map in the middle; the size of a field then comes
        // from the width of the box (resize). The world and
        // the frames know only the map: shift() moves a frame into the scene.
        frame(map, first, scene) {
            // the open part of the map (where Pip can walk) in the middle,
            // not the map: a map may have more rock on one side
            const blocked = new Set((map.walls || []).map(key));
            let [bx0, by0, bx1, by1] = [map.width, map.height, -1, -1];
            for (let y = 0; y < map.height; y++) {
                for (let x = 0; x < map.width; x++) {
                    if (blocked.has(x + ',' + y)) continue;
                    [bx0, by0, bx1, by1] = [Math.min(bx0, x), Math.min(by0, y), Math.max(bx1, x), Math.max(by1, y)];
                }
            }
            if (bx1 < 0) [bx0, by0, bx1, by1] = [0, 0, map.width - 1, map.height - 1];
            // as much rock on either side: a column or row more where the
            // rest would be odd
            const even = (n, inner) => n + (n - inner) % 2;
            const cols = even(Math.max(Math.max(map.width, scene.width) + 2, scene.columns || SCENE_COLUMNS), bx1 - bx0 + 1);
            const rows = even(Math.max(Math.max(map.height, scene.height) + 2, Math.round(cols / SCENE_RATIO)), by1 - by0 + 1);
            const ox = Math.floor((cols - (bx1 - bx0 + 1)) / 2) - bx0;
            const oy = Math.floor((rows - (by1 - by0 + 1)) / 2) - by0;
            this.offset = [ox, oy];
            // the open part and the rock round it stay as they are; outside
            // it the rock fades toward the edges (fadeEdges)
            this.inner = [ox + bx0 - 1, oy + by0 - 1, ox + bx1 + 2, oy + by1 + 2];
            const at = (p) => [p[0] + ox, p[1] + oy].concat(p.slice(2));
            const inside = new Set();
            for (let y = 0; y < map.height; y++) for (let x = 0; x < map.width; x++) inside.add((x + ox) + ',' + (y + oy));
            const walls = (map.walls || []).map(at);
            for (let y = 0; y < rows; y++) for (let x = 0; x < cols; x++) if (!inside.has(x + ',' + y)) walls.push([x, y]);
            const list = (name) => (map[name] || []).map(at);
            const layout = {
                width: cols, height: rows, walls,
                stones: list('stones'), levers: list('levers'), gates: list('gates'), trail: list('trail'), puddles: list('puddles'), burning: list('burning'),
                lamp: map.lamp && at(map.lamp), bello: map.bello && at(map.bello),
                labels: (map.labels || []).map((label) => ({ at: at(label.at), text: label.text })),
            };
            return { layout, start: this.shift(first) };
        }

        shift(frame) {
            const [ox, oy] = this.offset;
            return Object.assign({}, frame, {
                pip: Object.assign({}, frame.pip, { x: frame.pip.x + ox, y: frame.pip.y + oy }),
                glibbers: (frame.glibbers || []).map((g) => [g[0] + ox, g[1] + oy]),
                mampfers: (frame.mampfers || []).map((m) => [m[0] + ox, m[1] + oy]),
                stones: (frame.stones || []).map((p) => [p[0] + ox, p[1] + oy]),
            });
        }

        resize() {
            const width = this.box.clientWidth || 400;
            const size = Math.max(12, Math.min(MAX_TILE, Math.floor(width / this.layout.width)));
            this.world.style.setProperty('--tile', size + 'px');
            this.world.style.setProperty('--map-width', this.layout.width);
            this.world.style.width = size * this.layout.width + 'px';
            this.world.style.height = size * this.layout.height + 'px';
            // the plan beside the scene is as high as it (default.css)
            const layout = this.box.closest('.pip-layout, .pip-pair');
            if (layout) layout.style.setProperty('--scene-height', size * this.layout.height + 'px');
        }

        // p: [x, y], for paw prints [x, y, the way Bello went in degrees]
        tile(name, p, parent) {
            const node = el('div', 'pip-tile pip-' + name, parent || this.world);
            node.style.setProperty('--x', p[0]);
            node.style.setProperty('--y', p[1]);
            if (p[2]) node.style.setProperty('--turn', p[2] + 'deg');
            return node;
        }

        // A frame of the run: animate: walk there in duration ms.
        show(original, animate, duration) {
            // a boolean: classList.toggle(name, undefined) would flip the class
            animate = Boolean(animate);
            const frame = this.shift(original);
            this.world.style.setProperty('--step', (animate ? duration : 0) + 'ms');
            const pip = frame.pip;
            // the Mampfers' tiles, where they stand and where they come from
            const mampfers = (frame.mampfers || []).map((m) => [m[0], m[1]]);
            this.near([this.at, [pip.x, pip.y]], (this.mampferAt || []).concat(mampfers));
            this.at = [pip.x, pip.y];
            this.mampferAt = mampfers;
            this.pip.style.setProperty('--x', pip.x);
            this.pip.style.setProperty('--y', pip.y);
            this.lantern.style.setProperty('--x', pip.x);
            this.lantern.style.setProperty('--y', pip.y);
            this.pip.dataset.facing = pip.facing;
            this.pip.classList.toggle('pip-walking', animate && ['move', 'lamp', 'bello'].includes(frame.event));
            this.pip.classList.toggle('pip-bump', frame.event === 'wall');
            this.pip.classList.toggle('pip-caught', frame.event === 'caught');
            // the Mampfer has Pip, lantern and all: gone once it is there;
            // a step back brings Pip back
            this.pip.classList.toggle('pip-eaten', frame.event === 'chomped');
            this.lantern.classList.toggle('pip-eaten', frame.event === 'chomped');
            (frame.glibbers || []).forEach((g, i) => {
                if (!this.glibbers[i]) return;
                this.glibbers[i].style.setProperty('--x', g[0]);
                this.glibbers[i].style.setProperty('--y', g[1]);
            });
            // the Mampfer runs while it moves, and chomps standing; it looks
            // where it runs, standing or running up and down at Pip, and
            // with Pip in its own column as before
            (frame.mampfers || []).forEach((m, i) => {
                const node = this.mampfers[i];
                if (!node) return;
                const oldX = node.style.getPropertyValue('--x');
                const moved = oldX !== String(m[0]) || node.style.getPropertyValue('--y') !== String(m[1]);
                if (oldX !== '' && Number(oldX) !== m[0]) node.dataset.facing = m[0] < Number(oldX) ? 'west' : 'east';
                else if (pip.x !== m[0]) node.dataset.facing = pip.x < m[0] ? 'west' : 'east';
                node.style.setProperty('--x', m[0]);
                node.style.setProperty('--y', m[1]);
                node.classList.remove('pip-running');
                if (animate && moved) {
                    void node.offsetWidth;
                    node.classList.add('pip-running');
                }
            });
            const left = new Set((frame.stones || []).map(key));
            Object.keys(this.stones).forEach((k) => this.stones[k].classList.toggle('pip-taken', !left.has(k)));
            Object.keys(this.stoneGlows).forEach((k) => this.stoneGlows[k].classList.toggle('pip-taken', !left.has(k)));
            // the old lamp lights up when Pip gets there; finding Bello is an
            // end as good
            const lit = frame.event === 'lamp';
            if (this.lamp) {
                this.lamp.classList.toggle('pip-lit', lit);
                this.lampGlow.classList.toggle('pip-lit', lit);
            }
            this.world.classList.toggle('pip-done', lit || frame.event === 'bello');
            this.pip.classList.toggle('pip-cheer', animate && (lit || frame.event === 'bello'));
            const open = frame.pulls === 3;
            this.levers.forEach((lever) => lever.classList.toggle('pip-pulled', frame.pulls % 2 === 1));
            this.world.classList.toggle('pip-gate-open', open);
            // the spikes sink or shoot up when the pulls change it, while the
            // run plays; a jump shows them at once
            const moving = animate && this.spikesOpen !== undefined && open !== this.spikesOpen;
            this.spikes.forEach((node) => {
                node.classList.remove('pip-sinking', 'pip-rising');
                if (moving) {
                    void node.offsetWidth;
                    node.classList.add(open ? 'pip-sinking' : 'pip-rising');
                }
            });
            this.spikesOpen = open;
            // a pull counts aloud: 1, 2, 3! (three lower the spikes)
            const counted = frame.event === 'pull' ? (frame.pulls === 3 ? '3!' : String(frame.pulls)) : null;
            this.say(frame.event === 'say' ? frame.text : counted || SAY[frame.event], pip);
        }

        // Pip shines above the dark (.pip-bright); the nooks of the fields
        // Pip stands on or walks between go above Pip too, unlit by the
        // light layer, as Pip's lantern lights them anyway. So do those of
        // the Mampfers' fields (it shines too), darkened by hand
        // (.pip-shaded), as no lantern lights them.
        near(tiles, dark) {
            if (tiles) this.nearTiles = tiles.filter(Boolean).map(key);
            if (dark) this.darkTiles = dark.filter(Boolean).map(key);
            const set = new Set(this.nearTiles || []);
            const shaded = new Set((this.darkTiles || []).filter((k) => !set.has(k)));
            Object.keys(this.nooks).forEach((k) => {
                this.nooks[k].classList.toggle('pip-near', set.has(k) || shaded.has(k));
                this.nooks[k].classList.toggle('pip-shaded', shaded.has(k));
            });
        }

        // What Pip says, above Pip's head (none: hidden), in the letters of
        // the 2D Game Studio, kept inside the map: lines wrap, and the text
        // moves in from an edge.
        say(text, pip) {
            this.bubble.hidden = !text;
            if (!text) return;
            if (pip) this.speaker = pip;
            this.bubble.textContent = String(text).trim();
            const tile = parseFloat(this.world.style.getPropertyValue('--tile')) || 24;
            const width = this.world.clientWidth;
            const box = this.bubble.getBoundingClientRect();
            const p = this.speaker || { x: 0, y: 0 };
            const left = Math.max(2, Math.min(width - box.width - 2, (p.x + 0.5) * tile - box.width / 2));
            const top = Math.max(2, (p.y + 0.1) * tile - box.height);
            this.bubble.style.left = left + 'px';
            this.bubble.style.top = top + 'px';
        }
    }

    // The player under the map, with the controls of every step-by-step
    // widget (docs/widgets.md, stepper_keys.js): to the start, back, next,
    // to the end; play and its speed (1×, 2×, ½×); the step count; the
    // timeline of dots; ← → and a swipe across the map. A step forward or
    // back moves, a jump shows the frame at once. onLine(line or null)
    // marks the editor.
    const SPEED_NAMES = [[1, '1×'], [2, '2×'], [0.5, '½×']];
    const STEP_TIME = 320;
    const QUICK = ['turn', 'take'];
    const QUICK_SHARE = 0.45;
    const SPEED_KEY = 'pip-speed';
    const players = [];
    let speed = 0;
    try { speed = Math.max(0, Math.min(SPEED_NAMES.length - 1, parseInt(localStorage.getItem(SPEED_KEY), 10) || 0)); } catch (e) { /* 1× */ }

    class PipPlayer {
        // maps: every map of the task ({ layout, start, call }, or { size,
        // call } for one made from the person's seed), for tabs above the
        // map; null: just one. selected: the tab shown, the map a run goes
        // on first.
        constructor(box, layout, start, maps, onLine) {
            this.box = box;
            this.onLine = onLine || (() => {});
            box.dataset.stepper = '';
            this.maps = maps && maps.length > 1 ? maps : null;
            this.runs = [];
            this.selected = 0;
            this.busy = false;
            // the largest map of the task: every scene that size
            const sizes = (maps || []).map((m) => m && m.layout ? [m.layout.width, m.layout.height] : m && m.size ? m.size : [0, 0]);
            this.scene = { width: Math.max(layout.width, ...sizes.map((s) => s[0])), height: Math.max(layout.height, ...sizes.map((s) => s[1])) };
            if (this.maps) {
                this.tabs = el('div', 'pip-tabs', box);
                this.tabs.setAttribute('role', 'tablist');
                this.maps.forEach((map, i) => {
                    const tab = el('button', 'pip-tab', this.tabs);
                    tab.type = 'button';
                    tab.setAttribute('role', 'tab');
                    tab.textContent = 'Karte ' + (i + 1);
                    // the call the check makes on this map (walk(3))
                    if (map && map.call) el('code', 'pip-tab-call', tab).textContent = map.call.trim();
                    if (!map || !map.layout) tab.title = 'Diese Karte baut der Wichtel für dich, wenn du deinen Plan ausführst.';
                    tab.addEventListener('click', () => this.show(i));
                });
            }
            this.mapBox = el('div', 'pip-map-box', box);
            // a small map on a wide screen: more rock round it, so that the
            // scene fills the box with fields of at most MAX_TILE; drawn
            // anew when that changes (a hidden card has no width yet)
            this.columns = this.scene.columns = this.wanted();
            if (window.ResizeObserver) {
                new ResizeObserver(() => {
                    const columns = this.wanted();
                    if (columns === this.columns || !this.replay) return;
                    this.columns = this.scene.columns = columns;
                    this.redraw();
                }).observe(this.mapBox);
            }
            const row = el('div', 'pip-controls', box);
            this.buttons = {};
            [['first', 'bi-chevron-bar-left', 'Zum Anfang'], ['prev', 'bi-chevron-left', 'Einen Schritt zurück'],
                ['next', 'bi-chevron-right', 'Nächster Schritt'], ['last', 'bi-chevron-bar-right', 'Zum Ende']].forEach(([go, icon, label]) => {
                const b = this.button(row, icon, label);
                b.dataset.go = go;
                this.buttons[go] = b;
            });
            this.buttons.first.addEventListener('click', () => { this.stop(); this.go(0); });
            this.buttons.prev.addEventListener('click', () => { this.stop(); this.go(this.index - 1, true); });
            this.buttons.next.addEventListener('click', () => { if (!this.auto) this.stop(); this.go(this.index + 1, true); });
            this.buttons.last.addEventListener('click', () => { this.stop(); this.go(this.frames.length - 1); });
            this.playButton = this.button(row, 'bi-play-fill', 'Abspielen');
            this.playButton.className = 'btn btn-sm btn-outline-success';
            this.playButton.dataset.play = '';
            this.playButton.addEventListener('click', () => this.toggle());
            this.speedButton = el('button', 'btn btn-sm btn-outline-success', row);
            this.speedButton.type = 'button';
            this.speedButton.dataset.speed = '';
            this.speedButton.setAttribute('aria-label', 'Tempo beim Abspielen');
            this.speedButton.textContent = SPEED_NAMES[speed][1];
            // one speed for every player of the page, kept for the next page
            this.speedButton.addEventListener('click', () => {
                speed = (speed + 1) % SPEED_NAMES.length;
                try { localStorage.setItem(SPEED_KEY, String(speed)); } catch (e) { /* only for this page */ }
                players.forEach((player) => { player.speedButton.textContent = SPEED_NAMES[speed][1]; });
            });
            players.push(this);
            this.status = el('span', 'pip-status', row);
            this.status.setAttribute('aria-live', 'polite');
            // a step chosen on the timeline stops playing and shows that frame
            box.addEventListener('pointerdown', (event) => { if (event.target.closest('.stepper-timeline')) this.stop(); }, true);
            box.stepperGo = (i) => this.go(i);
            this.load({ layout: layout, frames: [start], map: 1, maps: this.maps ? this.maps.length : 1 });
            document.dispatchEvent(new CustomEvent('stepper-added', { detail: box }));
        }

        button(row, icon, label) {
            const b = el('button', 'btn btn-sm btn-success', row);
            b.type = 'button';
            b.setAttribute('aria-label', label);
            b.innerHTML = "<i class='bi " + icon + "' aria-hidden='true'></i>";
            return b;
        }

        // Map i in its tab: its run, if there was one, else its start; a map
        // made from the seed before any run only says so.
        show(i) {
            if (this.busy || this.timer) return;
            this.selected = i;
            const run = this.runs[i];
            const map = this.maps[i];
            if (run) this.load(Object.assign({}, run, { runs: this.runs }), false);
            else if (map && map.layout) this.load({ layout: map.layout, frames: [map.start], map: i + 1, maps: this.maps.length }, false);
            else {
                // no picture of another map in its place: a dark panel that
                // says so, the size of the scene, and nothing to step through
                this.stop();
                this.active(i);
                this.mapBox.classList.add('pip-unbuilt');
                this.mapBox.dataset.unbuilt = 'Karte ' + (i + 1) + ' baut der Wichtel für dich, wenn du deinen Plan ausführst.';
                Object.values(this.buttons).forEach((button) => { button.disabled = true; });
                this.playButton.disabled = true;
                this.box.dataset.steps = 1;
                this.status.textContent = 'Karte ' + (i + 1) + ' von ' + this.maps.length + ', noch nicht gebaut';
            }
        }

        active(i) {
            this.selected = i;
            if (!this.tabs) return;
            [...this.tabs.children].forEach((tab, k) => {
                tab.classList.toggle('active', k === i);
                tab.setAttribute('aria-selected', k === i ? 'true' : 'false');
            });
            this.lockTabs();
        }

        // While a run is on its way or plays, the map stays: the tabs wait.
        lock(busy) {
            this.busy = busy;
            this.lockTabs();
        }

        lockTabs() {
            if (!this.tabs) return;
            const locked = this.busy || !!this.timer;
            [...this.tabs.children].forEach((tab) => { tab.disabled = locked; });
        }

        // A run from the server (PipCheck.replay); play: start at once;
        // over: called once, when the run has played to its end or is
        // stopped or skipped (the card shows the result then).
        load(replay, play, over) {
            this.stop();
            this.mapBox.classList.remove('pip-unbuilt');
            this.over = over || null;
            this.replay = replay;
            if (replay.runs) this.runs = replay.runs;
            this.active((replay.map || 1) - 1);
            this.frames = replay.frames;
            this.mapBox.innerHTML = '';
            this.view = new PipView(this.mapBox, replay.layout, this.frames[0], this.scene);
            this.index = 0;
            this.update();
            if (play) this.play();
        }

        wanted() {
            return columns(this.mapBox);
        }

        // The code changed after a run: the run's steps name lines that may
        // say something else now, so it is dropped, on every map; the map
        // shows where the run began until the next one.
        forget() {
            if (this.busy || !this.replay || (this.frames.length < 2 && !this.runs.length)) return;
            this.load(Object.assign({}, this.replay, { frames: [this.frames[0]], runs: [], output: null, edited: true }), false);
            this.runs = [];
        }

        // the same frame in a scene of the new size
        redraw() {
            this.mapBox.innerHTML = '';
            this.view = new PipView(this.mapBox, this.replay.layout, this.frames[0], this.scene);
            this.view.show(this.frames[this.index], false);
        }

        // a long run (a maze) goes faster, so that it plays in about half a
        // minute at 1×; a turn or taking a stone is quicker than a step,
        // unless Glibbers walk meanwhile or a Mampfer moves in it: it walks
        // a field in a frame's time, and would rush through a turn
        duration(frame) {
            const base = Math.max(60, Math.min(STEP_TIME, 30000 / this.frames.length));
            const quick = frame && QUICK.includes(frame.event) && !(frame.glibbers || []).length && !this.mampferMoves(frame) ? QUICK_SHARE : 1;
            return base * quick / SPEED_NAMES[speed][0];
        }

        // Does a Mampfer stand elsewhere than in the frame before?
        mampferMoves(frame) {
            const i = this.frames.indexOf(frame);
            if (i <= 0 || !(frame.mampfers || []).length) return false;
            const before = this.frames[i - 1].mampfers || [];
            return frame.mampfers.some((m, k) => !before[k] || before[k][0] !== m[0] || before[k][1] !== m[1]);
        }

        go(index, animate) {
            this.index = Math.max(0, Math.min(this.frames.length - 1, index));
            this.view.show(this.frames[this.index], animate, this.duration(this.frames[this.index]));
            this.update();
            if (this.index === this.frames.length - 1 && !this.timer) this.finish();
        }

        finish() {
            const over = this.over;
            this.over = null;
            if (over) over();
        }

        update() {
            const frame = this.frames[this.index];
            const ran = this.frames.length > 1;
            const maps = this.replay.maps > 1 ? 'Karte ' + this.replay.map + ' von ' + this.replay.maps + ', ' : '';
            const steps = this.frames[this.frames.length - 1].step;
            this.status.textContent = ran ? maps + 'Schritt ' + frame.step + ' von ' + steps + (frame.line ? ', Zeile ' + frame.line : frame.after ? ', nach dem Plan' : '') : this.replay.edited ? 'Seit der Änderung noch nicht gelaufen' : 'Noch nicht gelaufen';
            this.buttons.first.disabled = this.buttons.prev.disabled = this.index === 0;
            this.buttons.next.disabled = this.buttons.last.disabled = this.index >= this.frames.length - 1;
            this.playButton.disabled = !ran;
            this.box.dataset.step = this.index;
            this.box.dataset.steps = this.frames.length;
            this.onLine(ran ? frame.line : null);
        }

        toggle() {
            if (this.timer) this.stop();
            else this.play();
        }

        // play at the end starts again from the beginning
        play() {
            if (this.frames.length < 2) return this.finish();
            if (this.index >= this.frames.length - 1) this.go(0);
            this.setPlay('bi-pause-fill', 'Anhalten');
            // what Pip says comes sentence by sentence, each as long as it
            // takes to read; then the run goes on
            const tick = () => {
                if (this.index >= this.frames.length - 1) return this.stop();
                this.auto = true;
                this.buttons.next.click();
                this.auto = false;
                const frame = this.frames[this.index];
                let wait = this.duration(frame);
                if (frame.event === 'say') {
                    wait = 0;
                    sentences(frame.text).forEach((sentence) => {
                        this.saying.push(setTimeout(() => this.view.say(sentence), wait));
                        wait += READ_TIME(sentence) / Math.sqrt(SPEED_NAMES[speed][0]);
                    });
                }
                if (this.index >= this.frames.length - 1) {
                    this.timer = setTimeout(() => this.stop(), wait);
                    return;
                }
                this.timer = setTimeout(tick, wait);
            };
            this.timer = setTimeout(tick, 200);
            this.lockTabs();
        }

        stop() {
            clearTimeout(this.timer);
            this.timer = null;
            (this.saying || []).forEach(clearTimeout);
            this.saying = [];
            this.lockTabs();
            this.finish();
            if (this.playButton) this.setPlay('bi-play-fill', 'Abspielen');
        }

        setPlay(icon, label) {
            this.playButton.innerHTML = "<i class='bi " + icon + "' aria-hidden='true'></i>";
            this.playButton.setAttribute('aria-label', label);
        }
    }

    // A map written into the page, for a prediction (»Wo steht Pip?«):
    // <div class='pip-world pip-static'><pre>the map</pre></div>, in the
    // legend of PipWorld (pip_world.rb), digits mark tiles to talk about.
    // Pip looks east. Drawn once, nothing moves.
    function parse(text) {
        const rows = text.split('\n').filter((row) => row.trim() !== '');
        const layout = { width: Math.max(...rows.map((row) => row.length)), height: rows.length, walls: [], stones: [], levers: [], gates: [], trail: [], puddles: [], labels: [], burning: [] };
        const start = { event: 'start', step: 0, glibbers: [], mampfers: [], stones: [], pulls: 0, pip: { x: 0, y: 0, facing: 'east' } };
        rows.forEach((row, y) => {
            row.split('').forEach((c, x) => {
                const p = [x, y];
                if (c === '#') layout.walls.push(p);
                else if (c === 'o') { layout.stones.push(p); start.stones.push(p); }
                else if (c === 'L') layout.lamp = p;
                else if (c === 'B') layout.bello = p;
                else if (c === 'H') layout.levers.push(p);
                else if (c === 'T') layout.gates.push(p);
                else if (c === 'p') layout.trail.push(p);
                else if (c === '~') layout.puddles.push(p);
                else if (c === 'G' || c === 'g') start.glibbers.push(p);
                else if (c === 'M') start.mampfers.push(p);
                else if (c === '*') layout.burning.push(p);
                else if (c === 'P') start.pip = { x: x, y: y, facing: 'east' };
                else if (/[0-9]/.test(c)) layout.labels.push({ at: p, text: c });
            });
        });
        return { layout: layout, start: start };
    }

    // The plan right after such a map (code blocks, one per language) goes
    // beside it on a wide screen, as in the cards (.pip-pair).
    function pair(box) {
        const code = [];
        for (let next = box.nextElementSibling; next && (next.matches('.code-language') || next.matches('pre, div.highlight')); next = next.nextElementSibling) code.push(next);
        if (code.length === 0) return;
        const wrap = document.createElement('div');
        wrap.className = 'pip-pair';
        box.before(wrap);
        wrap.append(box, ...code);
        // each plan in the editor of the cards, to read only: the same
        // colours, the line numbers (code_input.js)
        code.forEach((block) => {
            const pre = block.matches('pre') ? block : block.querySelector('pre');
            const language = block.dataset.language;
            if (!pre || !language || !window.CodeInput) return;
            const text = document.createElement('textarea');
            text.className = 'form-control task-code pip-plan';
            text.readOnly = true;
            text.spellcheck = false;
            text.dataset.codeLanguage = language;
            text.setAttribute('aria-label', 'Der Plan');
            text.value = pre.textContent.replace(/\n$/, '');
            text.rows = text.value.split('\n').length;
            pre.replaceWith(text);
            CodeInput.attach(text);
        });
    }

    function drawStatic() {
        document.querySelectorAll('.pip-world.pip-static').forEach((box) => {
            const source = box.querySelector('pre');
            if (!source) return;
            pair(box);
            const map = parse(source.textContent);
            source.hidden = true;
            const mapBox = el('div', 'pip-map-box', box);
            // beside its plan, the scene fills its column like a card's
            const scene = box.parentElement.matches('.pip-pair') ? { width: map.layout.width, height: map.layout.height, columns: columns(mapBox) } : null;
            new PipView(mapBox, map.layout, map.start, scene);
        });
    }

    // The first chapter (PipCheck, buttons): the plan is clicked, not
    // typed. A click moves Pip at once, here in the browser, as the world on
    // the server would (such a map has only walls, stones and the lamp), and
    // writes the command's line into the plan; a step into a wall or taking
    // where there is no stone only shows Pip's surprise and writes nothing.
    // »Rückgängig« takes the last line back. onPlan(text) after every change;
    // onLamp() when Pip reaches the lamp, for the card to check the plan.
    const DIRECTIONS = ['east', 'north', 'west', 'south'];
    const DELTA = { east: [1, 0], north: [0, -1], west: [-1, 0], south: [0, 1] };
    const SURPRISE_MS = 900;

    class PipButtons {
        constructor(row, player, layout, start, onPlan, onLamp) {
            this.row = row;
            this.player = player;
            this.layout = layout;
            this.start = start;
            this.onPlan = onPlan;
            this.onLamp = onLamp;
            this.walls = new Set((layout.walls || []).map(key));
            this.commands = {};
            row.querySelectorAll('.pip-button').forEach((button) => {
                this.commands[button.dataset.line] = button.dataset.command;
                button.addEventListener('click', () => this.click(button.dataset.command, button.dataset.line));
            });
            this.undoButton = row.querySelector('.pip-undo');
            this.undoButton.addEventListener('click', () => this.undo());
            this.reset();
        }

        reset() {
            this.lines = [];
            this.frames = [this.start];
        }

        // The frame after command, or [null, what Pip says] when it can't be done.
        step(frame, command) {
            const pip = Object.assign({}, frame.pip);
            let stones = frame.stones || [];
            let event = command === 'turn_left' ? 'turn' : command;
            if (command === 'move') {
                const [dx, dy] = DELTA[pip.facing];
                const x = pip.x + dx;
                const y = pip.y + dy;
                if (this.walls.has(key([x, y])) || x < 0 || y < 0 || x >= this.layout.width || y >= this.layout.height) return [null, 'wall'];
                pip.x = x;
                pip.y = y;
                if (this.layout.lamp && key(this.layout.lamp) === key([x, y])) event = 'lamp';
            } else if (command === 'turn_left') {
                pip.facing = DIRECTIONS[(DIRECTIONS.indexOf(pip.facing) + 1) % 4];
            } else if (command === 'take') {
                const here = key([pip.x, pip.y]);
                if (!stones.some((p) => key(p) === here)) return [null, 'no_stone'];
                stones = stones.filter((p) => key(p) !== here);
            }
            return [Object.assign({}, frame, { event, pip, stones, step: frame.step + 1, line: this.lines.length + 1 }), null];
        }

        last() {
            return this.frames[this.frames.length - 1];
        }

        click(command, line) {
            if (this.player.busy || this.player.timer || this.last().event === 'lamp') return;
            clearTimeout(this.surprise);
            const [frame, refused] = this.step(this.last(), command);
            if (!frame) {
                // Pip stays where the plan leaves Pip, and says why
                this.show(this.frames.length - 1);
                this.player.view.show(Object.assign({}, this.last(), { event: refused }), true, 300);
                this.surprise = setTimeout(() => this.player.view.show(this.last(), false), SURPRISE_MS);
                return;
            }
            this.lines.push(line);
            this.frames.push(frame);
            this.show(this.frames.length - 1, true);
            this.changed();
            if (frame.event === 'lamp') this.onLamp();
        }

        undo() {
            if (this.player.busy || this.player.timer || !this.lines.length) return;
            clearTimeout(this.surprise);
            this.lines.pop();
            this.frames.pop();
            this.show(this.frames.length - 1, true);
            this.changed();
        }

        show(index, animate) {
            this.player.frames = this.frames.slice();
            this.player.go(index, animate);
        }

        changed() {
            this.refresh();
            this.onPlan(this.lines.map((line) => line + '\n').join(''));
        }

        // at the lamp only »Rückgängig« is left; also after the card was busy
        refresh() {
            this.undoButton.disabled = !this.lines.length;
            this.row.querySelectorAll('.pip-button').forEach((button) => { button.disabled = this.last().event === 'lamp'; });
            this.player.update();
        }

        // A plan stored before (the card's state): clicked again, line by
        // line, as far as it goes.
        load(text) {
            this.reset();
            for (const line of String(text || '').split('\n').map((l) => l.trim()).filter(Boolean)) {
                const command = this.commands[line];
                const frame = command && this.step(this.last(), command)[0];
                if (!frame) break;
                this.lines.push(line);
                this.frames.push(frame);
            }
            this.show(this.frames.length - 1);
            this.changed();
        }
    }

    if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', drawStatic);
    else drawStatic();

    // As many columns as fill box with fields of at most MAX_TILE.
    function columns(box) {
        const width = box.clientWidth;
        return width > 0 ? Math.max(SCENE_COLUMNS, Math.ceil(width / MAX_TILE)) : SCENE_COLUMNS;
    }

    window.PipWorld = { PipView, PipPlayer, PipButtons, parse, columns, corners: null };
})();
