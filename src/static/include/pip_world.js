// Pip unter Funkelbach (PipCheck, docs/plan-katas.md): the map of a Pip
// task and the playback of a run. The server ran the plan against the world
// and sends every frame (PipWorld#trace); this only draws them. What stays
// the same thing moves (Pip, the Glibbers walk from tile to tile, a stone
// that is taken fades), and each step marks its line in the editor
// (onLine). Tiles are the 24-pixel sprites of /images/pip/, scaled so the
// map fits the card.
(function () {
    const MAX_TILE = 48;
    const SAY = { wall: 'Autsch!', gate: 'Zu!', puddle: 'Platsch!', bello: 'Bello!', caught: 'Iiih, Glibber!', no_stone: 'Hier ist nichts …', no_lever: 'Kein Hebel?', steps: 'Puh …', lamp: 'Licht!' };

    function el(tag, className, parent) {
        const node = document.createElement(tag);
        if (className) node.className = className;
        if (parent) parent.appendChild(node);
        return node;
    }

    const key = (p) => p[0] + ',' + p[1];

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

    // How many variants each strip of the ground and the rock has
    // (make_sprites.py).
    const FLOORS = 4;
    const TOPS = 3;
    const FACES = 4;
    const IMG = '/images/pip/';
    // pixels of a tile in the pictures, half of it, the height of a face
    const PX = 24;
    const HALF = 12;
    const FACE = 12;
    // the light edge of the rock's top (the palette of make_sprites.py, ROCK)
    const RIM_LIGHT = [100, 114, 158];
    const RIM_SOFT = [74, 84, 126];
    const RIM_DARK = [26, 28, 50];
    const VERSION = '?v=6';

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
        constructor(box, layout, start) {
            this.box = box;
            this.layout = layout;
            this.world = el('div', 'pip-map', box);
            if (!box.closest('[role=img]')) {
                this.world.setAttribute('role', 'img');
                this.world.setAttribute('aria-label', 'Die Karte: Pip, die Wände, die Funkensteine und die Lampe.');
            }
            this.stones = {};
            this.stoneGlows = {};
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
            put(layout.gates, 'gate pip-sort pip-tall');
            this.levers = (layout.levers || []).map((p) => this.tile('lever pip-sort', p));
            if (layout.bello) this.tile('bello pip-sort', layout.bello);
            this.glibbers = (start.glibbers || []).map((p) => this.tile('glibber pip-sort actor', p));
            this.light = el('div', 'pip-light', this.world);
            lights.forEach(([name, p, r]) => this.glow(name, p, r));
            if (layout.lamp) this.lampGlow = this.glow('lamp', [layout.lamp[0] + 0.15, layout.lamp[1] - 0.6], 3);
            (layout.stones || []).forEach((p) => { this.stoneGlows[key(p)] = this.glow('stone', p, 0.75); });
            this.lantern = this.glow('lantern actor', [start.pip.x, start.pip.y], 3.5);
            // shining by themselves, above the light, sorted by row
            (layout.stones || []).forEach((p) => { this.stones[key(p)] = this.tile('stone', p); });
            this.shining.forEach(([name, p]) => this.tile(name + ' pip-sort pip-bright', p));
            if (layout.lamp) {
                this.lamp = this.tile('lamp pip-sort pip-tall pip-bright', layout.lamp);
                this.sparks = el('div', 'pip-sparks', this.world);
                this.sparks.style.setProperty('--x', layout.lamp[0]);
                this.sparks.style.setProperty('--y', layout.lamp[1]);
                for (let i = 0; i < 10; i++) el('i', null, this.sparks).style.setProperty('--a', (i * 36 + 8) + 'deg');
            }
            this.pip = this.tile('pip pip-sort pip-bright actor', [start.pip.x, start.pip.y]);
            this.creatures(wall);
            (layout.labels || []).forEach((label) => { this.tile('label', label.at).textContent = label.text; });
            this.bubble = el('div', 'pip-bubble', this.world);
            this.bubble.hidden = true;
            this.show(start, false);
            if (window.ResizeObserver) new ResizeObserver(() => this.resize()).observe(box);
        }

        // The ground of each open tile, one of the variants, and some soft
        // darker patches, so that it does not look tiled.
        floor(wall) {
            for (let y = 0; y < this.layout.height; y++) {
                for (let x = 0; x < this.layout.width; x++) {
                    if (wall(x, y)) continue;
                    this.tile('floor', [x, y]).style.setProperty('--v', Math.floor(hash(x, y, 9) * FLOORS));
                    const h = hash(x, y, 1);
                    if (h < 0.22) this.tile('shade', [x, y]).style.opacity = (0.15 + h).toFixed(2);
                }
            }
        }

        // The rock, drawn as one picture of the whole map (a canvas of 24
        // pixels per tile, scaled up sharp), which throws the shadow. First
        // its shape at ground level: every wall tile, without the corners
        // that are round, with the nooks it fills in the corners of open
        // tiles (a round corner is a quarter circle round the middle of its
        // tile; about half of them, the corner point decides, so the same
        // corner stays round). Then, column by column, the lowest FACE
        // pixels of the shape above an open tile are its front face, the
        // rest is its top: so the face bends round a round corner and into
        // a nook. The top gets a light edge along its outline where it
        // borders open ground. The textures are the strips rock-top.png and
        // rock-face.png (make_sprites.py).
        walls(wall, lights) {
            const W = this.layout.width * PX;
            const H = this.layout.height * PX;
            const canvasNode = el('canvas', 'pip-rock', this.world);
            canvasNode.width = W;
            canvasNode.height = H;
            const open = (x, y) => !wall(x, y);
            const worn = (x, y, q) => hash(x + q.at[0], y + q.at[1], 8) < 0.5;
            const shape = new Uint8Array(W * H);
            const inQuarter = (q, px, py) => (px < HALF) === (q.at[0] === 0) && (py < HALF) === (q.at[1] === 0);
            const outsideRound = (px, py) => (px + 0.5 - HALF) ** 2 + (py + 0.5 - HALF) ** 2 > HALF * HALF;
            for (let ty = 0; ty < this.layout.height; ty++) {
                for (let tx = 0; tx < this.layout.width; tx++) {
                    const rounded = QUARTERS.filter((q) => wall(tx, ty)
                        ? worn(tx, ty, q) && q.sides.every(([dx, dy]) => open(tx + dx, ty + dy)) && open(tx + q.diagonal[0], ty + q.diagonal[1])
                        : worn(tx, ty, q) && q.sides.every(([dx, dy]) => wall(tx + dx, ty + dy)) && wall(tx + q.diagonal[0], ty + q.diagonal[1]));
                    for (let py = 0; py < PX; py++) {
                        for (let px = 0; px < PX; px++) {
                            const q = rounded.find((r) => inQuarter(r, px, py));
                            // a wall loses its round corner, an open tile gains its nook
                            const rock = wall(tx, ty) ? !(q && outsideRound(px, py)) : Boolean(q && outsideRound(px, py));
                            if (rock) shape[(ty * PX + py) * W + tx * PX + px] = 1;
                        }
                    }
                }
            }
            const at = (x, y) => (x < 0 || y < 0 || x >= W || y >= H) ? 1 : shape[y * W + x];
            const draw = () => {
                if (!tops.complete || !faces.complete || !tops.naturalWidth || !faces.naturalWidth) return;
                const g = canvasNode.getContext('2d');
                const read = (image) => {
                    const c = document.createElement('canvas');
                    c.width = image.naturalWidth;
                    c.height = image.naturalHeight;
                    const cg = c.getContext('2d');
                    cg.drawImage(image, 0, 0);
                    return cg.getImageData(0, 0, c.width, c.height);
                };
                const top = read(tops);
                const face = read(faces);
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
                for (let x = 0; x < W; x++) {
                    // from the bottom up: how far above the rock's lower edge,
                    // and the tile the face stands on (its variant)
                    let above = -1;
                    let foot = 0;
                    for (let y = H - 1; y >= 0; y--) {
                        if (!at(x, y)) {
                            above = -1;
                            continue;
                        }
                        // the lower edge of the rock, over open ground
                        if (above < 0) {
                            above = y < H - 1 && !at(x, y + 1) ? 0 : FACE;
                            foot = Math.floor(y / PX);
                        } else above++;
                        const i = (y * W + x) * 4;
                        const tx = Math.floor(x / PX);
                        const ty = Math.floor(y / PX);
                        if (above < FACE) {
                            const variant = Math.floor(hash(tx, foot, 11) * FACES);
                            copy(face, variant * PX + (x % PX), FACE - 1 - above, i);
                            continue;
                        }
                        copy(top, Math.floor(hash(tx, ty, 10) * TOPS) * PX + (x % PX), y % PX, i);
                        // the light edge of the top where it borders open ground
                        if (!at(x, y - 1)) setColour(i, RIM_LIGHT);
                        else if (!at(x, y - 2)) setColour(i, RIM_SOFT);
                        else if (!at(x - 1, y)) setColour(i, RIM_SOFT);
                        else if (!at(x + 1, y)) setColour(i, RIM_DARK);
                    }
                }
                g.putImageData(out, 0, 0);
            };
            const tops = new Image();
            const faces = new Image();
            tops.onload = faces.onload = draw;
            tops.src = IMG + 'rock-top.png' + VERSION;
            faces.src = IMG + 'rock-face.png' + VERSION;
            draw();
            // at the foot of a face: glowing mushrooms and crystals (with
            // their own light, above the dark), red mushrooms, stalagmites
            this.shining = [];
            for (let y = 0; y < this.layout.height; y++) {
                for (let x = 0; x < this.layout.width; x++) {
                    if (!wall(x, y) || wall(x, y + 1)) continue;
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

        // A spot of light in the light layer, r tiles across from its middle.
        glow(name, p, r) {
            const node = el('div', 'pip-glow pip-glow-' + name.split(' ').join(' pip-glow-'), this.light);
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

        resize() {
            const width = this.box.clientWidth || 400;
            const size = Math.max(12, Math.min(MAX_TILE, Math.floor(width / this.layout.width)));
            this.world.style.setProperty('--tile', size + 'px');
            this.world.style.setProperty('--map-width', this.layout.width);
            this.world.style.width = size * this.layout.width + 'px';
            this.world.style.height = size * this.layout.height + 'px';
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
        show(frame, animate, duration) {
            this.world.style.setProperty('--step', (animate ? duration : 0) + 'ms');
            const pip = frame.pip;
            this.pip.style.setProperty('--x', pip.x);
            this.pip.style.setProperty('--y', pip.y);
            this.lantern.style.setProperty('--x', pip.x);
            this.lantern.style.setProperty('--y', pip.y);
            this.pip.dataset.facing = pip.facing;
            this.pip.classList.toggle('pip-walking', animate && ['move', 'lamp', 'bello'].includes(frame.event));
            this.pip.classList.toggle('pip-bump', frame.event === 'wall' || frame.event === 'gate');
            this.pip.classList.toggle('pip-caught', frame.event === 'caught');
            (frame.glibbers || []).forEach((g, i) => {
                if (!this.glibbers[i]) return;
                this.glibbers[i].style.setProperty('--x', g[0]);
                this.glibbers[i].style.setProperty('--y', g[1]);
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
            const text = frame.event === 'say' ? frame.text : SAY[frame.event];
            this.bubble.hidden = !text;
            if (text) {
                this.bubble.textContent = text;
                this.bubble.style.setProperty('--x', pip.x);
                this.bubble.style.setProperty('--y', pip.y);
            }
        }
    }

    // The player under the map, with the controls of every step-by-step
    // widget (docs/widgets.md, stepper_keys.js): to the start, back, next,
    // to the end; play and its speed (1×, 2×, ½×); the step count; the
    // timeline of dots; ← → and a swipe across the map. A step forward or
    // back moves, a jump shows the frame at once. onLine(line or null)
    // marks the editor.
    const SPEED_NAMES = [[1, '1×'], [2, '2×'], [0.5, '½×']];
    const STEP_TIME = 450;

    class PipPlayer {
        constructor(box, layout, start, onLine) {
            this.box = box;
            this.onLine = onLine || (() => {});
            box.dataset.stepper = '';
            this.mapBox = el('div', 'pip-map-box', box);
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
            this.speed = 0;
            this.speedButton = el('button', 'btn btn-sm btn-outline-success', row);
            this.speedButton.type = 'button';
            this.speedButton.dataset.speed = '';
            this.speedButton.setAttribute('aria-label', 'Tempo beim Abspielen');
            this.speedButton.textContent = SPEED_NAMES[0][1];
            this.speedButton.addEventListener('click', () => {
                this.speed = (this.speed + 1) % SPEED_NAMES.length;
                this.speedButton.textContent = SPEED_NAMES[this.speed][1];
            });
            this.status = el('span', 'pip-status', row);
            this.status.setAttribute('aria-live', 'polite');
            // a step chosen on the timeline stops playing and shows that frame
            box.addEventListener('pointerdown', (event) => { if (event.target.closest('.stepper-timeline')) this.stop(); }, true);
            box.stepperGo = (i) => this.go(i);
            this.load({ layout: layout, frames: [start] });
            document.dispatchEvent(new CustomEvent('stepper-added', { detail: box }));
        }

        button(row, icon, label) {
            const b = el('button', 'btn btn-sm btn-success', row);
            b.type = 'button';
            b.setAttribute('aria-label', label);
            b.innerHTML = "<i class='bi " + icon + "' aria-hidden='true'></i>";
            return b;
        }

        // A run from the server (PipCheck.replay); play: start at once.
        load(replay, play) {
            this.stop();
            this.replay = replay;
            this.frames = replay.frames;
            this.mapBox.innerHTML = '';
            this.view = new PipView(this.mapBox, replay.layout, this.frames[0]);
            this.index = 0;
            this.update();
            if (play) this.play();
        }

        // a long run (a maze) goes faster, so that it plays in about half a
        // minute at 1×
        duration() {
            const base = Math.max(60, Math.min(STEP_TIME, 30000 / this.frames.length));
            return base / SPEED_NAMES[this.speed][0];
        }

        go(index, animate) {
            this.index = Math.max(0, Math.min(this.frames.length - 1, index));
            this.view.show(this.frames[this.index], animate, this.duration());
            this.update();
        }

        update() {
            const frame = this.frames[this.index];
            const ran = this.frames.length > 1;
            const maps = this.replay.maps > 1 ? 'Karte ' + this.replay.map + ' von ' + this.replay.maps + ' · ' : '';
            const steps = this.frames[this.frames.length - 1].step;
            this.status.textContent = ran ? maps + 'Schritt ' + frame.step + ' von ' + steps + (frame.line ? ' · Zeile ' + frame.line : '') : 'Noch nicht gelaufen';
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
            if (this.frames.length < 2) return;
            if (this.index >= this.frames.length - 1) this.go(0);
            this.setPlay('bi-pause-fill', 'Anhalten');
            const tick = () => {
                if (this.index >= this.frames.length - 1) return this.stop();
                this.auto = true;
                this.buttons.next.click();
                this.auto = false;
                if (this.index >= this.frames.length - 1) return this.stop();
                this.timer = setTimeout(tick, this.duration());
            };
            this.timer = setTimeout(tick, 200);
        }

        stop() {
            clearTimeout(this.timer);
            this.timer = null;
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
        const layout = { width: Math.max(...rows.map((row) => row.length)), height: rows.length, walls: [], stones: [], levers: [], gates: [], trail: [], puddles: [], labels: [] };
        const start = { event: 'start', step: 0, glibbers: [], stones: [], pulls: 0, pip: { x: 0, y: 0, facing: 'east' } };
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
                else if (c === 'P') start.pip = { x: x, y: y, facing: 'east' };
                else if (/[0-9]/.test(c)) layout.labels.push({ at: p, text: c });
            });
        });
        return { layout: layout, start: start };
    }

    function drawStatic() {
        document.querySelectorAll('.pip-world.pip-static').forEach((box) => {
            const source = box.querySelector('pre');
            if (!source) return;
            const map = parse(source.textContent);
            source.hidden = true;
            new PipView(el('div', 'pip-map-box', box), map.layout, map.start);
        });
    }

    if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', drawStatic);
    else drawStatic();

    window.PipWorld = { PipView, PipPlayer, parse };
})();
