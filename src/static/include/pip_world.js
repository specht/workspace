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
    // the one diagonal to it, and its corner towards the middle of the tile.
    const QUARTERS = [
        { name: 'nw', at: [0, 0], sides: [[0, -1], [-1, 0]], diagonal: [-1, -1], inner: '100% 100%' },
        { name: 'ne', at: [1, 0], sides: [[0, -1], [1, 0]], diagonal: [1, -1], inner: '0 100%' },
        { name: 'sw', at: [0, 1], sides: [[0, 1], [-1, 0]], diagonal: [-1, 1], inner: '100% 0' },
        { name: 'se', at: [1, 1], sides: [[0, 1], [1, 0]], diagonal: [1, 1], inner: '0 0' },
    ];
    const ROUND = (inner, keep) => 'radial-gradient(circle calc(var(--tile) / 2) at ' + inner + ', ' +
        (keep ? '#000 calc(100% - 0.5px), transparent 100%)' : 'transparent calc(100% - 0.5px), #000 100%)');

    class PipView {
        // box: the element the map goes into; layout: PipWorld#layout;
        // start: the first frame (where Pip stands, the Glibbers).
        //
        // Atmosphere, only drawn, the world does not know it: the walls
        // throw a shadow and about half their corners are round (a wall tile
        // loses the corner between two open sides, an open tile between two
        // walls gets a curved nook), so tunnels bend here and there; mushrooms and stalagmites grow
        // at the edges of the tunnels, never on a tile Pip walks on; a light
        // layer darkens the cave by multiplying and brightens it where
        // something glows (Pip's lantern, the old lamp once Pip has lit it,
        // the Funkensteine, the glowing mushrooms); fireflies drift and now and then a bat flies
        // past (not with prefers-reduced-motion).
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
            this.walls(wall, lights);
            const put = (list, name) => (list || []).forEach((p) => this.tile(name, p));
            (layout.trail || []).forEach((p) => {
                // a corner of the trail has prints of its own, pointing north-east
                const diagonal = p[2] % 90 === 45;
                this.tile(diagonal ? 'trail pip-trail-diagonal' : 'trail', [p[0], p[1], diagonal ? p[2] - 45 : p[2]]);
            });
            put(layout.puddles, 'puddle');
            put(layout.gates, 'gate');
            this.levers = (layout.levers || []).map((p) => this.tile('lever', p));
            if (layout.lamp) this.lamp = this.tile('lamp', layout.lamp);
            if (layout.bello) this.tile('bello', layout.bello);
            (layout.stones || []).forEach((p) => { this.stones[key(p)] = this.tile('stone', p); });
            this.glibbers = (start.glibbers || []).map((p) => this.tile('glibber actor', p));
            this.pip = this.tile('pip actor', [start.pip.x, start.pip.y]);
            this.light = el('div', 'pip-light', this.world);
            lights.forEach(([name, p, r]) => this.glow(name, p, r));
            if (layout.lamp) this.lampGlow = this.glow('lamp', layout.lamp, 3);
            (layout.stones || []).forEach((p) => { this.stoneGlows[key(p)] = this.glow('stone', p, 0.75); });
            this.lantern = this.glow('lantern actor', [start.pip.x, start.pip.y], 3.5);
            this.creatures(wall);
            (layout.labels || []).forEach((label) => { this.tile('label', label.at).textContent = label.text; });
            this.bubble = el('div', 'pip-bubble', this.world);
            this.bubble.hidden = true;
            this.show(start, false);
            if (window.ResizeObserver) new ResizeObserver(() => this.resize()).observe(box);
        }

        // The ground: some tiles a little darker, so it does not look tiled.
        floor(wall) {
            for (let y = 0; y < this.layout.height; y++) {
                for (let x = 0; x < this.layout.width; x++) {
                    const h = hash(x, y, 1);
                    if (!wall(x, y) && h < 0.22) this.tile('shade', [x, y]).style.opacity = (0.15 + h).toFixed(2);
                }
            }
        }

        // The walls in a layer of their own, which throws the shadow; round
        // corners, about half of them, so that the rock looks worn in places
        // and hewn in others (the corner point decides, so the same corner
        // stays round); what grows at the edges of the tunnels.
        walls(wall, lights) {
            const layer = el('div', 'pip-walls', this.world);
            const open = (x, y) => !wall(x, y);
            const worn = (x, y, q) => hash(x + q.at[0], y + q.at[1], 8) < 0.5;
            for (let y = 0; y < this.layout.height; y++) {
                for (let x = 0; x < this.layout.width; x++) {
                    if (wall(x, y)) {
                        // a corner between two open sides is round
                        const layers = QUARTERS.map((q) => {
                            const round = worn(x, y, q) && q.sides.every(([dx, dy]) => open(x + dx, y + dy)) && open(x + q.diagonal[0], y + q.diagonal[1]);
                            return round ? ROUND(q.inner, true) : 'linear-gradient(#000, #000)';
                        });
                        const node = this.tile('wall', [x, y], layer);
                        if (layers.some((l) => l.startsWith('radial'))) {
                            node.style.maskImage = node.style.webkitMaskImage = layers.join(', ');
                            node.classList.add('pip-rounded');
                        }
                    } else {
                        // between two walls, an open tile gets a curved nook
                        QUARTERS.forEach((q) => {
                            if (!(worn(x, y, q) && q.sides.every(([dx, dy]) => wall(x + dx, y + dy)) && wall(x + q.diagonal[0], y + q.diagonal[1]))) return;
                            const nook = this.tile('nook', [x, y], layer);
                            nook.style.setProperty('--qx', q.at[0]);
                            nook.style.setProperty('--qy', q.at[1]);
                            nook.style.maskImage = nook.style.webkitMaskImage = ROUND(q.inner, false);
                        });
                    }
                }
            }
            // at the foot of a wall that has an open tile below it: glowing
            // mushrooms (with their own light), red ones, stalagmites
            for (let y = 0; y < this.layout.height; y++) {
                for (let x = 0; x < this.layout.width; x++) {
                    if (!wall(x, y) || wall(x, y + 1)) continue;
                    const h = hash(x, y, 2);
                    if (h < 0.14) {
                        this.tile('deco pip-glowshroom', [x, y]);
                        lights.push(['shroom', [x, y], 1.3]);
                    } else if (h < 0.2) this.tile('deco pip-mushroom', [x, y]);
                    else if (h < 0.3) this.tile('deco pip-stalagmite', [x, y]);
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
            // the old lamp lights up when Pip gets there
            const lit = frame.event === 'lamp';
            if (this.lamp) {
                this.lamp.classList.toggle('pip-lit', lit);
                this.lampGlow.classList.toggle('pip-lit', lit);
            }
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
