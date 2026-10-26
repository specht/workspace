// Pip unter Funkelbach (PipCheck, docs/plan-katas.md): the map of a Pip
// task and the playback of a run. The server ran the plan against the world
// and sends every frame (PipWorld#trace); this only draws them. What stays
// the same thing moves (Pip, the Glibbers walk from tile to tile, a stone
// that is taken fades), and each step marks its line in the editor
// (onLine). Tiles are the 24-pixel sprites of /images/pip/, scaled so the
// map fits the card.
(function () {
    const SPRITES = '/images/pip/';
    const MAX_TILE = 48;
    const SPEEDS = [700, 450, 280, 160, 80];
    const SAY = { wall: 'Autsch!', caught: 'Iiih, Glibber!', no_stone: 'Hier ist nichts …', no_lever: 'Kein Hebel?', steps: 'Puh …', lamp: 'Licht!' };

    function el(tag, className, parent) {
        const node = document.createElement(tag);
        if (className) node.className = className;
        if (parent) parent.appendChild(node);
        return node;
    }

    const key = (p) => p[0] + ',' + p[1];

    class PipView {
        // box: the element the map goes into; layout: PipWorld#layout;
        // start: the first frame (where Pip stands, the Glibbers).
        constructor(box, layout, start) {
            this.box = box;
            this.layout = layout;
            this.world = el('div', 'pip-map', box);
            if (!box.closest('[role=img]')) {
                this.world.setAttribute('role', 'img');
                this.world.setAttribute('aria-label', 'Die Karte: Pip, die Wände, die Funkensteine und die Lampe.');
            }
            this.stones = {};
            this.resize();
            const put = (list, name) => (list || []).forEach((p) => this.tile(name, p));
            put(layout.walls, 'wall');
            put(layout.trail, 'trail');
            put(layout.puddles, 'puddle');
            put(layout.gates, 'gate');
            this.levers = (layout.levers || []).map((p) => this.tile('lever', p));
            if (layout.lamp) this.tile('lamp', layout.lamp);
            if (layout.bello) this.tile('bello', layout.bello);
            (layout.stones || []).forEach((p) => { this.stones[key(p)] = this.tile('stone', p); });
            (layout.labels || []).forEach((label) => { this.tile('label', label.at).textContent = label.text; });
            this.glibbers = (start.glibbers || []).map((p) => this.tile('glibber actor', p));
            this.pip = this.tile('pip actor', [start.pip.x, start.pip.y]);
            this.bubble = el('div', 'pip-bubble', this.world);
            this.bubble.hidden = true;
            this.show(start, false);
            if (window.ResizeObserver) new ResizeObserver(() => this.resize()).observe(box);
        }

        resize() {
            const width = this.box.clientWidth || 400;
            const size = Math.max(12, Math.min(MAX_TILE, Math.floor(width / this.layout.width)));
            this.world.style.setProperty('--tile', size + 'px');
            this.world.style.width = size * this.layout.width + 'px';
            this.world.style.height = size * this.layout.height + 'px';
        }

        tile(name, p) {
            const node = el('div', 'pip-tile pip-' + name, this.world);
            node.style.setProperty('--x', p[0]);
            node.style.setProperty('--y', p[1]);
            return node;
        }

        // A frame of the run: animate: walk there in duration ms.
        show(frame, animate, duration) {
            this.world.style.setProperty('--step', (animate ? duration : 0) + 'ms');
            const pip = frame.pip;
            this.pip.style.setProperty('--x', pip.x);
            this.pip.style.setProperty('--y', pip.y);
            this.pip.dataset.facing = pip.facing;
            this.pip.classList.toggle('pip-walking', animate && frame.event === 'move');
            this.pip.classList.toggle('pip-bump', frame.event === 'wall');
            this.pip.classList.toggle('pip-caught', frame.event === 'caught');
            (frame.glibbers || []).forEach((g, i) => {
                if (!this.glibbers[i]) return;
                this.glibbers[i].style.setProperty('--x', g[0]);
                this.glibbers[i].style.setProperty('--y', g[1]);
            });
            const left = new Set((frame.stones || []).map(key));
            Object.keys(this.stones).forEach((k) => this.stones[k].classList.toggle('pip-taken', !left.has(k)));
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

    // The player under the map: Zurück, Abspielen/Pause, Weiter, the speed,
    // and a line that says the step. onLine(line or null) marks the editor.
    class PipPlayer {
        constructor(box, layout, start, onLine) {
            this.box = box;
            this.onLine = onLine || (() => {});
            this.mapBox = el('div', 'pip-map-box', box);
            this.status = el('p', 'pip-status', box);
            this.status.setAttribute('aria-live', 'polite');
            const bar = el('div', 'pip-controls', box);
            this.back = this.button(bar, 'bi-chevron-left', 'Zurück', () => this.go(this.index - 1));
            this.playButton = this.button(bar, 'bi-play-fill', 'Abspielen', () => this.toggle());
            this.next = this.button(bar, 'bi-chevron-right', 'Weiter', () => this.go(this.index + 1));
            const speed = el('label', 'pip-speed', bar);
            speed.textContent = 'Tempo ';
            this.speed = el('input', null, speed);
            this.speed.type = 'range';
            this.speed.min = 0;
            this.speed.max = SPEEDS.length - 1;
            this.speed.value = 2;
            this.load({ layout: layout, frames: [start] });
        }

        button(bar, icon, label, action) {
            const b = el('button', 'btn btn-sm btn-outline-secondary pip-button', bar);
            b.type = 'button';
            b.title = label;
            b.innerHTML = "<i class='bi " + icon + "' aria-hidden='true'></i><span class='visually-hidden'>" + label + '</span>';
            b.addEventListener('click', action);
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

        duration() {
            return SPEEDS[Number(this.speed.value)];
        }

        go(index, animate) {
            this.index = Math.max(0, Math.min(this.frames.length - 1, index));
            this.view.show(this.frames[this.index], animate, this.duration());
            this.update();
        }

        update() {
            const frame = this.frames[this.index];
            const maps = this.replay.maps > 1 ? 'Karte ' + this.replay.map + ' von ' + this.replay.maps + ' · ' : '';
            const step = this.frames.length > 1 ? 'Schritt ' + frame.step + (frame.line ? ', Zeile ' + frame.line : '') : 'Noch nicht gelaufen';
            this.status.textContent = maps + step;
            this.back.disabled = this.index === 0;
            this.next.disabled = this.index >= this.frames.length - 1;
            this.onLine(this.frames.length > 1 ? frame.line : null);
        }

        toggle() {
            if (this.timer) this.stop();
            else this.play();
        }

        play() {
            if (this.index >= this.frames.length - 1) this.go(0);
            this.playButton.innerHTML = "<i class='bi bi-pause-fill' aria-hidden='true'></i><span class='visually-hidden'>Pause</span>";
            const tick = () => {
                if (this.index >= this.frames.length - 1) return this.stop();
                this.go(this.index + 1, true);
                this.timer = setTimeout(tick, this.duration());
            };
            this.timer = setTimeout(tick, 200);
        }

        stop() {
            clearTimeout(this.timer);
            this.timer = null;
            if (this.playButton) this.playButton.innerHTML = "<i class='bi bi-play-fill' aria-hidden='true'></i><span class='visually-hidden'>Abspielen</span>";
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
