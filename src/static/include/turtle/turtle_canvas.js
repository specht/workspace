// The turtle on the page (docs/plan-katas.md, »Turtle«): the picture of a
// task card, the player that draws a program's commands as they come from
// turtle_worker.js, one worker per language, and the card's run, stop and
// check. tasks.js makes a TurtleCard for every card with a .turtle-stage.
//
// The geometry is TurtleWorld's (turtle_world.rb): the turtle starts in the
// middle, looks to the right, x to the right and y up, left is
// counter-clockwise; the stage shows -VIEW … VIEW both ways.
(function () {
    const VIEW = 250;
    const GRID = 50;
    // distance per second and degrees per second, as in turtle_worker.js
    const PACE = { 1: [90, 180], 2: [260, 540], 3: [1000, 2160] };
    const SPEEDS = [[1, 'Langsam'], [2, 'Normal'], [3, 'Schnell'], [0, 'Sofort']];
    const START = { x: 0, y: 0, heading: 0, pen: true, color: '#000000', width: 2 };
    // a check that runs this long without an end is given up
    const CHECK_SECONDS = 15;
    const VERSION = (document.currentScript && document.currentScript.src.split('?')[1]) || '';
    const LANGUAGE_NAMES = { ruby: 'Ruby', python: 'Python', javascript: 'JavaScript' };

    function storageGet(key) {
        try { return window.localStorage.getItem(key); } catch (e) { return null; }
    }

    function storageSet(key, value) {
        try { window.localStorage.setItem(key, value); } catch (e) { /* only a convenience */ }
    }

    function el(tag, attributes, children) {
        const node = document.createElement(tag);
        Object.entries(attributes || {}).forEach(([name, value]) => {
            if (name === 'text') node.textContent = value;
            else node.setAttribute(name, value);
        });
        (children || []).forEach((child) => node.appendChild(child));
        return node;
    }

    function duration(command, value, speed) {
        const pace = PACE[speed];
        if (!pace) return 0;
        if (command === 'forward' || command === 'back') return Math.abs(value) / pace[0] * 1000;
        if (command === 'left' || command === 'right') return Math.abs(value) / pace[1] * 1000;
        return 0;
    }

    // ----- the stage: paper with the picture asked for, ink, turtle -----

    class Stage {
        constructor(box) {
            this.box = box;
            this.paper = el('canvas', { class: 'turtle-paper', 'aria-hidden': 'true' });
            this.ink = el('canvas', { class: 'turtle-ink', 'aria-hidden': 'true' });
            this.top = el('canvas', { class: 'turtle-top', role: 'img', 'aria-label': 'Die Zeichenfläche der Schildkröte' });
            this.frame = el('div', { class: 'turtle-frame' }, [this.paper, this.ink, this.top]);
            box.appendChild(this.frame);
            this.target = null;
            this.diff = null;
            this.segments = [];
            this.turtle = Object.assign({}, START);
            this.resize();
            if (window.ResizeObserver) new ResizeObserver(() => this.resize()).observe(this.frame);
        }

        resize() {
            const css = this.frame.clientWidth || 400;
            const ratio = window.devicePixelRatio || 1;
            const px = Math.round(css * ratio);
            if (this.size === px) return;
            this.size = px;
            this.scale = px / (2 * VIEW);
            [this.paper, this.ink, this.top].forEach((c) => { c.width = px; c.height = px; });
            this.drawPaper();
            this.redrawInk();
            this.drawTop();
        }

        point(x, y) {
            return [(x + VIEW) * this.scale, (VIEW - y) * this.scale];
        }

        drawPaper() {
            const ctx = this.paper.getContext('2d');
            const s = this.size;
            ctx.fillStyle = '#fffdf8';
            ctx.fillRect(0, 0, s, s);
            ctx.lineWidth = Math.max(1, this.scale * 0.6);
            for (let v = -VIEW; v <= VIEW; v += GRID) {
                ctx.strokeStyle = v === 0 ? '#e2dbc9' : '#f0ebde';
                const [x] = this.point(v, 0);
                const [, y] = this.point(0, v);
                ctx.beginPath(); ctx.moveTo(x, 0); ctx.lineTo(x, s); ctx.stroke();
                ctx.beginPath(); ctx.moveTo(0, y); ctx.lineTo(s, y); ctx.stroke();
            }
            if (this.target) {
                ctx.lineCap = 'round';
                ctx.lineJoin = 'round';
                ctx.strokeStyle = '#cfc8b8';
                this.target.forEach(([x1, y1, x2, y2, , width]) => {
                    ctx.lineWidth = Math.max(5, (width + 4) * this.scale);
                    ctx.beginPath();
                    ctx.moveTo(...this.point(x1, y1));
                    ctx.lineTo(...this.point(x2, y2));
                    ctx.stroke();
                });
            }
        }

        setTarget(segments) {
            this.target = segments;
            this.drawPaper();
        }

        clear() {
            this.segments = [];
            this.diff = null;
            this.turtle = Object.assign({}, START);
            this.ink.getContext('2d').clearRect(0, 0, this.size, this.size);
            this.drawTop();
        }

        line(x1, y1, x2, y2, color, width) {
            const ctx = this.ink.getContext('2d');
            ctx.lineCap = 'round';
            ctx.lineJoin = 'round';
            ctx.strokeStyle = color;
            ctx.lineWidth = Math.max(1, width * this.scale);
            ctx.beginPath();
            ctx.moveTo(...this.point(x1, y1));
            ctx.lineTo(...this.point(x2, y2));
            ctx.stroke();
        }

        redrawInk() {
            this.ink.getContext('2d').clearRect(0, 0, this.size, this.size);
            this.segments.forEach((s) => this.line(...s));
        }

        // A command, done at once; a forward or back is drawn and kept.
        apply(command, value) {
            const t = this.turtle;
            if (command === 'forward' || command === 'back') {
                const d = command === 'back' ? -value : value;
                const r = t.heading * Math.PI / 180;
                const x = t.x + d * Math.cos(r);
                const y = t.y + d * Math.sin(r);
                if (t.pen && d !== 0) {
                    const s = [t.x, t.y, x, y, t.color, t.width];
                    this.segments.push(s);
                    this.line(...s);
                }
                t.x = x;
                t.y = y;
            } else if (command === 'left') t.heading += value;
            else if (command === 'right') t.heading -= value;
            else if (command === 'pen_up') t.pen = false;
            else if (command === 'pen_down') t.pen = true;
            else if (command === 'color') t.color = value;
            else if (command === 'width') t.width = value;
        }

        // The turtle itself, and what a check found: missing pieces dashed,
        // pieces too much in red.
        drawTop(pose) {
            const ctx = this.top.getContext('2d');
            ctx.clearRect(0, 0, this.size, this.size);
            if (this.diff) {
                ctx.lineCap = 'round';
                const ratio = window.devicePixelRatio || 1;
                ctx.lineWidth = Math.max(3 * ratio, 4 * this.scale);
                ctx.strokeStyle = '#e8590c';
                ctx.setLineDash([7 * ratio, 6 * ratio]);
                this.diff.missing.forEach(([x1, y1, x2, y2]) => {
                    ctx.beginPath(); ctx.moveTo(...this.point(x1, y1)); ctx.lineTo(...this.point(x2, y2)); ctx.stroke();
                });
                ctx.setLineDash([]);
                ctx.strokeStyle = 'rgba(214, 40, 40, 0.75)';
                ctx.lineWidth = Math.max(4 * ratio, 6 * this.scale);
                this.diff.extra.forEach(([x1, y1, x2, y2]) => {
                    ctx.beginPath(); ctx.moveTo(...this.point(x1, y1)); ctx.lineTo(...this.point(x2, y2)); ctx.stroke();
                });
            }
            this.drawTurtle(ctx, pose || this.turtle);
        }

        drawTurtle(ctx, t) {
            const ratio = window.devicePixelRatio || 1;
            const [px, py] = this.point(t.x, t.y);
            const u = ratio * 1.25;
            ctx.save();
            ctx.translate(px, py);
            ctx.rotate(-t.heading * Math.PI / 180);
            ctx.globalAlpha = 0.9;
            ctx.fillStyle = '#3f7d4e';
            [[5, 6.5], [5, -6.5], [-5, 6.5], [-5, -6.5]].forEach(([x, y]) => {
                ctx.beginPath(); ctx.ellipse(x * u, y * u, 2.4 * u, 1.7 * u, 0, 0, 2 * Math.PI); ctx.fill();
            });
            ctx.beginPath(); ctx.moveTo(-8 * u, 0); ctx.lineTo(-11 * u, 1.3 * u); ctx.lineTo(-11 * u, -1.3 * u); ctx.fill();
            ctx.fillStyle = '#6fbf73';
            ctx.beginPath(); ctx.arc(10.5 * u, 0, 3.2 * u, 0, 2 * Math.PI); ctx.fill();
            ctx.fillStyle = '#2f8a4b';
            ctx.strokeStyle = '#1f5e33';
            ctx.lineWidth = 1.2 * ratio;
            ctx.beginPath(); ctx.ellipse(0, 0, 8.5 * u, 7 * u, 0, 0, 2 * Math.PI); ctx.fill(); ctx.stroke();
            ctx.fillStyle = '#58b06c';
            ctx.beginPath(); ctx.ellipse(0, 0, 4.3 * u, 3.5 * u, 0, 0, 2 * Math.PI); ctx.fill();
            ctx.globalAlpha = 1;
            // the pen: its colour when it is down, a ring when it is up
            ctx.beginPath(); ctx.arc(0, 0, 2.2 * u, 0, 2 * Math.PI);
            if (t.pen) {
                ctx.fillStyle = t.color;
                ctx.fill();
                ctx.strokeStyle = '#fffdf8';
                ctx.lineWidth = 0.9 * ratio;
                ctx.stroke();
            } else {
                ctx.strokeStyle = '#fffdf8';
                ctx.lineWidth = 1.2 * ratio;
                ctx.stroke();
            }
            ctx.restore();
        }

        showDiff(diff) {
            this.diff = diff;
            this.drawTop();
        }
    }

    // ----- the player: commands drawn over the time they take -----

    class Player {
        constructor(stage, onOutput, onLine) {
            this.stage = stage;
            this.onOutput = onOutput;
            this.onLine = onLine;
            this.queue = [];
            this.active = null;
            this.lastEnd = 0;
            this.speed = 2;
            this.frameRequested = false;
            this.idle = null;
        }

        reset(speed) {
            this.queue = [];
            this.active = null;
            this.lastEnd = 0;
            this.speed = speed;
            this.idle = null;
            this.lastLine = null;
        }

        push(items) {
            items.forEach((item) => this.queue.push(item));
            this.request();
        }

        request() {
            if (this.frameRequested) return;
            this.frameRequested = true;
            requestAnimationFrame((now) => { this.frameRequested = false; this.frame(now); });
        }

        // run once nothing is left to draw
        whenIdle(callback) {
            this.idle = callback;
            this.request();
        }

        // stopped: what was on its way is not drawn any more
        halt() {
            if (this.active) this.finishActive();
            this.queue = [];
            this.idle = null;
            this.stage.drawTop();
        }

        finishActive() {
            const a = this.active;
            this.active = null;
            this.stage.turtle = a.from;
            this.stage.apply(a.command, a.value);
            this.lastEnd = a.start + a.ms;
        }

        take(item, now) {
            if (item[0] === 'out') {
                this.onOutput(item[1]);
                return;
            }
            const [command, value, line] = item;
            // the line in the editor, while one can follow it
            if (line && this.speed !== 0 && line !== this.lastLine && this.queue.length < 500) {
                this.lastLine = line;
                this.onLine(line);
            }
            const ms = this.queue.length > 3000 ? 0 : duration(command, value, this.speed);
            if (ms === 0) {
                this.stage.apply(command, value);
                return;
            }
            const start = this.lastEnd > now - 120 ? this.lastEnd : now;
            this.active = { command, value, ms, start, from: Object.assign({}, this.stage.turtle), drawn: [this.stage.turtle.x, this.stage.turtle.y] };
        }

        frame(now) {
            let budget = 20000;
            while (budget-- > 0) {
                if (!this.active) {
                    if (this.queue.length === 0) break;
                    this.take(this.queue.shift(), now);
                    continue;
                }
                const a = this.active;
                const p = (now - a.start) / a.ms;
                if (p >= 1) {
                    this.finishActive();
                    continue;
                }
                // part of the way: the line so far, the turtle where it is
                const t = Object.assign({}, a.from);
                if (a.command === 'forward' || a.command === 'back') {
                    const d = (a.command === 'back' ? -a.value : a.value) * p;
                    const r = t.heading * Math.PI / 180;
                    t.x = a.from.x + d * Math.cos(r);
                    t.y = a.from.y + d * Math.sin(r);
                    if (t.pen) this.stage.line(a.drawn[0], a.drawn[1], t.x, t.y, t.color, t.width);
                    a.drawn = [t.x, t.y];
                } else {
                    t.heading += (a.command === 'left' ? 1 : -1) * a.value * p;
                }
                this.stage.drawTop(t);
                this.request();
                return;
            }
            this.stage.drawTop();
            if (this.queue.length > 0 || this.active) {
                this.request();
                return;
            }
            if (this.idle) {
                const idle = this.idle;
                this.idle = null;
                idle();
            }
        }
    }

    // ----- one worker per language, shared by the cards of the page -----

    class Runner {
        constructor(language) {
            this.language = language;
            this.worker = null;
            this.owner = null;
        }

        start() {
            if (this.worker) return;
            this.worker = new Worker('/include/turtle/turtle_worker.js?' + VERSION, { type: 'module' });
            this.worker.onmessage = (e) => { if (this.owner) this.owner.message(e.data); };
            this.worker.onerror = () => {
                if (this.owner) this.owner.message({ type: 'error', index: 0, message: 'Dein Browser konnte das Programm nicht starten.', line: null, fatal: true });
                if (this.owner) this.owner.message({ type: 'done' });
            };
        }

        prepare() {
            this.start();
            this.worker.postMessage({ type: 'prepare', language: this.language });
        }

        run(owner, message) {
            if (this.owner && this.owner !== owner) this.owner.interrupted();
            this.start();
            this.owner = owner;
            this.worker.postMessage(Object.assign({ type: 'run', language: this.language }, message));
        }

        // The only way to end a program in the middle: a new worker, which
        // loads its runtime again in the background, from the browser's cache.
        stop(owner) {
            if (this.owner !== owner) return;
            this.owner = null;
            if (this.worker) this.worker.terminate();
            this.worker = null;
            setTimeout(() => { if (!this.worker) this.prepare(); }, 100);
        }

        release(owner) {
            if (this.owner === owner) this.owner = null;
        }
    }

    const runners = {};
    function runner(language) {
        if (!runners[language]) runners[language] = new Runner(language);
        return runners[language];
    }

    // ----- a task card -----

    // hooks: code(), mark(line), console(text, error), clearConsole(),
    // status(text), save(code), check(code, report, done), invalid(message),
    // busy(running)
    class TurtleCard {
        constructor(box, hooks) {
            this.box = box;
            this.hooks = hooks;
            this.language = box.dataset.language;
            this.runs = JSON.parse(box.dataset.runs || '[null]');
            this.stage = new Stage(box);
            this.player = new Player(this.stage, (text) => hooks.console(text, false), (line) => hooks.mark(line));
            this.running = null;
            const target = box.dataset.target ? JSON.parse(box.dataset.target) : null;
            if (target) this.stage.setTarget(target);
            this.caption = el('div', { class: 'turtle-caption' });
            this.setCaption(target, box.dataset.targetCall);
            this.speed = Number(storageGet('turtle-speed') || 2);
            if (!(this.speed in PACE) && this.speed !== 0) this.speed = 2;
            const bar = el('div', { class: 'turtle-bar' });
            const speeds = el('div', { class: 'btn-group btn-group-sm turtle-speeds', role: 'group', 'aria-label': 'Wie schnell die Schildkröte zeichnet' });
            this.speedButtons = SPEEDS.map(([value, label]) => {
                const b = el('button', { type: 'button', class: 'btn btn-outline-secondary', text: label, 'data-speed': String(value) });
                b.addEventListener('click', () => this.setSpeed(value));
                speeds.appendChild(b);
                return b;
            });
            this.state = el('span', { class: 'turtle-state', 'aria-live': 'polite' });
            bar.appendChild(speeds);
            bar.appendChild(this.state);
            box.appendChild(this.caption);
            box.appendChild(bar);
            this.setSpeed(this.speed);
        }

        setCaption(target, call) {
            this.caption.replaceChildren();
            if (!target) return;
            const swatch = el('span', { class: 'turtle-swatch', 'aria-hidden': 'true' });
            this.caption.appendChild(swatch);
            if (call) {
                this.caption.appendChild(document.createTextNode('So soll '));
                this.caption.appendChild(el('code', { text: call }));
                this.caption.appendChild(document.createTextNode(' aussehen.'));
            } else {
                this.caption.appendChild(document.createTextNode('So soll dein Bild aussehen.'));
            }
        }

        setSpeed(value) {
            this.speed = value;
            storageSet('turtle-speed', String(value));
            this.speedButtons.forEach((b) => {
                const on = Number(b.dataset.speed) === value;
                b.classList.toggle('active', on);
                b.setAttribute('aria-pressed', on ? 'true' : 'false');
            });
        }

        prepare() {
            runner(this.language).prepare();
        }

        toggle() {
            if (this.running) this.stop();
            else this.run();
        }

        run() {
            const code = this.hooks.code();
            if (!code.trim()) {
                this.hooks.invalid('Schreib zuerst ein Programm.');
                return;
            }
            const target = this.box.dataset.target ? JSON.parse(this.box.dataset.target) : null;
            this.stage.setTarget(target);
            this.setCaption(target, this.box.dataset.targetCall);
            this.stage.clear();
            this.player.reset(this.speed);
            this.hooks.clearConsole();
            this.hooks.save(code);
            this.running = { code, commands: [], error: null, phase: 'live' };
            this.hooks.busy(true);
            this.state.textContent = '';
            runner(this.language).run(this, { code, runs: [null], speed: this.speed });
        }

        stop() {
            if (!this.running) return;
            runner(this.language).stop(this);
            clearTimeout(this.running.timer);
            this.player.halt();
            this.running = null;
            this.hooks.mark(null);
            this.hooks.busy(false);
            this.state.textContent = 'Angehalten.';
        }

        // another card of the page started the same language
        interrupted() {
            if (!this.running) return;
            this.player.halt();
            this.running = null;
            this.hooks.mark(null);
            this.hooks.busy(false);
            this.state.textContent = 'Angehalten: Eine andere Aufgabe läuft gerade.';
        }

        message(m) {
            const r = this.running;
            if (!r) return;
            if (m.type === 'loading') {
                this.state.textContent = LANGUAGE_NAMES[m.language] + ' wird geladen …';
            } else if (m.type === 'start') {
                if (r.phase === 'live') this.state.textContent = '';
                else r.checkIndex = m.index;
            } else if (m.type === 'items') {
                if (r.phase === 'live') {
                    m.list.forEach((item) => { if (item[0] !== 'out') r.commands.push(item[1] === undefined || item[1] === null ? [item[0]] : [item[0], item[1]]); });
                    this.player.push(m.list);
                } else {
                    const run = r.checkRuns[r.checkIndex];
                    m.list.forEach((item) => { if (item[0] !== 'out') run.push(item[1] === undefined || item[1] === null ? [item[0]] : [item[0], item[1]]); });
                }
            } else if (m.type === 'error') {
                if (!r.error) r.error = { message: m.message, line: m.line };
            } else if (m.type === 'limit') {
                r.error = { message: 'Die Schildkröte hat aufgehört: ' + (m.commands ? 'mehr als 200 000 Befehle.' : 'Dein Programm hat zu viel ausgegeben.'), line: null, limit: true };
                runner(this.language).stop(this);
                this.finish();
            } else if (m.type === 'done') {
                if (r.phase === 'live') this.player.whenIdle(() => this.finish());
                else this.checkDone();
            }
        }

        // The program ended by itself (or broke off): the check, for which
        // calls run once more, each from the start, at once.
        finish() {
            const r = this.running;
            if (!r) return;
            this.hooks.mark(null);
            if (r.error) {
                this.hooks.console((r.error.line ? 'Zeile ' + r.error.line + ': ' : '') + r.error.message, true);
                if (r.error.line) this.hooks.mark(r.error.line);
                if (r.error.limit) {
                    this.running = null;
                    this.hooks.busy(false);
                    return;
                }
                this.send({ runs: null, error: r.error.message, line: r.error.line });
                return;
            }
            if (this.runs.length === 1 && this.runs[0] === null) {
                this.send({ runs: [r.commands], error: null });
                return;
            }
            r.phase = 'check';
            r.checkRuns = this.runs.map(() => []);
            r.checkIndex = 0;
            this.state.textContent = 'Wird geprüft …';
            r.timer = setTimeout(() => {
                if (this.running !== r) return;
                runner(this.language).stop(this);
                this.running = null;
                this.hooks.busy(false);
                this.state.textContent = '';
                this.hooks.invalid('Beim Prüfen lief dein Programm länger als ' + CHECK_SECONDS + ' Sekunden. Steckt eine Endlosschleife darin, die nicht in einer Funktion ist?');
            }, CHECK_SECONDS * 1000);
            runner(this.language).run(this, { code: r.code, runs: this.runs, speed: 0 });
        }

        checkDone() {
            const r = this.running;
            clearTimeout(r.timer);
            if (r.error) {
                this.send({ runs: null, error: r.error.message, line: r.error.line });
                return;
            }
            this.send({ runs: r.checkRuns, error: null });
        }

        send(report) {
            const r = this.running;
            runner(this.language).release(this);
            this.state.textContent = '';
            this.hooks.check(r.code, report, () => {
                this.running = null;
                this.hooks.busy(false);
            });
        }

        // What a check found: the run it is about, the picture asked for
        // in grey, what is missing dashed and what is too much in red.
        showResult(show) {
            if (!show) return;
            if (show.call) {
                this.stage.setTarget(show.expected);
                this.setCaption(show.expected, show.call);
                this.stage.clear();
                show.drawn.forEach((s) => { this.stage.segments.push(s); this.stage.line(...s); });
            }
            this.stage.showDiff({ missing: show.missing, extra: show.extra });
        }
    }

    // A picture only, for visitors: the paper with what the task asks for.
    function showOnly(box) {
        const stage = new Stage(box);
        if (box.dataset.target) stage.setTarget(JSON.parse(box.dataset.target));
    }

    window.TurtleCard = TurtleCard;
    window.TurtleCard.showOnly = showOnly;
})();
