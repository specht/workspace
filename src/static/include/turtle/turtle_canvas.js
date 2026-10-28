// The turtle on the page (docs/plan-katas.md, »Turtle«): the picture of a
// task card, the player that draws a program's commands as they come from
// turtle_worker.js, one worker per language, and the card's run, stop and
// check. tasks.js makes a TurtleCard for every card with a .turtle-stage.
//
// The geometry is TurtleWorld's (turtle_world.rb): the turtle starts at
// (0, 0), looks to the right, x to the right and y up, left is
// counter-clockwise. The paper shows the picture the task asks for, with a
// margin, at least MIN_HALF steps from its middle to each side (VIEW
// without a picture), and zooms out when a drawing goes past its edge; its
// lines stay GRID steps apart.
(function () {
    const VIEW = 250;
    const MIN_HALF = 150;
    const MARGIN = 1.2;
    const GRID = 50;
    const GLIDE_MS = 600;
    // how faint the picture before stays behind a level picture
    const GHOST = 0.18;
    // How fast the turtle draws: pixels of line per second on the paper as
    // it is zoomed, however many turns there are, so that a fern of 40000
    // short lines takes no longer than a square of four long ones. Turns
    // take no time, but at »Langsam«, where one is meant to watch the
    // turtle turn, at most TURN_MS.
    const PACE = { 1: 100, 2: 300, 3: 2000 };
    const TURN_SPEED = 360;
    const TURN_MS = 250;
    const LINE_MS = 150;
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

    // 0 (black) to 1 (white), of a colour as #rrggbb
    function luminance(hex) {
        const c = [1, 3, 5].map((i) => parseInt(hex.slice(i, i + 2), 16) / 255);
        return 0.2126 * c[0] + 0.7152 * c[1] + 0.0722 * c[2];
    }

    // the grid on the paper: faint, a little stronger at the axes; on a
    // paper of the program's own colour, lighter or darker than it
    function gridColor(axis, dark, background) {
        if (!background) return axis ? '#e2dbc9' : '#f0ebde';
        if (dark) return axis ? 'rgba(255, 255, 255, 0.2)' : 'rgba(255, 255, 255, 0.09)';
        return axis ? 'rgba(0, 0, 0, 0.14)' : 'rgba(0, 0, 0, 0.06)';
    }

    // in ms; pixels: the pixels on the screen one step of the turtle takes
    function duration(command, value, speed, pixels) {
        const pace = PACE[speed];
        if (!pace) return 0;
        if (command === 'forward' || command === 'back') return Math.abs(value) * pixels / pace * 1000;
        if (speed === 1 && (command === 'left' || command === 'right')) return Math.min(TURN_MS, Math.abs(value) / TURN_SPEED * 1000);
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
            // the student switched the picture asked for off (TurtleCard)
            this.hideTarget = false;
            this.diff = null;
            this.segments = [];
            this.turtle = Object.assign({}, START);
            this.saved = [];
            this.background = null;
            this.view = { cx: 0, cy: 0, half: VIEW };
            // the widest line in pixels on the screen (a level picture
            // zoomed in far)
            this.maxWidth = Infinity;
            this.resize();
            if (window.ResizeObserver) new ResizeObserver(() => this.resize()).observe(this.frame);
        }

        resize() {
            const css = this.frame.clientWidth || 400;
            const ratio = window.devicePixelRatio || 1;
            const px = Math.round(css * ratio);
            if (this.size === px) return;
            this.size = px;
            [this.paper, this.ink, this.top].forEach((c) => { c.width = px; c.height = px; });
            this.redraw();
        }

        redraw() {
            this.scale = this.size / (2 * this.view.half);
            this.drawPaper();
            this.redrawInk();
            this.drawTop(this.pose);
        }

        // the pixels on the screen one step takes, as the paper is zoomed now
        pixels() {
            return this.scale / (window.devicePixelRatio || 1);
        }

        point(x, y) {
            const v = this.view;
            return [(x - v.cx + v.half) * this.scale, (v.half - (y - v.cy)) * this.scale];
        }

        // The view around the picture asked for and the start, or the
        // middle of the paper without one.
        // minHalf: how far in it may zoom (a level picture: as far as it
        // takes to fill the paper)
        fit(segments, minHalf = MIN_HALF) {
            const around = segments || (this.hideTarget ? null : this.target);
            if (!around || around.length === 0) {
                this.settle({ cx: 0, cy: 0, half: VIEW });
                return;
            }
            let [x0, y0, x1, y1] = [0, 0, 0, 0];
            around.forEach(([a, b, c, d]) => {
                x0 = Math.min(x0, a, c); x1 = Math.max(x1, a, c);
                y0 = Math.min(y0, b, d); y1 = Math.max(y1, b, d);
            });
            this.settle({ cx: (x0 + x1) / 2, cy: (y0 + y1) / 2, half: Math.max(minHalf, Math.max(x1 - x0, y1 - y0) / 2 * MARGIN) });
        }

        // The turtle at (x, y) past the edge: the paper follows the drawing,
        // gliding (glide) to fit everything drawn, the turtle and the
        // picture asked for, with room to grow; it moves as well as zooms
        // out, so that a drawing that wanders off to one side stays in the
        // middle (the maintainer).
        ensure(x, y) {
            const v = this.goal || this.view;
            const edge = v.half * 0.97;
            if (Math.abs(x - v.cx) <= edge && Math.abs(y - v.cy) <= edge) return;
            let [x0, y0, x1, y1] = [x, y, x, y];
            const grow = ([a, b, c, d]) => {
                x0 = Math.min(x0, a, c); x1 = Math.max(x1, a, c);
                y0 = Math.min(y0, b, d); y1 = Math.max(y1, b, d);
            };
            this.segments.forEach(grow);
            if (!this.hideTarget) (this.target || []).forEach(grow);
            const half = Math.max(MIN_HALF, v.half, Math.max(x1 - x0, y1 - y0) / 2 * 1.3);
            this.glide({ cx: (x0 + x1) / 2, cy: (y0 + y1) / 2, half });
        }

        // A run is over: the paper glides to everything drawn and the turtle
        // (and the picture asked for, when it shows), centred, also zooming
        // in again, so that a drawing that wandered off ends in the middle.
        frameAll() {
            const t = this.turtle;
            let [x0, y0, x1, y1] = [t.x, t.y, t.x, t.y];
            const grow = ([a, b, c, d]) => {
                x0 = Math.min(x0, a, c); x1 = Math.max(x1, a, c);
                y0 = Math.min(y0, b, d); y1 = Math.max(y1, b, d);
            };
            this.segments.forEach(grow);
            if (!this.hideTarget) (this.target || []).forEach(grow);
            if (this.segments.length === 0) return;
            const to = { cx: (x0 + x1) / 2, cy: (y0 + y1) / 2, half: Math.max(MIN_HALF, Math.max(x1 - x0, y1 - y0) / 2 * MARGIN) };
            const v = this.goal || this.view;
            // already about there: no glide for nothing
            if (Math.abs(to.half / v.half - 1) < 0.05 && Math.abs(to.cx - v.cx) < v.half * 0.05 && Math.abs(to.cy - v.cy) < v.half * 0.05) return;
            this.glide(to);
        }

        // The view moves to another over GLIDE_MS instead of jumping, the
        // ink drawn again in every frame; the zoom changes evenly (half
        // geometrically), slow at the start and the end.
        glide(to) {
            const from = Object.assign({}, this.view);
            const start = performance.now();
            this.goal = to;
            if (this.gliding) cancelAnimationFrame(this.gliding);
            const step = (now) => {
                const p = Math.min(1, Math.max(0, (now - start) / GLIDE_MS));
                const e = p < 0.5 ? 2 * p * p : 1 - 2 * (1 - p) * (1 - p);
                this.view = { cx: from.cx + (to.cx - from.cx) * e, cy: from.cy + (to.cy - from.cy) * e, half: from.half * Math.pow(to.half / from.half, e) };
                if (this.ghost) this.ghostAlpha = 1 + (GHOST - 1) * Math.min(1, 1.5 * e);
                this.redraw();
                if (p < 1) {
                    this.gliding = requestAnimationFrame(step);
                } else {
                    this.gliding = null;
                    this.goal = null;
                }
            };
            this.gliding = requestAnimationFrame(step);
        }

        // where the view is headed and stays; a new picture jumps there
        settle(to) {
            if (this.gliding) cancelAnimationFrame(this.gliding);
            this.gliding = null;
            this.goal = null;
            this.view = to;
        }

        drawPaper() {
            const ctx = this.paper.getContext('2d');
            const s = this.size;
            // the paper, or the colour the program gave it (background)
            ctx.fillStyle = this.background || '#fffdf8';
            ctx.fillRect(0, 0, s, s);
            const dark = this.background && luminance(this.background) < 0.45;
            const ratio = window.devicePixelRatio || 1;
            ctx.lineWidth = ratio;
            const v = this.view;
            // the grid, unless its lines would sit too close together
            if (GRID * this.scale >= 7 * ratio) {
                for (let g = Math.ceil((v.cx - v.half) / GRID) * GRID; g <= v.cx + v.half; g += GRID) {
                    ctx.strokeStyle = gridColor(g === 0, dark, this.background);
                    const [x] = this.point(g, 0);
                    ctx.beginPath(); ctx.moveTo(x, 0); ctx.lineTo(x, s); ctx.stroke();
                }
                for (let g = Math.ceil((v.cy - v.half) / GRID) * GRID; g <= v.cy + v.half; g += GRID) {
                    ctx.strokeStyle = gridColor(g === 0, dark, this.background);
                    const [, y] = this.point(0, g);
                    ctx.beginPath(); ctx.moveTo(0, y); ctx.lineTo(s, y); ctx.stroke();
                }
            }
            // the picture asked for: a thin pencil line under the ink, which
            // covers it where the drawing is right and leaves it where not
            if (this.target && !this.hideTarget) {
                ctx.lineCap = 'round';
                ctx.lineJoin = 'round';
                ctx.strokeStyle = '#ddd6c6';
                this.target.forEach(([x1, y1, x2, y2, , width]) => {
                    ctx.lineWidth = Math.max(2 * ratio, Math.min(width * this.scale, 6 * ratio) + ratio);
                    ctx.beginPath();
                    ctx.moveTo(...this.point(x1, y1));
                    ctx.lineTo(...this.point(x2, y2));
                    ctx.stroke();
                });
            }
        }

        setTarget(segments) {
            this.target = segments;
            this.fit();
            this.redraw();
        }

        clear() {
            this.segments = [];
            this.ghost = null;
            this.partial = null;
            this.diff = null;
            this.turtle = Object.assign({}, START);
            this.saved = [];
            this.background = null;
            this.fit();
            this.redraw();
        }

        line(x1, y1, x2, y2, color, width) {
            const ctx = this.ink.getContext('2d');
            ctx.lineCap = 'round';
            ctx.lineJoin = 'round';
            ctx.strokeStyle = color;
            ctx.lineWidth = Math.max(1, Math.min(width * this.scale, this.maxWidth * (window.devicePixelRatio || 1)));
            ctx.beginPath();
            ctx.moveTo(...this.point(x1, y1));
            ctx.lineTo(...this.point(x2, y2));
            ctx.stroke();
        }

        redrawInk() {
            const ctx = this.ink.getContext('2d');
            ctx.clearRect(0, 0, this.size, this.size);
            // the picture before, faint behind this one (a level picture)
            if (this.ghost) {
                ctx.globalAlpha = this.ghostAlpha;
                this.ghost.forEach((s) => this.line(...s));
                ctx.globalAlpha = 1;
            }
            this.segments.forEach((s) => this.line(...s));
            // the line on its way (Player)
            if (this.partial) this.line(...this.partial);
        }

        // A command, done at once; a forward or back is drawn and kept.
        apply(command, value) {
            const t = this.turtle;
            if (command === 'forward' || command === 'back') {
                const d = command === 'back' ? -value : value;
                const r = t.heading * Math.PI / 180;
                const x = t.x + d * Math.cos(r);
                const y = t.y + d * Math.sin(r);
                this.ensure(x, y);
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
            else if (command === 'background') {
                this.background = value;
                this.drawPaper();
            }
            // push remembers all of the turtle, pop goes back there
            else if (command === 'push') this.saved.push(Object.assign({}, t));
            else if (command === 'pop' && this.saved.length > 0) this.turtle = this.saved.pop();
        }

        // The turtle itself, and what a check found: missing pieces dashed,
        // pieces too much in red.
        drawTop(pose) {
            // the turtle on its way, also when the view glides
            this.pose = pose || null;
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
            this.scale = 1;
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
            this.lineShown = 0;
            // a level picture draws faster when it would take long (levels)
            this.scale = 1;
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

        // The line on its way goes on at the new speed from where it is.
        setSpeed(speed) {
            this.speed = speed;
            const a = this.active;
            if (a && speed === 0) this.finishActive();
            else if (a) {
                const now = performance.now();
                const p = Math.min(1, (now - a.start) / a.ms);
                a.ms = Math.max(1, this.duration(a.command, a.value));
                a.start = now - p * a.ms;
            }
            this.request();
        }

        duration(command, value) {
            return duration(command, value, this.speed, this.stage.pixels()) * this.scale;
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
            this.stage.partial = null;
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
            // the line in the editor, while one can follow it: at »Schnell«
            // a new one at most every LINE_MS
            if (line && this.speed !== 0 && line !== this.lastLine && (this.speed !== 3 || now - this.lineShown > LINE_MS)) {
                this.lastLine = line;
                this.lineShown = now;
                this.onLine(line);
            }
            const ms = this.duration(command, value);
            if (ms === 0) {
                this.stage.apply(command, value);
                return;
            }
            const start = this.lastEnd > now - 120 ? this.lastEnd : now;
            this.active = { command, value, ms, start, from: Object.assign({}, this.stage.turtle), drawn: [this.stage.turtle.x, this.stage.turtle.y] };
        }

        // At most about 12 ms of drawing per frame, so that the page keeps
        // answering clicks while a large picture appears at once.
        frame(now) {
            const deadline = performance.now() + 12;
            while (performance.now() < deadline) {
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
                    this.stage.ensure(t.x, t.y);
                    if (t.pen) {
                        this.stage.line(a.drawn[0], a.drawn[1], t.x, t.y, t.color, t.width);
                        this.stage.partial = [a.from.x, a.from.y, t.x, t.y, t.color, t.width];
                    }
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
    // the turtle cards of the page, which share their speed
    const cards = [];
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
            const card = box.closest('[data-task]');
            this.taskKey = card ? card.dataset.task : '';
            this.stage.hideTarget = !!target && storageGet('turtle-trace-hidden:' + this.taskKey) === '1';
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
            cards.push(this);
            bar.appendChild(speeds);
            bar.appendChild(this.state);
            box.appendChild(this.caption);
            box.appendChild(bar);
            this.setSpeed(this.speed, true);
        }

        setCaption(target, call) {
            this.caption.replaceChildren();
            if (!target) return;
            // the picture asked for can be switched off, for experiments
            // (the maintainer); remembered per task in this browser
            const toggle = el('button', { type: 'button', class: 'btn btn-link btn-sm turtle-trace-toggle' });
            const show = () => {
                const hidden = this.stage.hideTarget;
                toggle.replaceChildren(el('i', { class: 'bi ' + (hidden ? 'bi-eye' : 'bi-eye-slash'), 'aria-hidden': 'true' }), document.createTextNode(hidden ? 'einblenden' : 'ausblenden'));
                toggle.setAttribute('aria-label', hidden ? 'Das Bild der Aufgabe einblenden' : 'Das Bild der Aufgabe ausblenden');
            };
            toggle.addEventListener('click', () => {
                this.setTraceHidden(!this.stage.hideTarget);
                show();
            });
            this.traceToggle = show;
            show();
            const swatch = el('span', { class: 'turtle-swatch', 'aria-hidden': 'true' });
            this.caption.appendChild(swatch);
            if (call) {
                this.caption.appendChild(document.createTextNode('So soll '));
                this.caption.appendChild(el('code', { text: call }));
                this.caption.appendChild(document.createTextNode(' aussehen.'));
            } else {
                this.caption.appendChild(document.createTextNode('So soll dein Bild aussehen.'));
            }
            this.caption.appendChild(toggle);
        }

        setTraceHidden(hidden) {
            this.stage.hideTarget = hidden;
            storageSet('turtle-trace-hidden:' + this.taskKey, hidden ? '1' : '');
            this.stage.redraw();
            if (this.traceToggle) this.traceToggle();
        }

        // also while a program runs (Player.setSpeed); one speed for all
        // turtle cards of the page, remembered for the next one
        setSpeed(value, fromAll) {
            if (!fromAll) {
                cards.forEach((card) => { if (card !== this) card.setSpeed(value, true); });
                storageSet('turtle-speed', String(value));
            }
            this.speed = value;
            this.player.setSpeed(value);
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

        // another drawing opened (tasks.js, sketchBook): an empty paper
        blank() {
            this.lastRun = null;
            this.player.halt();
            this.stage.clear();
            this.state.textContent = '';
            this.hooks.clearConsole();
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
            this.stage.frameAll();
            // an endless drawing, stopped, may be handed in as far as it got
            if (this.running.phase === 'live') this.lastRun = { code: this.running.code, commands: this.running.commands.slice() };
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
            this.stage.frameAll();
            // what it drew, for handing in a drawing (tasks.js, sketchBook)
            this.lastRun = { code: r.code, commands: r.commands.slice() };
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
            // the differences are against the picture asked for: it shows
            if (this.stage.hideTarget) this.setTraceHidden(false);
            if (show.call) {
                this.stage.setTarget(show.expected);
                this.setCaption(show.expected, show.call);
                this.stage.clear();
                show.drawn.forEach((s) => {
                    this.stage.ensure(s[0], s[1]);
                    this.stage.ensure(s[2], s[3]);
                    this.stage.segments.push(s);
                    this.stage.line(...s);
                });
            }
            this.stage.showDiff({ missing: show.missing, extra: show.extra });
        }
    }

    // A picture only, for visitors: the paper with what the task asks for.
    function showOnly(box) {
        const stage = new Stage(box);
        if (box.dataset.target) stage.setTarget(JSON.parse(box.dataset.target));
    }

    // ----- how a recursive figure is built, level by level -----

    // The lines a list of commands draws, without drawing them.
    function trace(commands) {
        let t = Object.assign({}, START);
        const saved = [];
        const out = [];
        commands.forEach(([command, value]) => {
            if (command === 'forward' || command === 'back') {
                const d = command === 'back' ? -value : value;
                const r = t.heading * Math.PI / 180;
                const x = t.x + d * Math.cos(r);
                const y = t.y + d * Math.sin(r);
                if (t.pen) out.push([t.x, t.y, x, y, t.color, t.width]);
                t.x = x;
                t.y = y;
            } else if (command === 'left') t.heading += value;
            else if (command === 'right') t.heading -= value;
            else if (command === 'pen_up') t.pen = false;
            else if (command === 'pen_down') t.pen = true;
            else if (command === 'color') t.color = value;
            else if (command === 'width') t.width = value;
            else if (command === 'push') saved.push(Object.assign({}, t));
            else if (command === 'pop' && saved.length > 0) t = saved.pop();
        });
        return out;
    }

    // the parts of a level in these colours, what joins them in grey
    const PARTS = ['#2f9e44', '#1c7ed6', '#e8590c', '#ae3ec9'];
    const JOIN = '#868e96';
    const CONNECT = '#212529';

    // Each figure: its levels, the commands of a level (the parts made of
    // the level below in PARTS), and what to say about it.
    const FIGURES = {
        koch: {
            levels: 4,
            commands(level) {
                const out = [['width', 3]];
                const koch = (size, depth) => {
                    if (depth === 0) { out.push(['forward', size]); return; }
                    koch(size / 3, depth - 1); out.push(['left', 60]);
                    koch(size / 3, depth - 1); out.push(['right', 120]);
                    koch(size / 3, depth - 1); out.push(['left', 60]);
                    koch(size / 3, depth - 1);
                };
                if (level === 0) { koch(243, 0); return out; }
                [['left', 60], ['right', 120], ['left', 60], null].forEach((turn, i) => {
                    out.push(['color', PARTS[i]]);
                    koch(81, level - 1);
                    if (turn) out.push(turn);
                });
                return out;
            },
            says(level) {
                if (level === 0) return 'Stufe 0: eine gerade Linie.';
                return 'Stufe ' + level + ': vier Kurven der Stufe ' + (level - 1) + ', jede ein Drittel so lang, in vier Farben. Zwischen ihnen dreht sich die Schildkröte 60 Grad nach links, 120 nach rechts und 60 nach links.';
            },
        },
        sierpinski: {
            levels: 4,
            commands(level) {
                const out = [['width', 2]];
                const tri = (size, depth) => {
                    if (depth === 0) { for (let i = 0; i < 3; i++) out.push(['forward', size], ['left', 120]); return; }
                    const half = size / 2;
                    tri(half, depth - 1);
                    out.push(['pen_up'], ['forward', half], ['pen_down']);
                    tri(half, depth - 1);
                    out.push(['pen_up'], ['back', half], ['left', 60], ['forward', half], ['right', 60], ['pen_down']);
                    tri(half, depth - 1);
                    out.push(['pen_up'], ['left', 60], ['back', half], ['right', 60], ['pen_down']);
                };
                if (level === 0) { tri(256, 0); return out; }
                const half = 128;
                out.push(['color', PARTS[0]]); tri(half, level - 1);
                out.push(['pen_up'], ['forward', half], ['pen_down'], ['color', PARTS[1]]); tri(half, level - 1);
                out.push(['pen_up'], ['back', half], ['left', 60], ['forward', half], ['right', 60], ['pen_down'], ['color', PARTS[2]]); tri(half, level - 1);
                return out;
            },
            says(level) {
                if (level === 0) return 'Stufe 0: ein Dreieck mit drei gleich langen Seiten.';
                return 'Stufe ' + level + ': drei Dreiecke der Stufe ' + (level - 1) + ', halb so groß, in drei Farben: links unten, rechts unten und oben.';
            },
        },
        tree: {
            levels: 5,
            commands(level) {
                const out = [['left', 90]];
                const tree = (size, depth) => {
                    if (depth === 0) return;
                    out.push(['forward', size], ['left', 30]);
                    tree(size * 0.7, depth - 1);
                    out.push(['right', 60]);
                    tree(size * 0.7, depth - 1);
                    out.push(['left', 30], ['pen_up'], ['back', size], ['pen_down']);
                };
                if (level === 0) return out;
                out.push(['color', JOIN], ['width', 4], ['forward', 100], ['left', 30], ['width', 2], ['color', PARTS[0]]);
                tree(70, level - 1);
                out.push(['right', 60], ['color', PARTS[1]]);
                tree(70, level - 1);
                return out;
            },
            says(level) {
                if (level === 0) return 'Stufe 0: nichts. Hier hört die Funktion auf.';
                return 'Stufe ' + level + ': ein Stamm (grau), an dem links und rechts je ein Baum der Stufe ' + (level - 1) + ' wächst, 0,7-mal so groß und 30 Grad zur Seite gedreht.';
            },
        },
        hilbert: {
            levels: 5,
            commands(level) {
                const out = [['width', 3]];
                const size = level === 0 ? 0 : 240 / (Math.pow(2, level) - 1);
                const hilbert = (depth, angle) => {
                    if (depth === 0) return;
                    out.push(['right', angle]); hilbert(depth - 1, -angle);
                    out.push(['forward', size], ['left', angle]); hilbert(depth - 1, angle);
                    out.push(['forward', size]); hilbert(depth - 1, angle);
                    out.push(['left', angle], ['forward', size]); hilbert(depth - 1, -angle);
                    out.push(['right', angle]);
                };
                if (level === 0) return out;
                // the connecting lines black, as unlike the four colours of
                // the parts as can be (grey was too close to them)
                const part = (i, angle) => { out.push(['color', PARTS[i]]); hilbert(level - 1, angle); out.push(['color', CONNECT]); };
                out.push(['color', CONNECT], ['right', 90]); part(0, -90);
                out.push(['forward', size], ['left', 90]); part(1, 90);
                out.push(['forward', size]); part(2, 90);
                out.push(['left', 90], ['forward', size]); part(3, -90);
                out.push(['right', 90]);
                return out;
            },
            says(level) {
                if (level === 0) return 'Stufe 0: nichts.';
                if (level === 1) return 'Stufe 1: drei schwarze Linien, wie ein U. Die vier Kurven der Stufe 0 an den Ecken sind leer.';
                return 'Stufe ' + level + ': vier Kurven der Stufe ' + (level - 1) + ' in vier Farben, verbunden durch drei schwarze Linien wie bei Stufe 1. Die erste (grün) und die letzte (lila) sind gespiegelt: Sie drehen sich jedes Mal genau andersherum.';
            },
        },
    };

    // the fern of Logo: a stem that bends a little with every step, with a
    // fern of half the size to either side; the levels are its sizes
    const fern = (out, size, sign) => {
        if (size < 1) return;
        out.push(['forward', size], ['right', 70 * sign]);
        fern(out, size * 0.5, -sign);
        out.push(['left', 70 * sign], ['forward', size], ['left', 70 * sign]);
        fern(out, size * 0.5, sign);
        out.push(['right', 70 * sign], ['right', 7 * sign]);
        fern(out, size - 1, sign);
        out.push(['left', 7 * sign], ['back', size * 2]);
    };
    FIGURES.fern = {
        levels: 3,
        labels: ['Größe 3', 'Größe 6', 'Größe 12', 'Größe 25'],
        commands(level) {
            const size = [3, 6, 12, 25][level];
            const out = [['left', 90], ['width', 2], ['color', JOIN], ['forward', size], ['right', 70], ['color', PARTS[0]]];
            fern(out, size * 0.5, -1);
            out.push(['color', JOIN], ['left', 70], ['forward', size], ['left', 70], ['color', PARTS[1]]);
            fern(out, size * 0.5, 1);
            out.push(['right', 70], ['right', 7], ['color', PARTS[2]]);
            fern(out, size - 1, 1);
            return out;
        },
        says(level) {
            const size = [3, 6, 12, 25][level];
            return 'Ein Farnblatt der Größe ' + size + ': zwei Stücke Stiel (grau), rechts und links ein Farnblatt halber Größe (grün und blau), ein wenig gedreht, und dann der Rest, ein Farnblatt der Größe ' + (size - 1) + ' (orange). Kleiner als 1 zeichnet es nichts.';
        },
    };

    // a plant from a word (turtle-3): every F of the rule F[+F]F[-F]F is
    // a plant of the level before, each in its own colour; [ and ] are
    // push and pop
    const RULE = 'F[+F]F[-F]F';
    const RULE_PARTS = PARTS.concat(['#0c8599']);
    FIGURES.plant = {
        levels: 4,
        commands(level) {
            const out = [['left', 90], ['width', 2]];
            const size = 4 * Math.pow(3, 4 - level);
            const grow = (depth, colour) => {
                if (depth === 0) {
                    out.push(['color', colour], ['forward', size]);
                    return;
                }
                let part = 0;
                for (const letter of RULE) {
                    if (letter === 'F') grow(depth - 1, colour || RULE_PARTS[part++]);
                    else if (letter === '+') out.push(['left', 25]);
                    else if (letter === '-') out.push(['right', 25]);
                    else if (letter === '[') out.push(['push']);
                    else out.push(['pop']);
                }
            };
            grow(level, level === 0 ? JOIN : null);
            return out;
        },
        says(level) {
            const letters = (n) => (n === 0 ? 1 : 5 * letters(n - 1) + 6);
            if (level === 0) return 'Stufe 0: das Wort F, ein Strich.';
            return 'Stufe ' + level + ': ein Wort aus ' + letters(level) + ' Zeichen. Jedes F der Regel F[+F]F[-F]F ist eine Pflanze der Stufe ' + (level - 1) + ', in fünf Farben; [ merkt sich die Stelle, ] kehrt dorthin zurück.';
        },
    };

    // <div class='turtle-levels' data-figure='koch'></div>: the paper, a
    // button for each level and a sentence about it; a click lets the
    // turtle draw that level, its parts in colour.
    function levels(box) {
        const figure = FIGURES[box.dataset.figure];
        if (!figure) return;
        const stage = new Stage(box);
        stage.maxWidth = 3;
        const player = new Player(stage, () => {}, () => {});
        const says = el('p', { class: 'turtle-levels-says', 'aria-live': 'polite' });
        const buttons = el('div', { class: 'btn-group btn-group-sm', role: 'group', 'aria-label': figure.labels ? 'Größe' : 'Stufe' });
        const show = (level, animate) => {
            const commands = figure.commands(level);
            const before = stage.view;
            const previous = stage.segments;
            stage.target = null;
            stage.clear();
            // zoomed in on the small levels, out on the large ones, gliding
            // there with the level before fading behind it
            stage.fit(trace(commands), 1);
            if (animate) {
                const to = stage.view;
                stage.view = before;
                stage.ghost = previous;
                stage.ghostAlpha = 1;
                stage.glide(to);
            }
            stage.redraw();
            player.reset(animate ? 3 : 0);
            // every level in about three seconds, however many lines it has
            const total = commands.reduce((sum, [c, v]) => sum + duration(c, v, 3, stage.pixels()), 0);
            player.scale = Math.min(1, 3000 / Math.max(1, total));
            player.push(commands.map(([c, v]) => [c, v, null]));
            says.textContent = figure.says(level);
            Array.from(buttons.children).forEach((b, i) => {
                b.classList.toggle('active', i === level);
                b.setAttribute('aria-pressed', i === level ? 'true' : 'false');
            });
        };
        for (let level = 0; level <= figure.levels; level++) {
            const b = el('button', { type: 'button', class: 'btn btn-outline-secondary', text: figure.labels ? figure.labels[level] : 'Stufe ' + level });
            b.addEventListener('click', () => show(level, true));
            buttons.appendChild(b);
        }
        box.appendChild(el('div', { class: 'turtle-bar' }, [buttons]));
        box.appendChild(says);
        show(Math.min(2, figure.levels), false);
    }

    function startLevels() {
        document.querySelectorAll('.turtle-levels').forEach((box) => {
            if (box.dataset.ready) return;
            box.dataset.ready = '1';
            levels(box);
        });
    }
    if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', startLevels);
    else startLevels();

    // ----- colours to pick from, under the color chip of a card -----

    // Cling (the palette of Pixelflow and the 2D Game Studio), in its order,
    // and some of the HTML names, from red round to grey
    const CLING = ['#ffe617', '#fad31c', '#fdb717', '#faaa21', '#f1753f', '#ed5724', '#ef4538', '#ea2830', '#bc2326', '#8c0c03', '#e5185d',
        '#f384ae', '#fac6d2', '#b296c7', '#7b67ae', '#5f3577', '#c1d18a', '#799155', '#80bc42', '#4aa03f', '#16884a', '#003f2e', '#c3def1',
        '#55beed', '#31a8e0', '#238acc', '#0d60ae', '#143b86', '#001b4a', '#7dcdc2', '#00a8a8', '#12959f', '#094e54', '#381e11', '#c05c20',
        '#bf9b6b', '#e9d4a7', '#e7e6e1', '#cfd0d2', '#8a8b8f', '#778590', '#474d4d', '#050608', '#ffffff'];
    const NAMED = ['crimson', 'red', 'tomato', 'coral', 'salmon', 'deeppink', 'hotpink', 'pink', 'darkorange', 'orange', 'gold', 'yellow', 'khaki',
        'yellowgreen', 'limegreen', 'forestgreen', 'seagreen', 'olivedrab', 'darkgreen', 'turquoise', 'teal', 'skyblue', 'dodgerblue', 'steelblue',
        'royalblue', 'navy', 'orchid', 'violet', 'plum', 'purple', 'rebeccapurple', 'tan', 'peru', 'chocolate', 'sienna', 'saddlebrown',
        'white', 'silver', 'gray', 'dimgray', 'black'];

    // the colour into the card's program where the cursor was, or on a line
    // of its own at the end when the editor was never touched
    function insertColor(card, value, command) {
        const editor = card.querySelector('.task-code');
        if (!editor) return;
        const language = editor.dataset.codeLanguage;
        const text = language === 'ruby' ? command + ' "' + value + '"' : language === 'python' ? command + '("' + value + '")' : command + '("' + value + '");';
        const touched = editor.dataset.touched;
        editor.focus();
        if (touched) {
            editor.setRangeText(text, editor.selectionStart, editor.selectionEnd, 'end');
        } else {
            const end = editor.value.length;
            editor.setRangeText((end && !editor.value.endsWith('\n') ? '\n' : '') + text + '\n', end, end, 'end');
        }
        editor.dataset.touched = 'true';
        editor.dispatchEvent(new Event('input', { bubbles: true }));
        if (editor.codeInputUpdate) editor.codeInputUpdate();
    }

    let palette = null;

    function closePalette() {
        if (!palette) return;
        palette.opener.setAttribute('aria-expanded', 'false');
        palette.box.remove();
        document.removeEventListener('pointerdown', palette.outside, true);
        palette = null;
    }

    function openPalette(opener) {
        const card = opener.closest('.task-card');
        closePalette();
        const box = el('div', { class: 'turtle-palette', role: 'dialog', 'aria-label': 'Farben zum Aussuchen' });
        const value = el('code', { class: 'turtle-palette-value', text: ' ' });
        const group = (title, colours) => {
            box.appendChild(el('div', { class: 'turtle-palette-title', text: title }));
            const row = el('div', { class: 'turtle-palette-swatches' });
            colours.forEach((c) => {
                const b = el('button', { type: 'button', class: 'turtle-palette-swatch', style: '--swatch: ' + c, title: c, 'aria-label': c });
                const show = () => { value.textContent = c; };
                b.addEventListener('mouseenter', show);
                b.addEventListener('focus', show);
                b.addEventListener('click', () => {
                    closePalette();
                    insertColor(card, c, opener.dataset.command || 'color');
                });
                row.appendChild(b);
            });
            box.appendChild(row);
        };
        group('Cling', CLING);
        group('Namen', NAMED);
        box.appendChild(el('div', { class: 'turtle-palette-foot' }, [value, el('span', { text: 'Ein Klick setzt die Farbe in dein Programm.' })]));
        box.addEventListener('keydown', (e) => {
            if (e.key === 'Escape') {
                e.preventDefault();
                closePalette();
                opener.focus();
            }
        });
        opener.parentElement.appendChild(box);
        opener.setAttribute('aria-expanded', 'true');
        const outside = (e) => { if (!box.contains(e.target) && e.target !== opener && !opener.contains(e.target)) closePalette(); };
        document.addEventListener('pointerdown', outside, true);
        palette = { box, opener, outside };
        box.querySelector('button').focus({ preventScroll: true });
    }

    document.addEventListener('click', (e) => {
        const opener = e.target.closest('.turtle-palette-open');
        if (!opener) return;
        if (palette && palette.opener === opener) closePalette();
        else openPalette(opener);
    });
    // where the student last put the cursor counts from the first click on
    document.addEventListener('focusin', (e) => {
        if (e.target.matches && e.target.matches('.task-code')) e.target.dataset.touched = 'true';
    });

    window.TurtleCard = TurtleCard;
    window.TurtleCard.showOnly = showOnly;
})();
