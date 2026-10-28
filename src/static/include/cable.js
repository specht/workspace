// »Datenübertragung per Funk«: the radio with five lamps on /radio and /ether.
//
// Every element with data-cable becomes a device: a frequency (10.0 to 99.9
// kHz), up to five lamps and a button under each. A lamp is on while someone
// on the same frequency holds its button (or the keys 1 to 5); everyone on
// that frequency sees it. The server (src/ruby/include/cable_ether.rb) keeps
// one ether for the whole installation, without login. Teachers also get a
// panel: the number of lamps for everyone (with a countdown), the
// frequencies in use, the words to hand out (kept in this browser only) and
// the address with a QR code for the projector.
(function () {
    'use strict';

    const MIN_FREQ = 100;
    const MAX_FREQ = 999;
    const MAX_LAMPS = 5;
    const COLORS = ['#73d216', '#fad31c', '#e8352a', '#2d93e0', '#c53ee0'];
    const NAMES = ['grün', 'gelb', 'rot', 'blau', 'lila'];
    const SHORT_PATH = '/ether';

    function storageGet(key) {
        try { return window.localStorage.getItem(key); } catch (e) { return null; }
    }

    function storageSet(key, value) {
        try { window.localStorage.setItem(key, value); } catch (e) { /* private window: nothing kept */ }
    }

    function el(tag, attributes, children) {
        const node = document.createElement(tag);
        Object.entries(attributes || {}).forEach(([name, value]) => {
            if (name === 'text') node.textContent = value;
            else if (name === 'html') node.innerHTML = value;
            else node.setAttribute(name, value);
        });
        (children || []).forEach((child) => node.appendChild(child));
        return node;
    }

    function icon(name) {
        return el('i', { class: 'bi ' + name, 'aria-hidden': 'true' });
    }

    function formatFreq(f) {
        return (f / 10).toFixed(1);
    }

    function formatSeconds(s) {
        s = Math.max(0, Math.ceil(s));
        return Math.floor(s / 60) + ':' + String(s % 60).padStart(2, '0');
    }

    function lampsWord(n) {
        return n === 1 ? 'eine Lampe' : n + ' Lampen';
    }

    function randomInt(n) {
        const buffer = new Uint32Array(1);
        window.crypto.getRandomValues(buffer);
        return buffer[0] % n;
    }

    function shuffled(list) {
        const copy = list.slice();
        for (let i = copy.length - 1; i > 0; i--) {
            const j = randomInt(i + 1);
            [copy[i], copy[j]] = [copy[j], copy[i]];
        }
        return copy;
    }

    function setup(root) {
        let ws = null;
        let lamps = MAX_LAMPS;
        let lit = [];
        let pending = null;          // { n, at } with at in performance.now() ms
        let teacher = false;
        let words = null;
        let lastMessageAt = 0;
        let retryDelay = 1000;
        let retryTimer = null;
        let retryAt = 0;
        let full = false;
        let tuneTimer = null;
        let visible = true;
        let appliedNoticeTimer = null;
        const held = new Map();      // lamp -> Set of sources (pointer ids, keys)

        let freq = parseInt(storageGet('cable-freq'), 10);
        if (!(freq >= MIN_FREQ && freq <= MAX_FREQ)) freq = MIN_FREQ + randomInt(MAX_FREQ - MIN_FREQ + 1);

        // ----- the device -----
        const status = el('span', { class: 'cable-status', role: 'status', text: 'verbinde …' });
        const lcdValue = el('span', { text: formatFreq(freq) });
        const lcd = el('div', { class: 'cable-lcd', 'aria-live': 'polite' }, [lcdValue, el('span', { class: 'cable-unit', text: 'kHz' })]);
        const down = el('button', { type: 'button', class: 'cable-step', 'aria-label': 'Frequenz 0.1 kHz niedriger' }, [icon('bi-dash-lg')]);
        const up = el('button', { type: 'button', class: 'cable-step', 'aria-label': 'Frequenz 0.1 kHz höher' }, [icon('bi-plus-lg')]);
        const coarse = el('input', { type: 'range', min: '10', max: '99', step: '1', 'aria-label': 'Frequenz, ganze kHz' });
        const fine = el('input', { type: 'range', min: '0', max: '9', step: '1', 'aria-label': 'Frequenz, Zehntel kHz' });
        const channels = [];
        const channelBox = el('div', { class: 'cable-channels' });
        for (let i = 1; i <= MAX_LAMPS; i++) {
            const lamp = el('div', { class: 'cable-lamp', role: 'img', 'aria-label': 'Lampe ' + i + ' (' + NAMES[i - 1] + '): aus' });
            const key = el('button', { type: 'button', class: 'cable-key', 'aria-label': 'Lampe ' + i + ' halten', text: String(i) });
            const channel = el('div', { class: 'cable-channel', 'data-lamp': String(i) }, [lamp, key]);
            channels.push({ channel, lamp, key });
            channelBox.appendChild(channel);
        }
        const banner = el('div', { class: 'cable-banner', role: 'status' });
        banner.hidden = true;
        const notice = el('div', { class: 'cable-notice' });
        notice.hidden = true;
        const device = el('div', { class: 'cable-device' }, [
            el('div', { class: 'cable-top' }, [el('span', { text: 'Frequenz' }), status]),
            el('div', { class: 'cable-dial' }, [down, lcd, up]),
            el('div', { class: 'cable-sliders' }, [
                el('span', { text: 'kHz' }), coarse,
                el('span', { text: 'Zehntel' }), fine,
            ]),
            channelBox,
            banner,
            notice,
            el('div', { class: 'cable-keys-hint', text: 'Halten mit der Maus, dem Finger oder den Tasten 1 bis 5.' }),
        ]);
        root.appendChild(device);

        // ----- the teacher's panel (shown when the server says so) -----
        const panel = el('section', { class: 'cable-teacher', 'aria-label': 'Für Lehrkräfte' });
        panel.hidden = true;
        root.appendChild(panel);
        const projector = el('div', { class: 'cable-projector', role: 'dialog', 'aria-modal': 'true', 'aria-label': 'Adresse für den Beamer' });
        projector.hidden = true;
        document.body.appendChild(projector);

        // ----- frequency -----
        function showFreq() {
            lcdValue.textContent = formatFreq(freq);
            coarse.value = String(Math.floor(freq / 10));
            fine.value = String(freq % 10);
        }

        function setFreq(f) {
            f = Math.min(MAX_FREQ, Math.max(MIN_FREQ, f));
            if (f === freq) return;
            releaseAll();
            freq = f;
            lit = [];
            showLamps();
            showFreq();
            storageSet('cable-freq', String(freq));
            // Turning the dial sends a frequency every 80 ms at most.
            if (!tuneTimer) {
                tuneTimer = setTimeout(() => { tuneTimer = null; send({ t: 'tune', f: freq }); }, 80);
            }
        }

        coarse.addEventListener('input', () => setFreq(parseInt(coarse.value, 10) * 10 + freq % 10));
        fine.addEventListener('input', () => setFreq(Math.floor(freq / 10) * 10 + parseInt(fine.value, 10)));
        down.addEventListener('click', () => setFreq(freq - 1));
        up.addEventListener('click', () => setFreq(freq + 1));
        showFreq();

        // ----- lamps -----
        function showLamps() {
            channels.forEach(({ channel, lamp }, index) => {
                const i = index + 1;
                channel.hidden = i > lamps;
                const on = lit.includes(i);
                lamp.classList.toggle('on', on);
                lamp.setAttribute('aria-label', 'Lampe ' + i + ' (' + NAMES[index] + '): ' + (on ? 'an' : 'aus'));
                channel.classList.toggle('leaving', !!pending && pending.n < lamps && i > pending.n);
            });
        }

        function press(i, source) {
            if (i < 1 || i > lamps) return;
            let sources = held.get(i);
            if (!sources) held.set(i, sources = new Set());
            const first = sources.size === 0;
            sources.add(source);
            channels[i - 1].key.classList.add('pressed');
            if (first) send({ t: 'lamp', i: i, on: true });
        }

        function unpress(i, source) {
            const sources = held.get(i);
            if (!sources || !sources.delete(source)) return;
            if (sources.size > 0) return;
            held.delete(i);
            channels[i - 1].key.classList.remove('pressed');
            send({ t: 'lamp', i: i, on: false });
        }

        function releaseAll() {
            if (held.size === 0) return;
            held.clear();
            channels.forEach(({ key }) => key.classList.remove('pressed'));
            send({ t: 'release' });
        }

        channels.forEach(({ key }, index) => {
            const i = index + 1;
            key.addEventListener('pointerdown', (e) => {
                if (e.pointerType === 'mouse' && e.button !== 0) return;
                e.preventDefault();
                try { key.setPointerCapture(e.pointerId); } catch (error) { /* not capturable */ }
                press(i, 'p' + e.pointerId);
            });
            ['pointerup', 'pointercancel', 'lostpointercapture'].forEach((type) => {
                key.addEventListener(type, (e) => unpress(i, 'p' + e.pointerId));
            });
            key.addEventListener('contextmenu', (e) => e.preventDefault());
            // Space or Enter on a focused button holds it too.
            key.addEventListener('keydown', (e) => {
                if ((e.key === ' ' || e.key === 'Enter') && !e.repeat) { e.preventDefault(); press(i, 'space'); }
            });
            key.addEventListener('keyup', (e) => {
                if (e.key === ' ' || e.key === 'Enter') { e.preventDefault(); unpress(i, 'space'); }
            });
            key.addEventListener('blur', () => unpress(i, 'space'));
        });

        function typing(target) {
            if (!target || !target.closest) return false;
            if (target.isContentEditable) return true;
            const field = target.closest('input, textarea, select');
            return !!field && field.type !== 'range';
        }

        // The keys 1 to 5, while the device is on the screen (on the
        // activity page someone may be reading further down).
        document.addEventListener('keydown', (e) => {
            if (!/^[1-5]$/.test(e.key) || e.ctrlKey || e.metaKey || e.altKey || typing(e.target) || !visible) return;
            e.preventDefault();
            if (!e.repeat) press(parseInt(e.key, 10), 'key' + e.key);
        });
        document.addEventListener('keyup', (e) => {
            if (/^[1-5]$/.test(e.key)) unpress(parseInt(e.key, 10), 'key' + e.key);
        });
        // No lamp stays on when the window loses focus or the tab is hidden.
        window.addEventListener('blur', releaseAll);
        window.addEventListener('pagehide', releaseAll);
        document.addEventListener('visibilitychange', () => { if (document.hidden) releaseAll(); });

        if ('IntersectionObserver' in window) {
            new IntersectionObserver((entries) => {
                visible = entries.some((entry) => entry.isIntersecting);
                if (!visible) releaseAll();
            }).observe(device);
        }

        // ----- countdown -----
        function showBanner() {
            if (!pending) {
                if (!banner.classList.contains('done')) banner.hidden = true;
                return;
            }
            const left = (pending.at - performance.now()) / 1000;
            banner.classList.remove('done');
            banner.hidden = false;
            banner.replaceChildren(icon('bi-hourglass-split'), document.createTextNode(
                pending.n < lamps
                    ? 'Noch ' + formatSeconds(left) + ', dann gibt es nur noch ' + lampsWord(pending.n) + '.'
                    : 'Noch ' + formatSeconds(left) + ', dann gibt es ' + lampsWord(pending.n) + '.'));
        }

        function showApplied(n) {
            banner.classList.add('done');
            banner.hidden = false;
            banner.replaceChildren(icon('bi-lightbulb'), document.createTextNode('Jetzt gibt es ' + (n < MAX_LAMPS ? 'nur noch ' : '') + lampsWord(n) + '.'));
            clearTimeout(appliedNoticeTimer);
            appliedNoticeTimer = setTimeout(() => {
                banner.classList.remove('done');
                if (!pending) banner.hidden = true;
            }, 8000);
        }

        setInterval(() => {
            showBanner();
            if (teacher) showTeacherPending();
        }, 250);

        // ----- connection -----
        function setStatus(state, text) {
            status.className = 'cable-status ' + state;
            status.textContent = text;
        }

        function showNotice(text) {
            if (!text) { notice.hidden = true; return; }
            notice.hidden = false;
            notice.replaceChildren(icon('bi-plug'), document.createTextNode(text));
        }

        function send(message) {
            if (ws && ws.readyState === WebSocket.OPEN) ws.send(JSON.stringify(message));
        }

        function connect() {
            clearTimeout(retryTimer);
            setStatus('waiting', 'verbinde …');
            const protocol = location.protocol === 'https:' ? 'wss:' : 'ws:';
            const socket = new WebSocket(protocol + '//' + location.host + '/ws_cable');
            ws = socket;
            socket.addEventListener('open', () => {
                lastMessageAt = performance.now();
                send({ t: 'tune', f: freq });
            });
            socket.addEventListener('message', (event) => {
                lastMessageAt = performance.now();
                let message;
                try { message = JSON.parse(event.data); } catch (e) { return; }
                receive(message);
            });
            socket.addEventListener('close', () => {
                if (ws !== socket) return;
                ws = null;
                // The server let go of everything this page held.
                held.clear();
                channels.forEach(({ key }) => key.classList.remove('pressed'));
                lit = [];
                showLamps();
                const wait = full ? 30000 : retryDelay;
                retryDelay = Math.min(retryDelay * 2, 15000);
                retryAt = performance.now() + wait;
                setStatus('', 'getrennt');
                retryTimer = setTimeout(connect, wait);
                showRetry();
            });
        }

        function showRetry() {
            if (ws) return;
            const left = Math.max(0, Math.ceil((retryAt - performance.now()) / 1000));
            showNotice(full
                ? 'Gerade sind sehr viele verbunden. Neuer Versuch in ' + left + ' s.'
                : 'Die Verbindung ist unterbrochen. Neuer Versuch in ' + left + ' s …');
        }
        setInterval(showRetry, 1000);

        // Every 20 s a ping; without an answer for 45 s the connection is gone.
        setInterval(() => {
            if (!ws || ws.readyState !== WebSocket.OPEN) return;
            if (performance.now() - lastMessageAt > 45000) { ws.close(); return; }
            send({ t: 'ping' });
        }, 20000);

        function receive(message) {
            switch (message.t) {
            case 'hello':
                full = false;
                retryDelay = 1000;
                setStatus('online', 'verbunden');
                showNotice(null);
                lamps = message.lamps;
                pending = message.pending ? { n: message.pending.n, at: performance.now() + message.pending.in * 1000 } : null;
                teacher = message.teacher === true;
                if (teacher) {
                    words = message.words || [];
                    buildTeacherPanel();
                    send({ t: 'watch' });
                }
                showLamps();
                showBanner();
                break;
            case 'state':
                if (message.f !== freq) return;
                lit = message.on || [];
                showLamps();
                break;
            case 'lamps':
                lamps = message.n;
                for (const i of Array.from(held.keys())) {
                    if (i > lamps) { held.delete(i); channels[i - 1].key.classList.remove('pressed'); }
                }
                pending = null;
                showLamps();
                showApplied(lamps);
                break;
            case 'pending':
                pending = message.n ? { n: message.n, at: performance.now() + message.in * 1000 } : null;
                showLamps();
                showBanner();
                break;
            case 'overview':
                renderOverview(message);
                break;
            case 'error':
                if (message.reason === 'full') full = true;
                break;
            }
        }

        // ----- teacher -----
        let teacherBuilt = false;
        let delayInput, delayOutput, countButtons, pendingLine, pendingText, freqBox, totalLine, wordBox, wordFor, wordCount, givenBox;
        let lastOverview = null;
        const freqRows = new Map();  // frequency -> { row, people, dots, word }

        function buildTeacherPanel() {
            panel.hidden = false;
            if (teacherBuilt) return;
            teacherBuilt = true;

            delayInput = el('input', { type: 'range', min: '0', max: '300', step: '5', value: storageGet('cable-delay') || '180', 'aria-label': 'Wartezeit bis zur Änderung' });
            delayOutput = el('output', { text: '' });
            const showDelay = () => {
                const s = parseInt(delayInput.value, 10);
                delayOutput.textContent = s === 0 ? 'sofort' : 'in ' + formatSeconds(s);
            };
            delayInput.addEventListener('input', () => { showDelay(); storageSet('cable-delay', delayInput.value); });
            showDelay();
            countButtons = [];
            const counts = el('div', { class: 'cable-counts', role: 'group', 'aria-label': 'Anzahl der Lampen' });
            for (let n = 1; n <= MAX_LAMPS; n++) {
                const button = el('button', { type: 'button', class: 'btn btn-sm btn-outline-secondary', text: String(n), 'aria-label': lampsWord(n) });
                button.addEventListener('click', () => send({ t: 'lamps', n: n, delay: parseInt(delayInput.value, 10) }));
                counts.appendChild(button);
                countButtons.push(button);
            }
            pendingText = el('span');
            const cancel = el('button', { type: 'button', class: 'btn btn-sm btn-outline-danger', text: 'Abbrechen' });
            cancel.addEventListener('click', () => send({ t: 'cancel' }));
            pendingLine = el('div', { class: 'cable-pending' }, [pendingText, cancel]);
            pendingLine.hidden = true;

            freqBox = el('div', { class: 'cable-freqs' });
            totalLine = el('div', { class: 'cable-total' });

            wordBox = el('div', { class: 'cable-word', 'aria-live': 'polite', text: '–' });
            wordFor = el('div', { class: 'cable-word-for' });
            wordCount = el('span', { class: 'cable-word-count' });
            givenBox = el('div', { class: 'cable-given' });
            const next = el('button', { type: 'button', class: 'btn btn-sm btn-primary' }, [icon('bi-arrow-right-circle'), document.createTextNode('Nächstes Wort')]);
            const reshuffle = el('button', { type: 'button', class: 'btn btn-sm btn-outline-secondary' }, [icon('bi-shuffle'), document.createTextNode('Neu mischen')]);
            next.addEventListener('click', () => giveWord(null));
            reshuffle.addEventListener('click', () => {
                const go = () => {
                    newDeck();
                    storageSet('cable-given', '[]');
                    showWord();
                    if (lastOverview) renderOverview(lastOverview);
                };
                if (given().length === 0) go();
                else confirm_dialog({ title: 'Neu mischen?', text: 'Die Liste der vergebenen Wörter wird dabei geleert.', confirm: 'Neu mischen', icon: 'bi-shuffle' }, go);
            });

            const beamer = el('button', { type: 'button', class: 'btn btn-sm btn-outline-secondary' }, [icon('bi-qr-code'), document.createTextNode('Für den Beamer: Adresse und QR-Code')]);
            beamer.addEventListener('click', openProjector);

            panel.replaceChildren(
                el('div', { class: 'cable-teacher-head' }, [
                    el('span', {}, [icon('bi-person-video3'), document.createTextNode('Für Lehrkräfte')]),
                    beamer,
                ]),
                el('p', { class: 'cable-teacher-note', text: 'Die Zahl der Lampen gilt für alle, die das Funkgerät auf dieser Installation gerade benutzen, auch für andere Klassen. Ohne Anmeldung geht es unter ' + location.host + SHORT_PATH + '. Ein Wort für ein Paar gibt der Knopf in der Zeile seiner Frequenz: Sagen Sie es der Senderseite, und schreiben Sie es auf ihr Blatt.' }),
                el('div', { class: 'cable-teacher-grid' }, [
                    el('div', { class: 'cable-card' }, [
                        el('h3', { text: 'Lampen für alle' }),
                        el('div', { class: 'cable-delay' }, [el('label', { text: 'Wartezeit' }), delayOutput, delayInput]),
                        counts, pendingLine,
                    ]),
                    el('div', { class: 'cable-card cable-card-wide' }, [el('h3', { text: 'Frequenzen' }), totalLine, freqBox]),
                    el('div', { class: 'cable-card' }, [
                        el('h3', { text: 'Wörter' }), wordBox, wordFor,
                        el('div', { class: 'cable-word-bar' }, [wordCount, next, reshuffle]),
                        givenBox,
                    ]),
                ]),
            );
            showWord();
            showCounts();
        }

        function showCounts() {
            if (!countButtons) return;
            countButtons.forEach((button, index) => {
                const current = index + 1 === lamps;
                button.classList.toggle('current', current);
                button.classList.toggle('btn-secondary', current);
                button.classList.toggle('btn-outline-secondary', !current);
                button.setAttribute('aria-pressed', current ? 'true' : 'false');
            });
        }

        function showTeacherPending() {
            if (!pendingLine) return;
            showCounts();
            pendingLine.hidden = !pending;
            if (pending) pendingText.textContent = lampsWord(pending.n) + ' in ' + formatSeconds((pending.at - performance.now()) / 1000);
        }

        function dots(on, count) {
            const box = el('span', { class: 'cable-dots', 'aria-label': on.length ? 'an: ' + on.join(', ') : 'alle aus' });
            for (let i = 1; i <= count; i++) {
                const dot = el('span', { class: 'cable-dot' + (on.includes(i) ? ' on' : '') });
                dot.style.setProperty('--lamp', COLORS[i - 1]);
                box.appendChild(dot);
            }
            return box;
        }

        // The rows stay and change in place: an overview comes with every lamp
        // that goes on or off, and a rebuilt row would swallow a click.
        function renderOverview(message) {
            if (!freqBox) return;
            lastOverview = message;
            lamps = message.lamps;
            const people = message.freqs.reduce((sum, entry) => sum + entry.people, 0);
            totalLine.textContent = message.clients + (message.clients === 1 ? ' Gerät verbunden' : ' Geräte verbunden') +
                ', ' + people + ' auf ' + message.freqs.length + (message.freqs.length === 1 ? ' Frequenz' : ' Frequenzen');
            // Frequencies with a word stay in the list when nobody is on them.
            const entries = new Map(message.freqs.map((entry) => [entry.f, entry]));
            for (const g of given()) {
                if (g.f !== null && !entries.has(g.f)) entries.set(g.f, { f: g.f, people: 0, on: [] });
            }
            if (entries.size === 0) {
                freqRows.clear();
                freqBox.replaceChildren(el('div', { class: 'cable-empty', text: 'Noch niemand auf einer Frequenz.' }));
                return;
            }
            let table = freqBox.querySelector('table');
            if (!table) {
                table = el('table', {}, [
                    el('thead', {}, [el('tr', {}, [el('th', { text: 'kHz' }), el('th', { title: 'Personen', 'aria-label': 'Personen' }, [icon('bi-people')]), el('th', { text: 'Lampen' }), el('th', { text: 'Wort' })])]),
                    el('tbody'),
                ]);
                freqRows.clear();
                freqBox.replaceChildren(table);
            }
            const tbody = table.tBodies[0];
            for (const f of Array.from(freqRows.keys())) {
                if (!entries.has(f)) { freqRows.get(f).row.remove(); freqRows.delete(f); }
            }
            Array.from(entries.values()).sort((a, b) => a.f - b.f).forEach((entry, index) => {
                const r = freqRows.get(entry.f) || freqRow(entry.f);
                r.row.className = (entry.people > 2 ? 'crowded ' : '') + (entry.people === 0 ? 'idle ' : '') + (entry.f === freq ? 'mine' : '');
                r.people.textContent = String(entry.people);
                r.people.title = entry.people > 2 ? 'mehr als zwei auf dieser Frequenz' : '';
                r.dots.replaceChildren(dots(entry.on, message.lamps));
                const last = given().filter((g) => g.f === entry.f).pop();
                r.word.textContent = last ? last.word : '';
                if (tbody.children[index] !== r.row) tbody.insertBefore(r.row, tbody.children[index] || null);
            });
            showCounts();
        }

        function freqRow(f) {
            const give = el('button', {
                type: 'button', class: 'btn btn-sm btn-outline-primary cable-give',
                'aria-label': 'Ein Wort für ' + formatFreq(f) + ' kHz', title: 'Ein Wort für ' + formatFreq(f) + ' kHz',
            }, [icon('bi-chat-quote')]);
            give.addEventListener('click', () => giveWord(f));
            const r = { people: el('td'), dots: el('td'), word: el('span', { class: 'cable-row-word' }) };
            r.row = el('tr', {}, [el('td', { text: formatFreq(f) }), r.people, r.dots, el('td', {}, [el('div', { class: 'cable-word-cell' }, [r.word, give])])]);
            freqRows.set(f, r);
            return r;
        }

        // The words: shuffled in this browser, each once until all are used.
        function deck() {
            try {
                const saved = JSON.parse(storageGet('cable-words') || 'null');
                if (saved && Array.isArray(saved.order) && saved.order.length === words.length &&
                    saved.order.every((w) => words.includes(w))) return saved;
            } catch (e) { /* start over */ }
            return newDeck();
        }

        function newDeck() {
            const fresh = { order: shuffled(words), next: 0 };
            storageSet('cable-words', JSON.stringify(fresh));
            return fresh;
        }

        // The words given out, oldest first: { f (null without a frequency),
        // word, lamps, at }. Kept in this browser only, like the deck.
        function given() {
            try {
                const list = JSON.parse(storageGet('cable-given') || '[]');
                return Array.isArray(list) ? list : [];
            } catch (e) {
                return [];
            }
        }

        function giveWord(f) {
            const d = deck();
            if (d.next >= d.order.length) {
                wordBox.textContent = 'Alle verteilt';
                wordFor.textContent = 'Neu mischen fängt von vorn an.';
                return;
            }
            const word = d.order[d.next];
            d.next += 1;
            storageSet('cable-words', JSON.stringify(d));
            const list = given();
            list.push({ f: f, word: word, lamps: lamps, at: Date.now() });
            storageSet('cable-given', JSON.stringify(list.slice(-300)));
            showWord();
            if (lastOverview) renderOverview(lastOverview);
        }

        function showWord() {
            const d = deck();
            const list = given();
            const last = list[list.length - 1];
            wordBox.textContent = last ? last.word : '–';
            wordFor.textContent = last && last.f !== null ? 'für ' + formatFreq(last.f) + ' kHz' : '';
            wordCount.textContent = d.next + ' von ' + d.order.length;
            if (list.length === 0) {
                givenBox.replaceChildren();
                return;
            }
            const time = (at) => new Date(at).toLocaleTimeString('de-DE', { hour: '2-digit', minute: '2-digit' });
            const rows = list.slice().reverse().map((g) => el('tr', {}, [
                el('td', { text: time(g.at) }),
                el('td', { text: g.f === null ? '–' : formatFreq(g.f) }),
                el('td', { class: 'cable-given-word', text: g.word }),
                el('td', { text: String(g.lamps) }),
            ]));
            givenBox.replaceChildren(
                el('h4', { text: 'Vergeben' }),
                el('table', {}, [
                    el('thead', {}, [el('tr', {}, [el('th', { text: 'Zeit' }), el('th', { text: 'kHz' }), el('th', { text: 'Wort' }), el('th', { text: 'Lampen' })])]),
                    el('tbody', {}, rows),
                ]),
            );
        }

        function openProjector() {
            const close = el('button', { type: 'button', class: 'btn btn-light cable-close', 'aria-label': 'Schließen' }, [icon('bi-x-lg')]);
            close.addEventListener('click', closeProjector);
            projector.replaceChildren(
                close,
                el('p', { text: 'Öffnet auf einem Rechner oder Handy:' }),
                el('div', { class: 'cable-projector-address', text: location.host + SHORT_PATH }),
                el('img', { src: '/api/cable/qr.svg', alt: 'QR-Code mit der Adresse ' + location.host + SHORT_PATH }),
                el('p', { text: 'Stellt eure Frequenz ein. Eine Lampe leuchtet, solange ihr ihren Knopf haltet.' }),
            );
            projector.hidden = false;
            close.focus();
        }

        function closeProjector() {
            projector.hidden = true;
        }

        document.addEventListener('keydown', (e) => {
            if (e.key === 'Escape' && !projector.hidden) closeProjector();
        });

        showLamps();
        connect();
    }

    // The stylesheet, where the page has not linked it (the activity page).
    function addStylesheet() {
        if (document.querySelector("link[href*='/include/cable.css']")) return;
        document.head.appendChild(el('link', { rel: 'stylesheet', href: '/include/cable.css' }));
    }

    function start() {
        document.querySelectorAll('[data-cable]').forEach((root) => {
            if (root.dataset.cableReady) return;
            root.dataset.cableReady = '1';
            setup(root);
        });
    }

    addStylesheet();
    if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', start);
    else start();
})();
