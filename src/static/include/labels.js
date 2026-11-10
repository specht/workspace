// /etiketten: a label to draw on (label_printer.rb). The canvas has the
// printer's dots, 472 × 268, and stays black and white: after every stroke,
// picture and text the page turns each dot to black or white, so what one
// sees is what the printer prints. »Drucken« sends the dots as hexadecimal
// digits (ZPL's ^GFA data), and the page shows how far the label is.
(function () {
    'use strict';
    const W = 472, H = 268;
    const STAMPS = [
        ['emoji-smile', 0xf327], ['emoji-sunglasses', 0xf329], ['heart-fill', 0xf415], ['star-fill', 0xf586],
        ['rocket-takeoff', 0xf845], ['robot', 0xf6b1], ['bug', 0xf1dc], ['cpu', 0xf2d6], ['controller', 0xf2d4],
        ['code-slash', 0xf2c6], ['terminal', 0xf5c3], ['lightning-charge-fill', 0xf46c], ['sun', 0xf5a2],
        ['moon-stars', 0xf496], ['cloud-sun', 0xf2be], ['tree', 0xf5e2], ['flower1', 0xf3cd],
        ['music-note-beamed', 0xf49e], ['airplane', 0xf7cd], ['bicycle', 0xf18d], ['car-front', 0xf7e1],
        ['house', 0xf425], ['trophy', 0xf5e7], ['gem', 0xf3e6], ['puzzle', 0xf503], ['camera', 0xf220],
        ['telephone', 0xf5c1], ['cup-hot', 0xf7eb], ['balloon-heart', 0xf708], ['gift', 0xf3ec],
        ['snow2', 0xf56e], ['umbrella', 0xf5fd], ['globe2', 0xf3ef], ['award', 0xf154],
    ];
    const key = new URLSearchParams(location.search).get('k') || '';
    const $ = (s) => document.querySelector(s);
    const canvas = $('.lb-label canvas');
    const ctx = canvas.getContext('2d', { willReadFrequently: true });
    let tool = 'pen', width = 7, stampSize = 120, textSize = 48, stamp = STAMPS[0];
    let drawing = null;
    const undo = [];
    let browser = '';
    try {
        browser = localStorage.getItem('label-browser') || '';
        if (!browser) { browser = Math.random().toString(36).slice(2) + Date.now().toString(36); localStorage.setItem('label-browser', browser); }
    } catch (e) { browser = Math.random().toString(36).slice(2); }

    function post(url, data) {
        return fetch(url, { method: 'POST', headers: { 'Content-Type': 'application/json' }, body: JSON.stringify(data) })
            .then((r) => r.json().catch(() => ({})).then((body) => ({ status: r.status, body })));
    }

    function clear() {
        ctx.fillStyle = '#fff';
        ctx.fillRect(0, 0, W, H);
    }

    // every dot black or white, as the printer has it
    function threshold() {
        const img = ctx.getImageData(0, 0, W, H);
        const d = img.data;
        for (let i = 0; i < d.length; i += 4) {
            const v = (d[i] * 299 + d[i + 1] * 587 + d[i + 2] * 114) / 1000 < 128 ? 0 : 255;
            d[i] = d[i + 1] = d[i + 2] = v;
            d[i + 3] = 255;
        }
        ctx.putImageData(img, 0, 0);
    }

    function remember() {
        undo.push(ctx.getImageData(0, 0, W, H));
        if (undo.length > 30) undo.shift();
    }

    function bits() {
        const d = ctx.getImageData(0, 0, W, H).data;
        let hex = '';
        for (let y = 0; y < H; y++) {
            for (let bx = 0; bx < W / 8; bx++) {
                let byte = 0;
                for (let b = 0; b < 8; b++) {
                    const i = ((y * W) + bx * 8 + b) * 4;
                    if (d[i] < 128) byte |= 0x80 >> b;
                }
                hex += byte.toString(16).padStart(2, '0');
            }
        }
        return hex;
    }

    function point(e) {
        const r = canvas.getBoundingClientRect();
        return [(e.clientX - r.left) * W / r.width, (e.clientY - r.top) * H / r.height];
    }

    canvas.addEventListener('pointerdown', (e) => {
        e.preventDefault();
        const [x, y] = point(e);
        if (tool === 'stamp') {
            remember();
            ctx.fillStyle = '#000';
            ctx.font = `${stampSize}px bootstrap-icons`;
            ctx.textAlign = 'center';
            ctx.textBaseline = 'middle';
            ctx.fillText(String.fromCodePoint(stamp[1]), x, y);
            threshold();
            return;
        }
        if (tool === 'text') {
            const text = $('.lb-text').value.trim();
            if (!text) { $('.lb-text').focus(); return; }
            remember();
            ctx.fillStyle = '#000';
            ctx.font = `${textSize}px '${$('.lb-font').value}'`;
            ctx.textAlign = 'center';
            ctx.textBaseline = 'middle';
            ctx.fillText(text, x, y);
            threshold();
            return;
        }
        remember();
        canvas.setPointerCapture(e.pointerId);
        drawing = { x, y };
        ctx.fillStyle = tool === 'eraser' ? '#fff' : '#000';
        ctx.beginPath();
        ctx.arc(x, y, width / 2, 0, Math.PI * 2);
        ctx.fill();
    });
    canvas.addEventListener('pointermove', (e) => {
        if (!drawing) return;
        const [x, y] = point(e);
        ctx.strokeStyle = tool === 'eraser' ? '#fff' : '#000';
        ctx.lineWidth = tool === 'eraser' ? width * 2 : width;
        ctx.lineCap = 'round';
        ctx.lineJoin = 'round';
        ctx.beginPath();
        ctx.moveTo(drawing.x, drawing.y);
        ctx.lineTo(x, y);
        ctx.stroke();
        drawing = { x, y };
    });
    const end = () => { if (drawing) { drawing = null; threshold(); } };
    canvas.addEventListener('pointerup', end);
    canvas.addEventListener('pointercancel', end);

    function select(group, button) {
        group.querySelectorAll('button').forEach((b) => b.classList.toggle('active', b === button));
    }

    document.querySelector('.lb-tools').addEventListener('click', (e) => {
        const b = e.target.closest('button');
        if (!b) return;
        if (b.dataset.tool) {
            tool = b.dataset.tool;
            select(b.parentElement, b);
            document.querySelectorAll('.lb-opt').forEach((o) => { o.hidden = !o.dataset.for.split(' ').includes(tool); });
        } else if (b.classList.contains('lb-undo')) {
            if (undo.length) ctx.putImageData(undo.pop(), 0, 0);
        } else if (b.classList.contains('lb-clear')) {
            remember();
            clear();
        }
    });
    document.querySelector('.lb-options').addEventListener('click', (e) => {
        const b = e.target.closest('button');
        if (!b) return;
        if (b.dataset.width) { width = +b.dataset.width; select(b.parentElement, b); }
        if (b.dataset.stampSize) { stampSize = +b.dataset.stampSize; select(b.parentElement, b); }
        if (b.dataset.textSize) { textSize = +b.dataset.textSize; select(b.parentElement, b); }
        if (b.dataset.stamp) {
            stamp = STAMPS[+b.dataset.stamp];
            b.parentElement.querySelectorAll('button').forEach((x) => x.setAttribute('aria-pressed', x === b ? 'true' : 'false'));
        }
    });

    const stamps = document.querySelector('.lb-stamps');
    STAMPS.forEach(([name], i) => {
        const b = document.createElement('button');
        b.type = 'button';
        b.dataset.stamp = i;
        b.setAttribute('aria-pressed', i === 0 ? 'true' : 'false');
        b.setAttribute('aria-label', name.replace(/-fill$/, '').replace(/-/g, ' '));
        b.innerHTML = `<i class='bi bi-${name}'></i>`;
        stamps.appendChild(b);
    });

    // printing, then how far the label is
    const status = $('.lb-status');
    const send = $('.lb-send');
    function say(html) { status.innerHTML = html; }

    function follow(id) {
        post('/api/labels/status', { key, id }).then(({ status: code, body }) => {
            if (code !== 200) { say('Das Etikett ist nicht mehr in der Warteschlange.'); send.disabled = false; return; }
            if (body.state === 'printed') {
                say("<i class='bi bi-check-lg'></i> Gedruckt! Hol dir dein Etikett am Drucker.");
                send.disabled = false;
                return;
            }
            if (body.state === 'printing') say("<i class='bi bi-printer'></i> Wird gedruckt …");
            else if (!body.station) say('Dein Etikett wartet. Der Drucker ist gerade nicht verbunden; sag am Stand Bescheid.');
            else say(body.ahead > 0 ? `Dein Etikett wartet, ${body.ahead === 1 ? 'eins ist' : `${body.ahead} sind`} vor dir dran.` : 'Dein Etikett ist als Nächstes dran.');
            setTimeout(() => follow(id), 2000);
        });
    }

    send.addEventListener('click', () => {
        const hex = bits();
        if (!/[1-9a-f]/.test(hex)) { say('Das Etikett ist noch leer.'); return; }
        send.disabled = true;
        say('Wird geschickt …');
        post('/api/labels/print', { key, browser, bits: hex }).then(({ status: code, body }) => {
            if (code === 200) { follow(body.id); return; }
            send.disabled = false;
            if (body.error === 'wait') say(`Einen Moment noch: In ${body.seconds} Sekunden kannst du das nächste drucken.`);
            else if (body.error === 'busy') say('Gerade warten sehr viele Etiketten. Versuch es gleich noch einmal.');
            else if (body.error === 'labels_off' || body.error === 'wrong_key') say('Gerade druckt hier nichts mehr.');
            else say('Das hat nicht geklappt. Versuch es noch einmal.');
        }).catch(() => { send.disabled = false; say('Keine Verbindung. Versuch es noch einmal.'); });
    });

    clear();
    Promise.all([document.fonts.load('64px bootstrap-icons'), document.fonts.load("48px 'Patua One'"),
        document.fonts.load("48px 'Pixelify Sans'"), document.fonts.load("48px 'IBM Plex Sans'")]).catch(() => {});
    post('/api/labels/open', { key }).then(({ body }) => {
        if (body.on && body.key_ok) { $('.lb-app').hidden = false; return; }
        const closed = $('.lb-closed');
        closed.hidden = false;
        closed.textContent = body.on
            ? 'Etiketten druckt nur, wer den QR-Code am Stand scannt.'
            : 'Gerade druckt hier nichts. Am Tag der offenen Tür steht hier ein Etikettendrucker.';
    });
}());
