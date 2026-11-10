// /etiketten-station (label_printer_api.rb): switch the labels on for a few
// hours, show the QR code and the laptop's command, and the labels of the
// last two days, each drawn from its dots, with »nochmal« and »löschen«.
// Refreshes every five seconds.
(function () {
    'use strict';
    const W = 472, H = 268;
    const root = document.querySelector('.ls-root');
    const esc = (s) => String(s).replace(/[&<>'"]/g, (c) => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', "'": '&#39;', '"': '&quot;' }[c]));
    const time = (ts) => new Date(ts * 1000).toLocaleTimeString('de-DE', { hour: '2-digit', minute: '2-digit' });
    const day = (ts) => new Date(ts * 1000).toLocaleDateString('de-DE', { weekday: 'short', day: 'numeric', month: 'numeric' });
    const STATES = { queued: 'wartet', printing: 'wird gedruckt', printed: 'gedruckt' };
    let shown = '';

    function post(url, data) {
        return fetch(url, { method: 'POST', headers: { 'Content-Type': 'application/json' }, body: JSON.stringify(data || {}) }).then((r) => r.json());
    }

    function paint(canvas, hex) {
        const ctx = canvas.getContext('2d');
        const img = ctx.createImageData(W, H);
        for (let i = 0; i < W * H; i++) {
            const byte = parseInt(hex.substr((i >> 3) * 2, 2), 16);
            const v = byte & (0x80 >> (i & 7)) ? 0 : 255;
            img.data[i * 4] = img.data[i * 4 + 1] = img.data[i * 4 + 2] = v;
            img.data[i * 4 + 3] = 255;
        }
        ctx.putImageData(img, 0, 0);
    }

    function render(s) {
        const live = s.station_seen && s.now - s.station_seen < 15;
        let io = '';
        if (s.on) {
            const command = `ruby label-station.rb --server ${s.server} --token ${s.token}`;
            io += "<div class='ls-setup mb-4'>";
            io += `<div class='ls-qr'>${s.qr || ''}</div>`;
            io += '<div>';
            io += `<p class='mb-2'><span class='badge text-bg-success'>An</span> bis ${time(s.until)} Uhr. Der QR-Code führt zum Etikett; ohne ihn druckt niemand. <a href='${esc(s.url)}' target='_blank'>Seite öffnen</a></p>`;
            io += "<div class='d-flex flex-wrap gap-2 mb-3'><button class='btn btn-outline-danger btn-sm' data-hours='0'><i class='bi bi-power'></i>Ausschalten</button></div>";
            io += `<p class='mb-1'><span class='ls-state'><span class='ls-dot${live ? ' ls-live' : ''}'></span>${live ? 'Der Laptop holt die Etiketten ab.' : 'Der Laptop meldet sich gerade nicht.'}</span></p>`;
            io += "<p class='mb-1 mt-3'>Auf dem Laptop mit dem Drucker: <a href='/api/labels/label-station.rb'>label-station.rb</a> laden und im selben Ordner starten:</p>";
            io += `<pre class='ls-command mb-1'>${esc(command)}</pre>`;
            io += "<p class='text-body-secondary small'>Unter Linux schreibt es nach <code>/dev/usb/lp0</code>; wird das verweigert, braucht der Benutzer die Gruppe <code>lp</code>, oder du startest mit <code>sudo</code>. Ein Drucker in CUPS: <code>--lp NAME</code>. Zum Ausprobieren ohne Drucker: <code>--dry-run etiketten</code>. Jedes Einschalten macht einen neuen QR-Code und einen neuen Befehl.</p>";
            io += '</div></div>';
        } else {
            io += "<p><span class='badge text-bg-secondary'>Aus</span> Gerade druckt niemand. Einschalten für:</p><div class='d-flex flex-wrap gap-2 mb-4'>";
            s.hours.forEach((h) => { io += `<button class='btn btn-outline-primary' data-hours='${h}'><i class='bi bi-printer'></i>${h} Stunden</button>`; });
            io += '</div>';
        }
        io += `<h2 class='h4 mt-4'>Etiketten</h2>`;
        if (!s.jobs.length) io += "<p class='text-body-secondary'>Noch keine. Etiketten bleiben zwei Tage, ohne Namen.</p>";
        io += "<div class='ls-jobs'>";
        s.jobs.forEach((j) => {
            io += `<div class='ls-job' data-id='${esc(j.id)}'><canvas width='${W}' height='${H}' aria-label='Etikett von ${time(j.ts)} Uhr'></canvas>` +
                `<div class='ls-job-meta'><span class='ls-when'>${day(j.ts)} ${time(j.ts)}, ${STATES[j.state] || esc(j.state)}</span>` +
                `<button class='btn btn-sm btn-outline-secondary' data-again='${esc(j.id)}' title='Nochmal drucken' aria-label='Nochmal drucken'><i class='bi bi-arrow-repeat'></i></button>` +
                `<button class='btn btn-sm btn-outline-danger' data-delete='${esc(j.id)}' title='Löschen' aria-label='Löschen'><i class='bi bi-trash'></i></button></div></div>`;
        });
        io += '</div>';
        root.innerHTML = io;
        s.jobs.forEach((j) => paint(root.querySelector(`.ls-job[data-id='${CSS.escape(j.id)}'] canvas`), j.bits));
    }

    function load() {
        post('/api/labels/state').then((s) => {
            // redraw only when something changed, so that buttons stay put
            const key = JSON.stringify([s.on, s.until, s.token, s.station_seen && s.now - s.station_seen < 15, s.jobs.map((j) => [j.id, j.state])]);
            if (key !== shown) { shown = key; render(s); }
        });
    }

    root.addEventListener('click', (e) => {
        const b = e.target.closest('button');
        if (!b) return;
        if (b.dataset.hours !== undefined) post('/api/labels/switch', { hours: +b.dataset.hours }).then(load);
        if (b.dataset.delete) post('/api/labels/delete', { id: b.dataset.delete }).then(load);
        if (b.dataset.again) post('/api/labels/again', { id: b.dataset.again }).then(load);
    });

    load();
    setInterval(load, 5000);
}());
