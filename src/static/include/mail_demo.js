// Phishing (fair-web): a made-up mailbox, three real mails and two phishing
// mails, to hunt for clues in. Every address ends in .example, a top-level
// domain that belongs to no one; every service and every person is made up.
//
//   <script src='/include/mail_demo.js'></script>
//   <div class='mail-demo'></div>
//
// A click on a mail in the list opens it. Every sentence can be clicked, not
// only the suspicious ones (else the clickable places would give the answer
// away): a clue explains itself, anything else says so. Links show their
// real target in a status bar under the mail when the pointer or the
// keyboard is on them, as in a mail program; a click on one counts as found.
// Under the mail: »Phishing« or »Echt«, then how many clues were found and a
// button that shows the rest.
//
// The sentences are spans with role=button, not buttons: a button does not
// wrap across lines, and a sentence has to flow like text. They take the
// focus (Tab) and answer Enter and Space as a button does.
(function () {
    'use strict';

    const el = (tag, attrs = {}, children = []) => {
        const node = document.createElement(tag);
        for (const [k, v] of Object.entries(attrs)) {
            if (k === 'class') node.className = v;
            else if (k === 'text') node.textContent = v;
            else node.setAttribute(k, v);
        }
        for (const c of [].concat(children)) node.append(c);
        return node;
    };
    const icon = (name) => el('i', { class: `bi bi-${name}`, 'aria-hidden': 'true' });

    // a mail's body: paragraphs of parts; a part is a sentence (a string),
    // a sentence that is a clue ({ text, clue }), or a link ({ link, href,
    // clue }); a string ending in a space leads into the link after it, '\n'
    // breaks the line
    const SCHOOL = 'schule-am-see.example';
    const MAILS = [
        {
            id: 'ag', name: 'Hacker-AG', address: `ag@${SCHOOL}`, subject: 'Dienstag: Roboter fertig bauen', time: 'Mo 08:12', phishing: false,
            body: [
                ['Hallo zusammen,'],
                ['am Dienstag bauen wir die Roboter fertig.', 'Bringt bitte eure Akkus mit!'],
                ['Die Termine bis zu den Ferien stehen wie immer auf ', { link: `${SCHOOL}/ag`, href: `https://${SCHOOL}/ag` }, '.'],
                ['Bis Dienstag!'],
            ],
        },
        {
            id: 'portal', name: 'Schulportal', address: 'support@schuIe-am-see.example', subject: 'Dringend: Ihr Postfach wird gelöscht', time: 'Mo 09:47', phishing: true,
            senderClue: 'sender',
            body: [
                [{ text: 'Sehr geehrter Nutzer,', clue: 'greeting' }],
                ['Ihr Postfach hat die maximale Größe erreicht.', { text: 'Es wird in 24 Stunden unwiderruflich gelöscht.', clue: 'haste' }],
                [{ text: 'Bestätigen Sie jetzt Ihr Passwort, damit Ihre Daten erhalten bleiben:', clue: 'password' }],
                [{ link: `${SCHOOL}/login`, href: `https://${SCHOOL}.konto-pruefung.example/login`, clue: 'link' }],
                ['Mit freundlichen Grüßen', '\n', 'Ihr IT-Team'],
            ],
        },
        {
            id: 'mensa', name: 'Mensa', address: `mensa@${SCHOOL}`, subject: 'Speiseplan für die Woche', time: 'Mo 11:03', phishing: false,
            body: [
                ['Liebe Schülerinnen und Schüler,'],
                ['der Speiseplan für diese Woche hängt aus.', 'Am Donnerstag gibt es Pizza, am Freitag bleibt die Mensa wegen des Wandertags zu.'],
                ['Euer Mensa-Team'],
            ],
        },
        {
            id: 'paket', name: 'Paketdienst', address: 'info@zustell-service.example', subject: 'Ihr Paket konnte nicht zugestellt werden', time: 'Mo 13:28', phishing: true,
            attachment: { name: 'Zustellung.pdf.exe', clue: 'attachment' },
            body: [
                [{ text: 'Guten Tag,', clue: 'nameless' }],
                [{ text: 'Ihr Paket konnte heute nicht zugestellt werden.', clue: 'unexpected' }],
                [{ text: 'Zahlen Sie die Zustellgebühr von 1,99 € innerhalb von 48 Stunden, sonst geht das Paket zurück.', clue: 'money' }],
                ['Die Einzelheiten stehen im Anhang.'],
                ['Ihre Sendung können Sie hier verfolgen: ', { link: 'Sendung verfolgen', href: 'http://zustell-service.example.paket-track.example/de', clue: 'link' }],
            ],
        },
        {
            id: 'klasse', name: 'Klassenleitung 9b', address: `klasse9b@${SCHOOL}`, subject: 'Wandertag am Freitag', time: 'Mo 16:40', phishing: false,
            body: [
                ['Liebe 9b,'],
                ['am Freitag treffen wir uns um 8 Uhr am Bahnhof.', 'Denkt an feste Schuhe und etwas zu trinken.'],
                ['Die Einverständniserklärung bringt ihr bitte bis Mittwoch unterschrieben mit, auf Papier.'],
            ],
        },
    ];
    const CLUES = {
        sender: 'Der Absender sieht aus wie die Schule, schule-am-see.example. Aber dort steht ein großes I statt eines kleinen l: schuIe. Diese Domain kann jeder kaufen und von ihr Mails schicken.',
        greeting: '»Sehr geehrter Nutzer«: Die Schule kennt deinen Namen. Wer ihn nicht kennt, schreibt an sehr viele auf einmal.',
        haste: 'Zeitdruck: In 24 Stunden soll alles weg sein. Wer Angst hat, etwas zu verlieren, denkt weniger nach, und genau darauf zählt die Mail.',
        password: 'Keine Schule und kein seriöser Dienst fragt per Mail nach deinem Passwort. Du gibst es nur auf der echten Seite ein, die du selbst aufrufst.',
        link: 'Der Text des Links und sein Ziel sind verschieden. Unten in der Leiste steht, wohin er wirklich führt. Entscheidend ist das Ende des Rechnernamens, direkt vor dem ersten einzelnen Schrägstrich, und das gehört hier jemand anderem; die echte Adresse steht vorne nur zur Tarnung.',
        nameless: '»Guten Tag« ohne deinen Namen: Auf einem Paket steht, an wen es geht. Wer es dir wirklich schicken will, kennt deinen Namen.',
        unexpected: 'Hast du ein Paket bestellt? Eine Mail über etwas, das du nicht erwartest, ist schon ein Grund, genau hinzusehen.',
        money: 'Ein kleiner Betrag soll harmlos wirken. Wer ihn bezahlt, gibt seine Bankdaten oder seine Karte ein, und damit geht es um viel mehr als 1,99 €.',
        attachment: 'Die Datei endet auf .exe: Das ist ein Programm, kein PDF. Windows blendet bekannte Endungen oft aus; dann sieht man nur »Zustellung.pdf«. Ein Doppelklick startet das Programm.',
    };
    const CALM = 'Hier ist nichts Verdächtiges.';
    const STATUS_IDLE = 'Zeig auf einen Link, um zu sehen, wohin er führt.';

    // in the order they stand in the mail: sender, text, attachment
    const cluesOf = (mail) => [mail.senderClue, ...mail.body.flat().map((p) => typeof p === 'object' && p.clue),
        mail.attachment && mail.attachment.clue].filter(Boolean);

    function setUp(root) {
        root.dataset.chip = 'Zum Ausprobieren: Hinweise finden';
        const state = {};                           // mail id → { found: Set, verdict }
        const list = el('ul', { class: 'mail-list' });
        const pane = el('div', { class: 'mail-pane' });
        root.append(el('div', { class: 'mail-app' }, [list, pane]));
        const items = {};

        function open(mail) {
            const s = state[mail.id] || (state[mail.id] = { found: new Set(), verdict: null });
            for (const [id, b] of Object.entries(items)) b.setAttribute('aria-current', String(id === mail.id));
            const sentences = {};                   // clue → the spans that show it
            const notes = el('ul', { class: 'mail-notes', 'aria-live': 'polite' });
            const status = el('div', { class: 'mail-status' }, [STATUS_IDLE]);
            const verdict = el('div', { class: 'mail-verdict', 'aria-live': 'polite' });

            // a found clue: its place in the mail and its note carry the same
            // number, and pointing at one lights up the other
            const numberOf = (clue) => [...s.found].indexOf(clue) + 1;
            const pair = (clue, on) => {
                (sentences[clue] || []).forEach((n) => n.classList.toggle('mail-paired', on));
                notes.querySelectorAll(`[data-clue="${clue}"]`).forEach((n) => n.classList.toggle('mail-paired', on));
            };
            const mark = (clue) => (sentences[clue] || []).forEach((n) => {
                n.classList.add('mail-found');
                n.dataset.n = numberOf(clue);
                n.addEventListener('mouseenter', () => pair(clue, true));
                n.addEventListener('mouseleave', () => pair(clue, false));
            });
            const note = (clue, text) => {
                const li = el('li', { class: 'mail-note-clue', 'data-clue': clue }, [
                    el('span', { class: 'mail-num', text: String(numberOf(clue)), 'aria-hidden': 'true' }),
                    el('span', { text: text || CLUES[clue] })]);
                li.addEventListener('mouseenter', () => pair(clue, true));
                li.addEventListener('mouseleave', () => pair(clue, false));
                notes.append(li);
            };
            const addClue = (clue, text) => {
                calmLine.replaceChildren();
                if (s.found.has(clue)) return;
                s.found.add(clue);
                mark(clue);
                note(clue, text);
                showVerdict();
            };
            // what is not a clue: one line above the clues, replaced each time
            const calmLine = el('div', { class: 'mail-calm', 'aria-live': 'polite' });
            const calm = (text) => calmLine.replaceChildren(icon('dash-circle'), el('span', { text }));
            const sentence = (text, clue) => {
                const span = el('span', { class: 'mail-sentence', role: 'button', tabindex: '0', text });
                if (clue) (sentences[clue] = sentences[clue] || []).push(span);
                const act = () => (clue ? addClue(clue) : calm(CALM));
                span.addEventListener('click', act);
                span.addEventListener('keydown', (e) => {
                    if (e.key === 'Enter' || e.key === ' ') { e.preventDefault(); act(); }
                });
                return span;
            };
            const link = (part) => {
                const a = el('a', { href: '#', class: 'mail-link', text: part.link });
                if (part.clue) (sentences[part.clue] = sentences[part.clue] || []).push(a);
                const show = () => status.replaceChildren(icon('link-45deg'), el('span', { class: 'mail-url', text: part.href }));
                const hide = () => status.replaceChildren(STATUS_IDLE);
                a.addEventListener('mouseenter', show);
                a.addEventListener('focus', show);
                a.addEventListener('mouseleave', hide);
                a.addEventListener('blur', hide);
                a.addEventListener('click', (e) => {
                    e.preventDefault();
                    if (part.clue) addClue(part.clue, `Gut, dass das nur eine Übung ist: Der Link führt zu ${part.href}. ${CLUES[part.clue]}`);
                    else calm(`Der Link führt zu ${part.href}, also dorthin, wo sein Text sagt.`);
                });
                return a;
            };
            const paragraph = (parts) => el('p', {}, parts.flatMap((part, i) => {
                if (part === '\n') return [el('br')];
                const node = typeof part === 'string' ? (part === '.' ? part : sentence(part.trimEnd()))
                    : part.link ? link(part) : sentence(part.text, part.clue);
                const gap = i > 0 && part !== '.' && parts[i - 1] !== '\n' ? [' '] : [];
                return [...gap, node];
            }));

            function showVerdict() {
                if (s.verdict === null) return;
                const right = s.verdict === mail.phishing;
                const all = cluesOf(mail);
                const rest = all.filter((c) => !s.found.has(c));
                const line = mail.phishing
                    ? `Hinweise gefunden: ${s.found.size} von ${all.length}.`
                    : 'In dieser Mail steht nichts, was auf Phishing hindeutet.';
                const show = rest.length && mail.phishing
                    ? Object.assign(el('button', { type: 'button', class: 'btn btn-sm btn-outline-secondary' }, [icon('eye'), ' Übrige Hinweise zeigen']),
                        { onclick: () => rest.forEach((c) => addClue(c)) })
                    : '';
                verdict.replaceChildren(
                    el('div', { class: `dp-result dp-result-${right ? 'ok' : 'bad'}` }, [icon(right ? 'check-circle-fill' : 'x-circle-fill'),
                        el('span', {}, [el('strong', { text: right ? (mail.phishing ? 'Richtig: Das ist Phishing.' : 'Richtig: Die Mail ist echt.')
                            : (mail.phishing ? 'Vorsicht: Das ist Phishing.' : 'Diese Mail ist echt.') }), ' ', line])]),
                    show);
                const from = items[mail.id].querySelector('.mail-item-from');
                from.querySelectorAll('.mail-mark').forEach((n) => n.remove());
                from.append(el('i', { class: `mail-mark bi bi-${right ? 'check-circle-fill' : 'x-circle-fill'}`, 'aria-label': right ? 'richtig erkannt' : 'falsch eingeschätzt' }));
            }
            const decide = (phishing) => { s.verdict = phishing; showVerdict(); };

            const head = el('div', { class: 'mail-head' }, [
                el('div', { class: 'mail-subject', text: mail.subject }),
                el('div', { class: 'mail-from' }, [el('span', { class: 'mail-label', text: 'Von:' }), ' ', el('strong', { text: mail.name }), ' ',
                    sentence(`<${mail.address}>`, mail.senderClue)]),
                el('div', { class: 'mail-from' }, [el('span', { class: 'mail-label', text: 'An:' }), ' du@schule-am-see.example']),
            ]);
            const attachment = mail.attachment
                ? el('div', { class: 'mail-attachment' }, [icon('paperclip'), ' ', sentence(mail.attachment.name, mail.attachment.clue)]) : '';
            const button = (phishing) => Object.assign(el('button', { type: 'button', class: `btn btn-sm ${phishing ? 'btn-outline-danger' : 'btn-outline-success'}` },
                [icon(phishing ? 'shield-exclamation' : 'envelope-check'), phishing ? ' Phishing' : ' Echt']), { onclick: () => decide(phishing) });

            pane.replaceChildren(head, el('div', { class: 'mail-body' }, mail.body.map(paragraph)), attachment, status,
                el('div', { class: 'mail-decide' }, [el('span', { text: 'Diese Mail ist …' }), button(true), button(false)]),
                verdict, calmLine, notes);
            for (const clue of s.found) {
                mark(clue);
                note(clue);
            }
            showVerdict();
        }

        for (const mail of MAILS) {
            const b = el('button', { type: 'button', class: 'mail-item' }, [
                el('span', { class: 'mail-item-from', text: mail.name }),
                el('span', { class: 'mail-item-time', text: mail.time }),
                el('span', { class: 'mail-item-subject', text: mail.subject })]);
            b.addEventListener('click', () => open(mail));
            items[mail.id] = b;
            list.append(el('li', {}, [b]));
        }
        open(MAILS[0]);
    }

    const start = () => document.querySelectorAll('.mail-demo').forEach((root) => {
        if (root.dataset.ready) return;
        root.dataset.ready = '1';
        setUp(root);
    });
    if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', start);
    else start();
})();
