// The Telefonbuch (telefonbuch.html, phone_games_api.rb#phone_book_page):
// the published Telefonspiele in window.PHONE_BOOK, sorted by title, as the
// pages of a book. A click on the right page turns forward, on the left
// page back (also the arrow keys and a swipe); on a narrow screen one page
// at a time, its left half turning back and its right half on. An entry
// cannot be clicked to start a game: one types its number on the phone. A
// page holds as many entries as fit (measured, so
// long descriptions take more room). The notes stand in a colophon, on the
// right page of the spread after the last entries (a blank page beside it),
// so there is always a page to turn to; on a narrow screen on the next page.
// A number in the address (/telefonbuch?nummer=123: a game in the
// Workspace forwarded its call there): the phone has it dialled already.
(function () {
    const number = new URLSearchParams(location.search).get('nummer') || '';
    const phone = document.querySelector('iframe.phone-book-line');
    if (phone && /^\d{3,4}$/.test(number)) {
        phone.src = '/telefonbuch/telefon/?line&dial=' + number;
        phone.scrollIntoView({ block: 'center' });
    }
})();

(function () {
    const book = document.getElementById('phone-book');
    if (!book) return;
    const entries = window.PHONE_BOOK || [];
    const notes = window.PHONE_BOOK_NOTES;
    let pages = [{ kind: 'entries', list: [] }];     // and { kind: 'blank' }, { kind: 'colophon' }
    let at = 0;                       // the first page shown (even on a spread)
    let turning = false;

    const narrow = () => window.matchMedia('(max-width: 640px)').matches;
    const step = () => narrow() ? 1 : 2;
    const esc = (t) => String(t == null ? '' : t).replace(/[&<>"']/g, (c) => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' }[c]));
    const letter = (title) => {
        const t = String(title || '').replace(/^(der|die|das|ein|eine)\s+/i, '').trim();
        const c = t.charAt(0).toUpperCase();
        return ({ 'Ä': 'A', 'Ö': 'O', 'Ü': 'U' })[c] || c;
    };

    const entryHtml = (e) =>
        `<div class='phone-book-entry'><div class='pb-line'><span class='pb-title'>${esc(e.title)}</span><span class='pb-dots'></span>` +
        `<span class='pb-number'>${e.word ? `${esc(e.word)} <small>${esc(e.number)}</small>` : esc(e.number)}</span></div>` +
        (e.description ? `<div class='pb-description'>${esc(e.description)}</div>` : '') + '</div>';
    const colophonHtml = () =>
        `<div class='pb-remark'><div class='pb-remark-head'>Telefonbuch des Hackschule Workspace</div>` +
        (notes.line ? `<p>${esc(notes.line)}</p>` : '') +
        `<p>${esc(notes.own)} <a href='${esc(notes.href)}'>${esc(notes.link)}</a></p></div>`;
    const HEAD = 'Hackschule Workspace';

    function pageHtml(index) {
        const page = pages[index];
        if (!page || page.kind === 'blank') return '';
        if (page.kind === 'colophon') return colophonHtml();
        const list = page.list;
        const first = list.length ? letter(list[0].title) : '', last = list.length ? letter(list[list.length - 1].title) : '';
        const range = first === last ? first : `${first} – ${last}`;
        const body = list.length ? list.map(entryHtml).join('')
            : "<p class='phone-book-none'>Noch steht kein Spiel im Telefonbuch.</p>";
        return `<div class='phone-book-head'><span>${HEAD}</span><span>${esc(range)}</span></div>` +
            `<div class='pb-body'>${body}</div>`;
    }

    // the pages, by filling a hidden page of the same size until it overflows
    function paginate() {
        const sample = book.querySelector('.phone-book-leaf');
        if (!sample) return;
        const probe = document.createElement('div');
        probe.className = 'phone-book-leaf right';
        probe.setAttribute('aria-hidden', 'true');
        probe.style.cssText = `position: absolute; left: 0; top: 0; visibility: hidden; pointer-events: none; width: ${sample.offsetWidth}px`;
        probe.innerHTML = `<div class='phone-book-head'><span>${HEAD}</span><span>A</span></div><div class='pb-body'></div>`;
        book.appendChild(probe);
        const body = probe.querySelector('.pb-body');
        const full = () => body.scrollHeight > body.clientHeight + 1;
        const lists = [[]];
        for (const e of entries) {
            body.insertAdjacentHTML('beforeend', entryHtml(e));
            if (full() && lists[lists.length - 1].length) {
                lists.push([]);
                body.innerHTML = entryHtml(e);
            }
            lists[lists.length - 1].push(e);
        }
        probe.remove();
        pages = lists.map((list) => ({ kind: 'entries', list }));
        if (notes) {
            // wide: the right page of the next spread; narrow: the next page
            const colophon = narrow() ? pages.length : 2 * (Math.floor((pages.length - 1) / 2) + 1) + 1;
            while (pages.length < colophon) pages.push({ kind: 'blank' });
            pages.push({ kind: 'colophon' });
        }
        at = Math.min(at, pages.length - 1);
    }

    function layout() {
        render();
        paginate();
        render();
    }

    function leaf(side, index) {
        const empty = !pages[index];
        return `<div class='phone-book-leaf ${side}${empty ? ' empty' : ''}' data-side='${side}'>${empty ? '' : pageHtml(index)}</div>`;
    }

    function render() {
        if (!narrow()) at -= at % 2;
        book.innerHTML = `<div class='phone-book-spread'>${narrow() ? leaf('right', at) : leaf('left', at) + leaf('right', at + 1)}</div>`;
    }

    // a page turns around the spine: its front is the page it was, its back
    // the page it becomes
    function turn(direction) {
        const target = at + direction * step();
        if (turning || target < 0 || target >= pages.length) return;
        if (narrow() || window.matchMedia('(prefers-reduced-motion: reduce)').matches) {
            at = target;
            render();
            return;
        }
        turning = true;
        const spread = book.querySelector('.phone-book-spread');
        const sheet = document.createElement('div');
        sheet.className = `phone-book-turning ${direction > 0 ? 'forward' : 'backward'}`;
        if (direction > 0) {
            sheet.innerHTML = `<div class='front'>${leaf('right', at + 1)}</div><div class='back'>${leaf('left', target)}</div>`;
            spread.lastElementChild.outerHTML = leaf('right', target + 1);
        } else {
            sheet.innerHTML = `<div class='front'>${leaf('left', at)}</div><div class='back'>${leaf('right', target + 1)}</div>`;
            spread.firstElementChild.outerHTML = leaf('left', target);
        }
        sheet.querySelectorAll('.phone-book-leaf').forEach((l) => { l.style.height = '100%'; });
        book.appendChild(sheet);
        requestAnimationFrame(() => requestAnimationFrame(() => {
            sheet.style.transform = `rotateY(${direction > 0 ? -180 : 180}deg)`;
        }));
        sheet.addEventListener('transitionend', () => {
            at = target;
            turning = false;
            render();
        }, { once: true });
    }

    book.addEventListener('click', (e) => {
        if (e.target.closest('a')) return;
        if (swiped) { swiped = false; return; }
        const page = e.target.closest('.phone-book-leaf');
        if (!page || page.classList.contains('empty')) return;
        // one page at a time: its left half turns back, its right half on
        if (narrow()) {
            const box = page.getBoundingClientRect();
            turn(e.clientX < box.left + box.width / 2 ? -1 : 1);
            return;
        }
        turn(page.dataset.side === 'left' ? -1 : 1);
    });
    // a swipe turns the page too: to the left on, to the right back
    let startX = null, startY = null, swiped = false;
    book.addEventListener('touchstart', (e) => {
        startX = e.touches[0].clientX;
        startY = e.touches[0].clientY;
    }, { passive: true });
    book.addEventListener('touchend', (e) => {
        if (startX === null) return;
        const dx = e.changedTouches[0].clientX - startX, dy = e.changedTouches[0].clientY - startY;
        startX = null;
        if (Math.abs(dx) > 40 && Math.abs(dx) > 1.5 * Math.abs(dy)) {
            swiped = true;
            setTimeout(() => { swiped = false; }, 400);
            turn(dx < 0 ? 1 : -1);
        }
    }, { passive: true });
    document.addEventListener('keydown', (e) => {
        if (e.target.closest('input, textarea, select')) return;
        if (e.key === 'ArrowLeft') turn(-1);
        if (e.key === 'ArrowRight') turn(1);
    });
    let resized = null;
    window.addEventListener('resize', () => {
        clearTimeout(resized);
        resized = setTimeout(() => { if (!turning) layout(); }, 150);
    });
    layout();
    if (document.fonts) document.fonts.ready.then(() => { if (!turning) layout(); });
})();
