// The Telefonbuch (telefonbuch.html, phone_games_api.rb#phone_book_page):
// the published Telefonspiele in window.PHONE_BOOK, sorted by title, as the
// pages of a book. A click on the right page turns forward, on the left
// page back (also the buttons and the arrow keys); on a narrow screen one
// page at a time. An entry cannot be clicked to start a game: one types its
// number on the phone.
(function () {
    const PER_PAGE = 8;
    const book = document.getElementById('phone-book');
    if (!book) return;
    const entries = window.PHONE_BOOK || [];
    const notes = window.PHONE_BOOK_NOTES;
    const REMARK = 3;                 // the remark at the end takes the room of three entries
    const pages = [];
    for (let i = 0; i < entries.length; i += PER_PAGE) pages.push(entries.slice(i, i + PER_PAGE));
    if (pages.length === 0) pages.push([]);
    if (notes && pages[pages.length - 1].length > PER_PAGE - REMARK) pages.push([]);
    const remarkPage = pages.length - 1;
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

    function pageHtml(index) {
        const list = pages[index];
        if (!list) return '';
        const range = list.length ? `${letter(list[0].title)} – ${letter(list[list.length - 1].title)}` : '';
        const remark = notes && index === remarkPage
            ? `<div class='pb-remark'><div class='pb-remark-head'>Anmerkung</div><p>${esc(notes.line)} ${esc(notes.random)}</p>` +
              `<p>${esc(notes.own)} <a href='${esc(notes.href)}'>${esc(notes.link)}</a></p></div>` : '';
        const body = list.length ? list.map((e) =>
            `<div class='phone-book-entry'><div class='pb-line'><span class='pb-title'>${esc(e.title)}</span><span class='pb-dots'></span>` +
            `<span class='pb-number'>${esc(e.number)}</span></div>` +
            (e.description ? `<div class='pb-description'>${esc(e.description)}</div>` : '') + '</div>').join('')
            : (entries.length ? '' : "<p class='phone-book-none'>Noch steht kein Spiel im Telefonbuch.</p>");
        return `<div class='phone-book-head'><span>Telefonbuch</span><span>${esc(range)}</span></div>${body}${remark}` +
            `<span class='phone-book-folio'>${index + 1}</span>`;
    }

    function leaf(side, index) {
        const empty = !pages[index];
        return `<div class='phone-book-leaf ${side}${empty ? ' empty' : ''}' data-side='${side}'>${empty ? '' : pageHtml(index)}</div>`;
    }

    function render() {
        if (!narrow()) at -= at % 2;
        book.innerHTML = `<div class='phone-book-spread'>${narrow() ? leaf('right', at) : leaf('left', at) + leaf('right', at + 1)}</div>`;
        const last = Math.min(pages.length, at + step());
        document.getElementById('phone-book-where').textContent = pages.length > 1 ? `Seite ${at + 1}${last > at + 1 ? `–${last}` : ''} von ${pages.length}` : '';
        document.getElementById('phone-book-back').disabled = at === 0;
        document.getElementById('phone-book-next').disabled = at + step() >= pages.length;
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
        const page = e.target.closest('.phone-book-leaf');
        if (!page || page.classList.contains('empty')) return;
        turn(page.dataset.side === 'left' ? -1 : 1);
    });
    document.getElementById('phone-book-back').addEventListener('click', () => turn(-1));
    document.getElementById('phone-book-next').addEventListener('click', () => turn(1));
    document.addEventListener('keydown', (e) => {
        if (e.target.closest('input, textarea, select')) return;
        if (e.key === 'ArrowLeft') turn(-1);
        if (e.key === 'ArrowRight') turn(1);
    });
    window.matchMedia('(max-width: 640px)').addEventListener('change', render);
    render();
})();
