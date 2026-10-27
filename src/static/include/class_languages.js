// »Sprachen der Klasse« on the class page (class_settings_html): the
// languages a class uses, ranked; each activity shows the first of them it
// offers (CodeLanguages.resolve). Below them the other languages, the most
// offered first. A language moves by dragging (mouse, pen or touch on its
// grip), by its buttons or by the keyboard (↑/↓ move, Entf removes, Enter
// adds); every move glides (FLIP), is said aloud (aria-live) and saved at
// once. The list keeps at least one language. A sentence below says what
// the order means for the activities.
(function () {
    const reduced = window.matchMedia && window.matchMedia('(prefers-reduced-motion: reduce)').matches;
    const esc = (t) => String(t).replace(/[&<>"']/g, (c) => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' })[c]);

    function setup(root) {
        if (root.dataset.ready) return;
        root.dataset.ready = '1';
        const data = JSON.parse(root.dataset.languages);
        const names = {};
        data.languages.forEach((l) => { names[l.id] = l.name; });
        const count = {};
        data.languages.forEach((l) => { count[l.id] = data.activities.filter((a) => a.languages.includes(l.id)).length; });
        let ranked = data.ranked.filter((l) => names[l]);
        if (!ranked.length) ranked = [data.default];
        const activities = (n) => (n === 1 ? 'einer Aktivität' : `${n} Aktivitäten`);

        root.innerHTML = "<ol class='cl-list' aria-labelledby='class-languages-title'></ol>" +
            "<h4 class='class-setting-subtitle cl-more-title'>Weitere Sprachen</h4>" +
            "<ul class='cl-more' aria-label='Weitere Sprachen'></ul>" +
            "<p class='class-setting-hint cl-preview'></p>" +
            "<p class='visually-hidden cl-say' aria-live='polite'></p>";
        const list = root.querySelector('.cl-list');
        const more = root.querySelector('.cl-more');
        const moreTitle = root.querySelector('.cl-more-title');
        const preview = root.querySelector('.cl-preview');
        const say = root.querySelector('.cl-say');

        // one element per language, moved between the two lists
        const items = {};
        data.languages.forEach((l) => {
            const li = document.createElement('li');
            li.className = 'cl-item';
            li.dataset.lang = l.id;
            li.tabIndex = 0;
            li.innerHTML = "<span class='cl-rank' aria-hidden='true'></span>" +
                "<i class='bi bi-grip-vertical cl-grip' aria-hidden='true'></i>" +
                `<img class='cl-logo' src='/images/languages/${esc(l.id)}.webp' alt='' width='22' height='22'>` +
                `<span class='cl-name'>${esc(l.name)}</span>` +
                `<span class='cl-count'>in ${activities(count[l.id])}</span>` +
                "<span class='cl-tools'>" +
                `<button type='button' class='cl-btn cl-up' aria-label='${esc(l.name)} nach oben'><i class='bi bi-arrow-up' aria-hidden='true'></i></button>` +
                `<button type='button' class='cl-btn cl-down' aria-label='${esc(l.name)} nach unten'><i class='bi bi-arrow-down' aria-hidden='true'></i></button>` +
                `<button type='button' class='cl-btn cl-remove' aria-label='${esc(l.name)} entfernen'><i class='bi bi-x-lg' aria-hidden='true'></i></button>` +
                `<button type='button' class='cl-btn cl-add' aria-label='${esc(l.name)} hinzufügen'><i class='bi bi-plus-lg' aria-hidden='true'></i><span>Hinzufügen</span></button>` +
                '</span>';
            items[l.id] = li;
        });

        // the other languages: most offered first, then by name
        const spare = () => data.languages.map((l) => l.id).filter((id) => !ranked.includes(id))
            .sort((a, b) => count[b] - count[a] || names[a].localeCompare(names[b]));

        // The positions of every item before a change, and the glide from
        // there to where the change puts them
        const measure = () => {
            const rects = new Map();
            Object.values(items).forEach((li) => rects.set(li, li.getBoundingClientRect()));
            return rects;
        };
        const glide = (rects, skip) => {
            if (reduced) return;
            rects.forEach((old, li) => {
                if (li === skip || !li.isConnected) return;
                const now = li.getBoundingClientRect();
                const dx = old.left - now.left;
                const dy = old.top - now.top;
                if (Math.abs(dx) < 1 && Math.abs(dy) < 1) return;
                li.animate([{ transform: `translate(${dx}px, ${dy}px)` }, { transform: 'none' }],
                    { duration: Math.min(420, 180 + Math.hypot(dx, dy) * 0.6), easing: 'cubic-bezier(.2,.8,.2,1)' });
            });
        };

        function layout() {
            ranked.forEach((id, i) => {
                const li = items[id];
                list.appendChild(li);
                li.querySelector('.cl-rank').textContent = i + 1;
                li.setAttribute('aria-label', `${names[id]}, Stelle ${i + 1} von ${ranked.length}, in ${activities(count[id])}`);
                const last = ranked.length === 1;
                const remove = li.querySelector('.cl-remove');
                remove.setAttribute('aria-disabled', last ? 'true' : 'false');
                remove.title = last ? 'Die Klasse braucht mindestens eine Sprache.' : `${names[id]} entfernen`;
                li.querySelector('.cl-up').disabled = i === 0;
                li.querySelector('.cl-down').disabled = i === ranked.length - 1;
            });
            const rest = spare();
            rest.forEach((id) => {
                const li = items[id];
                more.appendChild(li);
                li.setAttribute('aria-label', `${names[id]}, in ${activities(count[id])}, nicht in der Liste. Enter fügt sie hinzu.`);
            });
            moreTitle.hidden = !rest.length;
            more.hidden = !rest.length;
            preview.textContent = sentence();
        }

        // What the order means: each activity in the first language of the
        // list it offers, else Ruby, else its own first
        function sentence() {
            const shown = data.activities.map((a) => ranked.find((l) => a.languages.includes(l)) ||
                (a.languages.includes(data.default) ? data.default : a.languages[0]));
            const order = ranked.concat(Array.from(new Set(shown)).filter((l) => !ranked.includes(l)));
            const buckets = order.map((l) => ({ l, titles: data.activities.filter((_, i) => shown[i] === l).map((a) => a.title) }))
                .filter((b) => b.titles.length);
            if (!buckets.length) return '';
            if (buckets.length === 1) {
                const n = buckets[0].titles.length;
                return `Damit ${n === 1 ? 'zeigt die eine Aktivität' : `zeigen alle ${n} Aktivitäten`} ${names[buckets[0].l]}.`;
            }
            const parts = buckets.map((b, i) => {
                const n = b.titles.length;
                const own = ranked.includes(b.l);
                let who;
                if (n <= 2) who = b.titles.map((t) => `»${t}«`).join(' und ');
                else if (own && i === buckets.length - 1) who = `die übrigen ${n}`;
                else if (!own) who = `${n} Aktivitäten, die ${ranked.length === 1 ? `kein ${names[ranked[0]]}` : 'keine dieser Sprachen'} anbieten,`;
                else who = `${n} Aktivitäten`;
                return `${who} ${names[b.l]}`;
            });
            return `Damit zeigen ${parts.slice(0, -1).join(', ')} und ${parts[parts.length - 1]}.`;
        }

        // saved a moment after the last change, so that a run of moves is one request
        let timer = null;
        function save() {
            clearTimeout(timer);
            const languages = ranked.slice();
            timer = setTimeout(() => {
                api_call('/api/set_group_code_languages', { group: data.group, languages }, () => show_success_message('Gespeichert.'), { no_please_wait: true });
            }, 350);
        }

        function tell(text) {
            say.textContent = '';
            requestAnimationFrame(() => { say.textContent = text; });
        }

        // A change of the ranking: glide, announce, save; focus stays with
        // the language that moved
        function change(next, moved, text, skip) {
            if (next.join() === ranked.join()) return;
            const rects = measure();
            ranked = next;
            layout();
            glide(rects, skip);
            if (moved) items[moved].focus({ preventScroll: true });
            tell(text);
            save();
        }
        const move = (id, by) => {
            const i = ranked.indexOf(id);
            const j = i + by;
            if (i < 0 || j < 0 || j >= ranked.length) return;
            const next = ranked.slice();
            next.splice(i, 1);
            next.splice(j, 0, id);
            change(next, id, `${names[id]} ist jetzt an Stelle ${j + 1} von ${next.length}.`);
        };
        const add = (id, at) => {
            if (ranked.includes(id)) return;
            const next = ranked.slice();
            next.splice(at === undefined ? next.length : at, 0, id);
            change(next, id, `${names[id]} hinzugefügt, an Stelle ${next.indexOf(id) + 1} von ${next.length}.`);
        };
        const remove = (id) => {
            if (ranked.length === 1) {
                tell('Die Klasse braucht mindestens eine Sprache.');
                const li = items[id];
                if (!reduced) li.animate([{ transform: 'translateX(0)' }, { transform: 'translateX(-5px)' }, { transform: 'translateX(5px)' }, { transform: 'translateX(0)' }], { duration: 260 });
                return;
            }
            const i = ranked.indexOf(id);
            change(ranked.filter((l) => l !== id), null, `${names[id]} entfernt.`);
            const focus = ranked[Math.min(i, ranked.length - 1)];
            if (focus) items[focus].focus({ preventScroll: true });
        };

        root.addEventListener('click', (event) => {
            const li = event.target.closest('.cl-item');
            if (!li || dragged) return;
            const id = li.dataset.lang;
            const button = event.target.closest('.cl-btn');
            if (button) {
                if (button.classList.contains('cl-up')) move(id, -1);
                else if (button.classList.contains('cl-down')) move(id, 1);
                else if (button.classList.contains('cl-remove')) remove(id);
                else if (button.classList.contains('cl-add')) add(id);
                return;
            }
            // a language below: a tap adds it
            if (li.parentElement === more) add(id);
        });
        root.addEventListener('keydown', (event) => {
            const li = event.target.closest('.cl-item');
            if (!li || event.target !== li) return;
            const id = li.dataset.lang;
            const inList = li.parentElement === list;
            if (inList && (event.key === 'ArrowUp' || event.key === 'ArrowDown')) {
                event.preventDefault();
                move(id, event.key === 'ArrowUp' ? -1 : 1);
            } else if (inList && (event.key === 'Delete' || event.key === 'Backspace')) {
                event.preventDefault();
                remove(id);
            } else if (!inList && (event.key === 'Enter' || event.key === ' ')) {
                event.preventDefault();
                add(id);
            }
        });

        // Dragging: the item lifts and follows the pointer, a gap opens where
        // it would land and the others glide aside; dropped below, it leaves
        // the list (unless it is the last), dropped in the list from below,
        // it joins. A mouse can take it anywhere, a finger by its grip, so
        // that the page still scrolls under a finger.
        let press = null;
        let dragged = null;
        let gap = null;
        root.addEventListener('pointerdown', (event) => {
            const li = event.target.closest('.cl-item');
            if (!li || event.button !== 0 || event.target.closest('.cl-btn')) return;
            if (event.pointerType === 'touch' && !event.target.closest('.cl-grip, .cl-logo, .cl-rank')) return;
            // no text selection while the row is held; the row has the focus
            if (event.pointerType !== 'touch') {
                event.preventDefault();
                li.focus({ preventScroll: true });
            }
            press = { li, x: event.clientX, y: event.clientY, id: event.pointerId };
        });
        root.addEventListener('pointermove', (event) => {
            if (!press || event.pointerId !== press.id) return;
            if (!dragged) {
                if (Math.hypot(event.clientX - press.x, event.clientY - press.y) < 5) return;
                start(event);
            }
            follow(event);
        });
        const end = (event, dropped) => {
            if (!press || event.pointerId !== press.id) return;
            if (dragged) drop(dropped);
            press = null;
        };
        root.addEventListener('pointerup', (event) => end(event, true));
        root.addEventListener('pointercancel', (event) => end(event, false));

        function start(event) {
            const li = press.li;
            const rect = li.getBoundingClientRect();
            press.dx = press.x - rect.left;
            press.dy = press.y - rect.top;
            press.from = li.parentElement === list ? 'list' : 'more';
            gap = document.createElement('li');
            gap.className = 'cl-gap';
            gap.style.height = `${rect.height}px`;
            li.parentElement.insertBefore(gap, li);
            dragged = li;
            li.setPointerCapture(event.pointerId);
            li.classList.add('cl-dragging');
            Object.assign(li.style, { position: 'fixed', left: `${rect.left}px`, top: `${rect.top}px`, width: `${rect.width}px`, margin: '0' });
            document.body.classList.add('cl-dragging-page');
        }

        function follow(event) {
            dragged.style.left = `${event.clientX - press.dx}px`;
            dragged.style.top = `${event.clientY - press.dy}px`;
            // where the gap goes: in the list by the middles of its items,
            // below it in the other languages' sorted place; away from both
            // (beside them, above or far below) it closes
            const listBox = list.getBoundingClientRect();
            const area = root.getBoundingClientRect();
            const x = event.clientX;
            const y = event.clientY;
            const near = x > area.left - 60 && x < Math.min(area.right, listBox.right) + 60;
            const inList = near && y > listBox.top - 40 && y <= listBox.bottom + 24;
            const moreBox = more.getBoundingClientRect();
            const inMore = near && !inList && y > listBox.bottom + 24 && y < Math.max(moreBox.bottom, listBox.bottom + 80) + 40;
            const canLeave = press.from === 'more' || ranked.length > 1;
            let target;
            let before = null;
            if (!inList && !(inMore && canLeave)) {
                if (gap.parentElement) {
                    const rects = measure();
                    gap.remove();
                    glide(rects, dragged);
                    renumber();
                }
                return;
            }
            if (inMore) {
                target = more;
                const id = dragged.dataset.lang;
                const order = spare().concat(press.from === 'list' ? [id] : []).filter((l, i, a) => a.indexOf(l) === i)
                    .sort((a, b) => count[b] - count[a] || names[a].localeCompare(names[b]));
                const after = order.slice(order.indexOf(id) + 1);
                before = after.map((l) => items[l]).find((li) => li.parentElement === more) || null;
            } else {
                target = list;
                const others = Array.from(list.children).filter((c) => c !== dragged && c !== gap);
                before = others.find((c) => {
                    const r = c.getBoundingClientRect();
                    return y < r.top + r.height / 2;
                }) || null;
            }
            if (gap.parentElement === target && gap.nextSibling === before) return;
            if (target === more && more.hidden) { more.hidden = false; moreTitle.hidden = false; }
            const rects = measure();
            target.insertBefore(gap, before);
            // out of the flow, it takes the look of where it would land
            if (dragged.parentElement !== target) target.appendChild(dragged);
            glide(rects, dragged);
            renumber();
        }

        // the numbers while dragging: as they will be after the drop
        function renumber() {
            const order = Array.from(list.children).filter((c) => c !== dragged).map((c) => (c === gap ? dragged : c));
            order.forEach((li, i) => { li.querySelector('.cl-rank').textContent = i + 1; });
        }

        function drop(dropped) {
            const li = dragged;
            const id = li.dataset.lang;
            const from = li.getBoundingClientRect();
            let next = ranked.filter((l) => l !== id);
            if (!gap.parentElement) {
                // let go away from the lists: back where it came from
                next = ranked.slice();
            } else if (dropped && gap.parentElement === list) {
                const index = Array.from(list.children).filter((c) => c !== li).indexOf(gap);
                next.splice(index, 0, id);
            } else if (!dropped || (press.from === 'list' && next.length === 0)) {
                next = ranked.slice();
            }
            // back into the flow, then gliding from where it was let go
            li.classList.remove('cl-dragging');
            li.removeAttribute('style');
            gap.remove();
            gap = null;
            document.body.classList.remove('cl-dragging-page');
            const rects = measure();
            rects.set(li, from);
            const changed = next.join() !== ranked.join();
            const before = ranked;
            ranked = next;
            layout();
            glide(rects);
            if (changed) {
                const i = next.indexOf(id);
                tell(i < 0 ? `${names[id]} entfernt.` : before.includes(id)
                    ? `${names[id]} ist jetzt an Stelle ${i + 1} von ${next.length}.`
                    : `${names[id]} hinzugefügt, an Stelle ${i + 1} von ${next.length}.`);
                save();
            }
            // the click that ends a drag is no tap
            setTimeout(() => { dragged = null; }, 0);
        }

        layout();
    }

    document.querySelectorAll('.class-languages').forEach(setup);
})();
