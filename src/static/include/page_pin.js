// Pinning an activity from its own page (main.rb, page_pin_button): the
// same pins as on the class page. With one class a click pins or unpins;
// with several the button opens a list of them, each with a tick.
(function () {
    document.querySelectorAll('.page-pin').forEach((box) => {
        const slug = box.dataset.slug;
        const groups = JSON.parse(box.dataset.groups || '[]');
        const toggle = box.querySelector('.page-pin-toggle');
        const label = toggle.querySelector('span');
        const icon = toggle.querySelector('.bi');

        function show() {
            const count = groups.filter((g) => g.pinned).length;
            icon.className = 'bi ' + (count > 0 ? 'bi-pin-angle-fill' : 'bi-pin-angle');
            label.textContent = count === 0 ? 'Anpinnen'
                : groups.length === 1 ? 'Angepinnt'
                : count === 1 ? 'Angepinnt: ' + groups.find((g) => g.pinned).name
                : 'Angepinnt für ' + count + ' Klassen';
            toggle.classList.toggle('pinned', count > 0);
            if (groups.length === 1) toggle.setAttribute('aria-pressed', count > 0 ? 'true' : 'false');
        }

        function set(group, on, done) {
            api_call(on ? '/api/class/pin' : '/api/class/unpin', { group: group.name, slug: slug }, (data) => {
                if (data.success) group.pinned = on;
                show();
                if (done) done();
            }, { no_please_wait: true });
        }

        if (groups.length === 1) {
            toggle.removeAttribute('aria-expanded');
            toggle.addEventListener('click', () => set(groups[0], !groups[0].pinned));
        } else {
            const menu = document.createElement('div');
            menu.className = 'page-pin-menu';
            menu.hidden = true;
            groups.forEach((group) => {
                const item = document.createElement('button');
                item.type = 'button';
                item.className = 'page-pin-item';
                const mark = () => {
                    item.setAttribute('aria-pressed', group.pinned ? 'true' : 'false');
                    item.innerHTML = "<i class='bi " + (group.pinned ? 'bi-check-square-fill' : 'bi-square') + "' aria-hidden='true'></i>";
                    item.appendChild(document.createTextNode(group.name));
                };
                mark();
                item.addEventListener('click', () => set(group, !group.pinned, mark));
                menu.appendChild(item);
            });
            box.appendChild(menu);
            toggle.addEventListener('click', () => {
                menu.hidden = !menu.hidden;
                toggle.setAttribute('aria-expanded', menu.hidden ? 'false' : 'true');
            });
            document.addEventListener('click', (event) => {
                if (!menu.hidden && !box.contains(event.target)) {
                    menu.hidden = true;
                    toggle.setAttribute('aria-expanded', 'false');
                }
            });
        }
        show();
    });
})();
