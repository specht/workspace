// Katas handed in to be shown in class (KataHandIns), on the class board:
// »Namen zeigen« shows who handed in what; each »Lösung n« opens in a viewer
// large enough for the projector, with the previous and next one beside it
// (also with the arrow keys), and without the name unless it is switched on.
(function () {
    document.addEventListener('change', function (e) {
        const toggle = e.target.closest('.kata-hand-ins-names input');
        if (!toggle) return;
        toggle.closest('.kata-hand-ins').querySelectorAll('.kata-hand-in-name').forEach(function (el) {
            el.hidden = !toggle.checked;
        });
    });

    document.addEventListener('click', function (e) {
        const button = e.target.closest('.kata-hand-in');
        if (button) show(button);
    });

    function show(button) {
        const box = button.closest('.kata-hand-ins');
        const items = Array.from(box.querySelectorAll('.kata-hand-in'));
        const names = box.querySelector('.kata-hand-ins-names input');
        let index = items.indexOf(button);
        const dialog = document.createElement('dialog');
        dialog.className = 'kata-viewer';
        dialog.innerHTML = "<div class='kata-viewer-head'><span class='kata-viewer-title'></span>" +
            "<button type='button' class='kata-viewer-prev' title='Vorige Lösung'><i class='bi bi-chevron-left' aria-hidden='true'></i><span class='visually-hidden'>Vorige Lösung</span></button>" +
            "<button type='button' class='kata-viewer-next' title='Nächste Lösung'><i class='bi bi-chevron-right' aria-hidden='true'></i><span class='visually-hidden'>Nächste Lösung</span></button>" +
            "<button type='button' class='kata-viewer-close' title='Schließen'><i class='bi bi-x-lg' aria-hidden='true'></i><span class='visually-hidden'>Schließen</span></button></div>" +
            "<div class='kata-viewer-code'></div>";
        const render = function () {
            const item = items[index];
            const label = item.dataset.label;
            dialog.querySelector('.kata-viewer-title').textContent = names && names.checked ? label + ': ' + item.dataset.name : label;
            const code = dialog.querySelector('.kata-viewer-code');
            code.innerHTML = '';
            code.appendChild(item.parentElement.querySelector('template').content.cloneNode(true));
            dialog.querySelector('.kata-viewer-prev').disabled = index === 0;
            dialog.querySelector('.kata-viewer-next').disabled = index === items.length - 1;
        };
        const step = function (delta) {
            index = Math.max(0, Math.min(items.length - 1, index + delta));
            render();
        };
        dialog.querySelector('.kata-viewer-prev').addEventListener('click', function () { step(-1); });
        dialog.querySelector('.kata-viewer-next').addEventListener('click', function () { step(1); });
        dialog.querySelector('.kata-viewer-close').addEventListener('click', function () { dialog.close(); });
        dialog.addEventListener('keydown', function (e) {
            if (e.key === 'ArrowLeft') step(-1);
            if (e.key === 'ArrowRight') step(1);
        });
        dialog.addEventListener('close', function () { dialog.remove(); });
        document.body.appendChild(dialog);
        render();
        dialog.showModal();
    }
})();
