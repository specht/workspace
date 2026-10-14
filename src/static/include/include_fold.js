// Handbuch boxes (_include) the person has already been through on another
// page shrink to one line: detailed the first time, one line after. The
// server names the parts and the page where they were done (SEEN_PARTS,
// include_fold_setup). Only when the page loads, never while someone reads.
(function () {
    const seen = window.SEEN_PARTS || {};
    document.querySelectorAll('.reference-include[data-part]').forEach((box) => {
        const where = seen[box.dataset.part];
        // A box with done: folds through tasks.js once its own task is solved.
        if (!where || box.dataset.done || box.classList.contains('reference-include-folded')) return;
        const line = document.createElement('p');
        line.className = 'reference-include-done';
        const icon = document.createElement('i');
        icon.className = 'bi bi-arrow-repeat';
        line.appendChild(icon);
        const text = document.createElement('span');
        if (box.dataset.short) {
            // Written by the Handbuch page: escaped text and <code> only.
            text.innerHTML = box.dataset.short;
        } else {
            text.textContent = box.dataset.title || 'Wie gehabt';
        }
        line.appendChild(text);
        const note = document.createElement('span');
        note.className = 'reference-include-where';
        note.textContent = ' Kennst du schon aus »' + where + '«.';
        line.appendChild(note);
        const show = document.createElement('button');
        show.type = 'button';
        show.className = 'btn btn-link btn-sm';
        show.textContent = 'Anleitung anzeigen';
        show.addEventListener('click', function () {
            box.classList.remove('reference-include-folded');
            line.remove();
        });
        line.appendChild(show);
        box.prepend(line);
        box.classList.add('reference-include-folded');
    });
})();
