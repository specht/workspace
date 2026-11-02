// A grid with parity bits (content/check-digits, »ein Gitter, das ein Bit
// repariert«): <div class='parity-grid' data-title='…' data-flips='1'>
// holds the grid as an HTML table (its last column and last row are the
// parity bits, set bold), which stays as it is without JavaScript. The
// widget draws the same grid as buttons: a click flips a bit, the count of
// ones stands beside every row and under every column, rows and columns
// with an odd count turn red, and where they cross the cell is ringed.
// data-flips is how many bits may be flipped at once: with 1, flipping
// another bit puts the first one back, so that two flipped bits can be seen
// only where the page allows it (after the prediction »zwei-bits«).
(function () {
    'use strict';

    const ORDINAL = ['erste', 'zweite', 'dritte', 'vierte', 'fünfte', 'sechste', 'siebte', 'achte'];
    const COUNT = ['keine', 'eine', 'zwei', 'drei', 'vier', 'fünf', 'sechs', 'sieben', 'acht'];

    function el(tag, className, text) {
        const node = document.createElement(tag);
        if (className) node.className = className;
        if (text !== undefined) node.textContent = text;
        return node;
    }

    function readGrid(table) {
        return Array.from(table.querySelectorAll('tr')).map((tr) =>
            Array.from(tr.children).map((cell) => (cell.textContent.trim() === '1' ? 1 : 0)));
    }

    function ones(bits) {
        return bits.reduce((sum, bit) => sum + bit, 0);
    }

    // what the parities say, in one or two sentences
    function message(oddRows, oddColumns, flipped, maxFlips) {
        if (flipped.length === 0) {
            return 'Jede Zeile und jede Spalte hat eine gerade Zahl von Einsen. Klick auf ein Bit, um es zu kippen.';
        }
        if (oddRows.length === 1 && oddColumns.length === 1) {
            return `Die ${ORDINAL[oddRows[0]]} Zeile und die ${ORDINAL[oddColumns[0]]} Spalte haben eine ungerade Zahl von Einsen. ` +
                'Wo sie sich kreuzen, steht das gekippte Bit; ein Klick darauf repariert es.';
        }
        if (oddRows.length === 0 && oddColumns.length === 0) {
            return 'Alle Zeilen und Spalten sind wieder gerade.';
        }
        // »keine Zeile«, »eine Zeile«, »zwei Zeilen«
        const some = (n, one, many) => (n < 2 ? `${COUNT[n]} ${one}` : `${COUNT[n]} ${many}`);
        const rows = some(oddRows.length, 'Zeile', 'Zeilen');
        const columns = some(oddColumns.length, 'Spalte', 'Spalten');
        const none = oddRows.length === 0 || oddColumns.length === 0;
        let text = `${rows[0].toUpperCase()}${rows.slice(1)}${none ? ', aber' : ' und'} ${columns} haben eine ungerade Zahl von Einsen.`;
        if (none) {
            text += ' Ohne Kreuzung findet das Gitter den Fehler nicht.';
        } else if (oddRows.length > 1 && oddColumns.length > 1) {
            text += ` Ein gekipptes Bit könnte an ${COUNT[oddRows.length * oddColumns.length]} Kreuzungen stehen.`;
        }
        if (maxFlips > 1 && flipped.length === maxFlips) {
            text += ' Ein weiterer Klick dreht das zuerst gekippte Bit zurück.';
        }
        return text;
    }

    function mount(box) {
        const table = box.querySelector('table');
        if (!table) return;
        const start = readGrid(table);
        const size = start.length;
        const width = start[0].length;
        const maxFlips = Math.max(1, parseInt(box.dataset.flips || '1', 10));
        let grid = start.map((row) => row.slice());
        let flipped = []; // [row, column] in the order they were flipped

        const head = el('div', 'parity-head');
        head.appendChild(el('span', 'parity-title', box.dataset.title || 'Zum Ausprobieren'));
        const reset = el('button', 'btn btn-sm btn-outline-secondary parity-reset');
        reset.type = 'button';
        reset.appendChild(el('i', 'bi bi-arrow-counterclockwise'));
        reset.appendChild(document.createTextNode(' Zurücksetzen'));
        head.appendChild(reset);

        const picture = el('div', 'parity-picture');
        const board = el('div', 'parity-board');
        board.style.gridTemplateColumns = `repeat(${width}, var(--parity-cell)) var(--parity-count)`;
        picture.appendChild(board);
        const cells = [];
        const rowCounts = [];
        const columnCounts = [];
        for (let r = 0; r < size; r++) {
            cells.push([]);
            for (let c = 0; c < width; c++) {
                const button = el('button', 'parity-cell');
                button.type = 'button';
                if (r === size - 1 || c === width - 1) button.classList.add('parity-check');
                button.addEventListener('click', () => flip(r, c));
                board.appendChild(button);
                cells[r].push(button);
            }
            const count = el('span', 'parity-count');
            board.appendChild(count);
            rowCounts.push(count);
        }
        for (let c = 0; c < width; c++) {
            const count = el('span', 'parity-count parity-count-column');
            board.appendChild(count);
            columnCounts.push(count);
        }
        board.appendChild(el('span', 'parity-corner'));

        const legend = el('div', 'parity-legend');
        const flippedKey = el('span', 'parity-key');
        flippedKey.appendChild(el('span', 'parity-swatch parity-swatch-flipped'));
        flippedKey.appendChild(document.createTextNode('gekippt'));
        const oddKey = el('span', 'parity-key');
        oddKey.appendChild(el('span', 'parity-swatch parity-swatch-odd'));
        oddKey.appendChild(document.createTextNode('ungerade Zahl von Einsen'));
        const boldKey = el('span', 'parity-key');
        boldKey.appendChild(el('span', 'parity-swatch parity-swatch-check'));
        boldKey.appendChild(document.createTextNode('Paritätsbit'));
        legend.append(boldKey, flippedKey, oddKey);
        const text = el('p', 'parity-message');

        box.replaceChildren(head, picture, legend, text);
        box.classList.add('parity-ready');

        function flip(r, c) {
            const at = flipped.findIndex(([fr, fc]) => fr === r && fc === c);
            if (at >= 0) {
                flipped.splice(at, 1);
            } else {
                if (flipped.length >= maxFlips) {
                    const [or, oc] = flipped.shift();
                    grid[or][oc] = start[or][oc];
                }
                flipped.push([r, c]);
            }
            grid[r][c] = flipped.some(([fr, fc]) => fr === r && fc === c) ? 1 - start[r][c] : start[r][c];
            show();
        }

        reset.addEventListener('click', () => {
            grid = start.map((row) => row.slice());
            flipped = [];
            show();
        });

        function show() {
            const oddRows = [];
            const oddColumns = [];
            for (let r = 0; r < size; r++) {
                const n = ones(grid[r]);
                rowCounts[r].textContent = n;
                rowCounts[r].classList.toggle('parity-odd', n % 2 === 1);
                if (n % 2 === 1) oddRows.push(r);
            }
            for (let c = 0; c < width; c++) {
                const n = ones(grid.map((row) => row[c]));
                columnCounts[c].textContent = n;
                columnCounts[c].classList.toggle('parity-odd', n % 2 === 1);
                if (n % 2 === 1) oddColumns.push(c);
            }
            for (let r = 0; r < size; r++) {
                for (let c = 0; c < width; c++) {
                    const cell = cells[r][c];
                    const inRow = oddRows.includes(r);
                    const inColumn = oddColumns.includes(c);
                    cell.textContent = grid[r][c];
                    cell.classList.toggle('parity-flipped', grid[r][c] !== start[r][c]);
                    cell.classList.toggle('parity-line', inRow || inColumn);
                    cell.classList.toggle('parity-cross', inRow && inColumn);
                    const what = r === size - 1 || c === width - 1 ? 'Paritätsbit' : 'Bit';
                    cell.setAttribute('aria-label', `${what} in der ${ORDINAL[r]} Zeile und ${ORDINAL[c]} Spalte: ${grid[r][c]}`);
                }
            }
            text.textContent = message(oddRows, oddColumns, flipped, maxFlips);
        }
        show();
    }

    function start() {
        document.querySelectorAll('.parity-grid').forEach((box) => {
            if (box.dataset.ready) return;
            box.dataset.ready = '1';
            mount(box);
        });
    }
    if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', start);
    else start();
})();
