// Screenshot hooks for »Reguläre Ausdrücke«
// (src/tutorial-screenshots/README.md, »Tutorial-specific hooks«).

// The smallest DFA for [0-9]+(\.[0-9]+)?, a number with a decimal point:
// drawn as Automatron draws (automaton_layout.js), from the automaton itself.
// Automatron knows no character classes; with all ten digits written out its
// intermediate automata would be far too large to draw.
const DIGITS = '0123456789';
function decimalNumber() {
    const edges = [];
    for (const d of DIGITS) {
        edges.push({ from: 'q0', to: 'q1', label: d }, { from: 'q1', to: 'q1', label: d },
            { from: 'q2', to: 'q3', label: d }, { from: 'q3', to: 'q3', label: d });
    }
    edges.push({ from: 'q1', to: 'q2', label: '.' });
    return { states: [0, 1, 2, 3].map((i) => ({ id: `q${i}`, label: `q${i}` })), start: 'q0', accept: ['q1', 'q3'], edges };
}

export default {
    actions: {
        // hook: regex-teaser: the expression above its automaton, in the
        // page's own colours and fonts, where Automatron shows its drawings
        async 'regex-teaser'({ page }) {
            await page.evaluate((automaton) => {
                const svg = window.AutomatonLayout.svg(automaton);
                document.getElementById('output_area').innerHTML = `<div class="regex-teaser" style="display: inline-block; padding: 1.2em 1.6em; text-align: center;">`
                    + `<div style="font-family: '0xProto', monospace; font-size: 1.9em; margin-bottom: 0.6em;">[0-9]+(\\.[0-9]+)?</div>`
                    + `<div class="automaton-drawing">${svg}</div></div>`;
            }, decimalNumber());
            await page.waitForTimeout(300);
        },
    },

    variants: {
        // the drawing follows the site's colour scheme, and the site the system's
        async dark({ page, on }) {
            await page.emulateMedia({ colorScheme: on ? 'dark' : 'light' });
            await page.waitForTimeout(500);
        },
    },
};
