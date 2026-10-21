// Screenshot hooks for »Endliche Automaten«
// (src/tutorial-screenshots/README.md, »Tutorial-specific hooks«).

// The smallest DFA for »a decimal number is divisible by 3«: the state is the
// digit sum modulo 3. Drawn as Automatron draws (automaton_layout.js), but
// from the automaton itself, not from a regular expression, whose
// intermediate automata are far too large.
const GROUPS = ['0369', '147', '258'];
function divisibleBy3() {
    const edges = [];
    for (let from = 0; from < 3; from++) {
        // the group g adds g to the remainder
        GROUPS.forEach((digits, g) => {
            for (const d of digits) edges.push({ from: `q${from}`, to: `q${(from + g) % 3}`, label: d });
        });
    }
    return { states: [0, 1, 2].map((i) => ({ id: `q${i}`, label: `q${i}` })), start: 'q0', accept: ['q0'], edges };
}

export default {
    // variants: dark: Automatron's drawings follow the site's colour scheme,
    // and the site follows the system's.
    actions: {
        // hook: divisible-by-3: shows that DFA where Automatron shows its
        // drawings, in the page's own colours and font.
        async 'divisible-by-3'({ page }) {
            await page.evaluate((automaton) => {
                document.getElementById('output_area').innerHTML = `<div class="automaton-drawing">${window.AutomatonLayout.svg(automaton)}</div>`;
            }, divisibleBy3());
            await page.waitForTimeout(300);
        },
    },

    variants: {
        async dark({ page, on }) {
            await page.emulateMedia({ colorScheme: on ? 'dark' : 'light' });
            await page.waitForTimeout(500);
        },
    },
};
