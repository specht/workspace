// Screenshot hooks of the Logik-Werkstatt (src/tutorial-screenshots/README.md,
// »Tutorial-specific hooks«): the free editor at the top of the page with a
// circuit loaded, light and dark.

// the circuits the pictures show, as netlists (logic_circuit.js)
const CIRCUITS = {
    // a half adder half built: the XOR still waits for b
    intro: `in a @48,48
in b @48,152
xor g1 a - @200,40
and g2 a b @200,136
out s g1 @400,64
out c g2 @400,160
`,
    // the full adder of two half adders
    teaser: `block HA
  in a
  in b
  xor x a b
  and k a b
  out s x
  out c k
end
in a @64,56
in b @64,104
in c_in @64,160
HA h1 a b @136,24
HA h2 h1.s c_in @232,104
or o h1.c h2.c @408,152
out s h2.s @536,136
out c_out o @536,176
`,
};

export default {
    variants: {
        async dark({ page, on }) {
            await page.emulateMedia({ colorScheme: on ? 'dark' : 'light' });
            await page.evaluate((on) => document.documentElement.setAttribute('data-bs-theme', on ? 'dark' : 'light'), on);
            await page.waitForTimeout(500);
        },
    },

    actions: {
        // hook: sandbox NAME: the free editor with the circuit NAME, on the
        // page's background; intro: a wire being drawn from b to the XOR,
        // teaser: a and c_in switched on
        async sandbox({ page, args }) {
            const name = args.trim();
            await page.evaluate(({ text, name }) => {
                const el = document.querySelector('.circuit-editor[data-free]');
                const editor = el.circuitEditor;
                const { circuit } = window.LogicCircuit.Netlist.parse(text);
                editor.c = circuit;
                editor.inputs = {};
                editor.values = {};
                editor.sel = null;
                editor.history = [];
                editor.future = [];
                editor.after(false, false);
                if (name === 'teaser') editor.setInputs({ a: 1, c_in: 1 });
                el.style.setProperty('background', 'var(--bs-body-bg)', 'important');
                // the page's fixed bars (contents, footer) would cover it
                for (const element of document.querySelectorAll('body *')) {
                    if (el.contains(element) || element.contains(el)) continue;
                    const { position } = getComputedStyle(element);
                    if (position === 'fixed' || position === 'sticky') element.style.opacity = '0';
                }
            }, { text: CIRCUITS[name], name });
            const editor = page.locator('.circuit-editor[data-free]');
            await editor.scrollIntoViewIfNeeded();
            await page.waitForTimeout(1500);
            if (name === 'intro') {
                // press on b's output and move most of the way to the XOR's second input
                const from = await editor.locator(".ce-pin-hit[data-part='b'][data-pin='out']").boundingBox();
                const to = await editor.locator(".ce-pin-hit[data-part='g1'][data-index='1']").boundingBox();
                const x0 = from.x + from.width / 2;
                const y0 = from.y + from.height / 2;
                const x1 = to.x + to.width / 2;
                const y1 = to.y + to.height / 2;
                await page.mouse.move(x0, y0);
                await page.mouse.down();
                for (let i = 1; i <= 8; i++) await page.mouse.move(x0 + (x1 - x0 - 30) * i / 8, y0 + (y1 - y0 + 18) * i / 8);
                await page.waitForTimeout(300);
            } else {
                await page.mouse.move(1, 1);
            }
            await page.waitForTimeout(400);
        },
    },
};
