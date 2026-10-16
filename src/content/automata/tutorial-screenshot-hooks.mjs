// Screenshot hooks for »Endliche Automaten«
// (src/tutorial-screenshots/README.md, »Tutorial-specific hooks«).

// The smallest DFA for »a decimal number is divisible by 3«: the state is the
// digit sum modulo 3. Drawn here in the structure Automatron's drawings have
// (svg.automaton, g.node, g.edge), so its CSS colours it in both themes; not
// built from a regular expression, whose intermediate automata are far too
// large to draw, and not laid out by Graphviz, which draws each pair of
// opposite edges on top of each other.
function divisibleBy3Svg() {
    const R = 22;                       // radius of a state
    // loop: where the loop sits, in degrees (0 = right, -90 = up)
    const states = [
        { x: 110, y: 190, name: '0', accept: true, loop: -110 },
        { x: 420, y: 75, name: '1', loop: -40 },
        { x: 420, y: 305, name: '2', loop: 40 },
    ];
    const groups = ['0, 3, 6, 9', '1, 4, 7', '2, 5, 8'];
    const f = (n) => n.toFixed(1);
    const parts = [];
    const arrow = (tipX, tipY, ux, uy) => {
        // ux, uy: the direction the arrow points
        const back = 9, half = 4;
        const bx = tipX - ux * back, by = tipY - uy * back;
        return `<polygon fill="black" stroke="black" points="${f(tipX)},${f(tipY)} ${f(bx - uy * half)},${f(by + ux * half)} ${f(bx + uy * half)},${f(by - ux * half)}"/>`;
    };
    const label = (x, y, text, anchor = 'middle') => `<text text-anchor="${anchor}" x="${f(x)}" y="${f(y + 4.5)}" font-size="14.00">${text}</text>`;

    // The start arrow
    const s0 = states[0];
    parts.push(`<g class="edge"><path fill="none" stroke="black" d="M${f(s0.x - 80)},${f(s0.y)}L${f(s0.x - R - 4 - 9)},${f(s0.y)}"/>${arrow(s0.x - R - 4, s0.y, 1, 0)}</g>`);

    states.forEach((from, i) => {
        const ring = from.accept ? R + 4 : R;
        // A loop for 0, 3, 6, 9: they keep the remainder
        const a = from.loop * Math.PI / 180, spread = 0.5;
        const p1 = [from.x + ring * Math.cos(a - spread), from.y + ring * Math.sin(a - spread)];
        const tip = [from.x + (ring + 1) * Math.cos(a + spread), from.y + (ring + 1) * Math.sin(a + spread)];
        const c1 = [from.x + 3.4 * R * Math.cos(a - spread * 1.3), from.y + 3.4 * R * Math.sin(a - spread * 1.3)];
        const c2 = [from.x + 3.4 * R * Math.cos(a + spread * 1.3), from.y + 3.4 * R * Math.sin(a + spread * 1.3)];
        const ux = tip[0] - c2[0], uy = tip[1] - c2[1], ul = Math.hypot(ux, uy);
        const end = [tip[0] - ux / ul * 9, tip[1] - uy / ul * 9];
        const out = 2.55 * R + 12;
        const lx = from.x + out * Math.cos(a), ly = from.y + out * Math.sin(a);
        const anchor = Math.cos(a) > 0.35 ? 'start' : Math.cos(a) < -0.35 ? 'end' : 'middle';
        parts.push(`<g class="edge"><path fill="none" stroke="black" d="M${f(p1[0])},${f(p1[1])}C${f(c1[0])},${f(c1[1])} ${f(c2[0])},${f(c2[1])} ${f(end[0])},${f(end[1])}"/>${arrow(tip[0], tip[1], ux / ul, uy / ul)}${label(lx, ly, groups[0], anchor)}</g>`);

        // To the other states: the group g adds g to the remainder. The edge
        // of each pair that runs clockwise bends inwards, the other outwards.
        states.forEach((to, j) => {
            if (i === j) return;
            const g = (j - i + 3) % 3;
            const inner = g === 1;
            const dx = to.x - from.x, dy = to.y - from.y, len = Math.hypot(dx, dy);
            const ux = dx / len, uy = dy / len;
            const nx = -uy, ny = ux;                    // to the right of the direction of travel
            const bend = inner ? 22 : 34;               // how far the curve's middle leaves the straight line
            const cx = (from.x + to.x) / 2 + nx * bend * 2, cy = (from.y + to.y) / 2 + ny * bend * 2;
            const rFrom = from.accept ? R + 4 : R, rTo = to.accept ? R + 4 : R;
            const sx = cx - from.x, sy = cy - from.y, sl = Math.hypot(sx, sy);
            const ex = cx - to.x, ey = cy - to.y, el = Math.hypot(ex, ey);
            const start = [from.x + sx / sl * rFrom, from.y + sy / sl * rFrom];
            const tip = [to.x + ex / el * (rTo + 1), to.y + ey / el * (rTo + 1)];
            const end = [tip[0] + ex / el * 9, tip[1] + ey / el * 9];
            // A label is wider than high: pushed sideways, it needs more room
            const off = bend + 11 + 26 * Math.abs(nx);
            const mx = (from.x + to.x) / 2 + nx * off, my = (from.y + to.y) / 2 + ny * off;
            parts.push(`<g class="edge"><path fill="none" stroke="black" d="M${f(start[0])},${f(start[1])}Q${f(cx)},${f(cy)} ${f(end[0])},${f(end[1])}"/>${arrow(tip[0], tip[1], -ex / el, -ey / el)}${label(mx, my, groups[g])}</g>`);
        });
    });

    states.forEach((s) => {
        const rings = `<ellipse fill="white" stroke="black" cx="${s.x}" cy="${s.y}" rx="${R}" ry="${R}"/>` +
            (s.accept ? `<ellipse fill="none" stroke="black" cx="${s.x}" cy="${s.y}" rx="${R + 4}" ry="${R + 4}"/>` : '');
        parts.push(`<g class="node ${s.accept ? 'accept' : 'state'}">${rings}<text text-anchor="middle" x="${s.x}" y="${s.y + 4.5}" font-size="17.00">s<tspan dy="0.28em" font-size="0.7em">${s.name}</tspan></text></g>`);
    });
    return `<svg class="automaton" width="570pt" height="380pt" viewBox="0 0 570 380" xmlns="http://www.w3.org/2000/svg"><g class="graph">${parts.join('')}</g></svg>`;
}

export default {
    // variants: dark: Automatron's drawings follow the site's colour scheme,
    // and the site follows the system's.
    actions: {
        // hook: divisible-by-3: shows that DFA where Automatron shows its
        // drawings, in the page's own colours and font.
        async 'divisible-by-3'({ page }) {
            await page.evaluate((svg) => {
                document.getElementById('output_area').innerHTML = svg;
            }, divisibleBy3Svg());
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
