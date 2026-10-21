import { expect, test } from './fixtures';
import { loginAsE2eUser } from './workspace';

/*
 * The movement of the tree widgets (algo_stepper.js, stepper_motion.js,
 * docs/widgets.md): an arrow is a pointer of its node, and it never comes
 * loose. Steps through every tree widget on /reference/baeume, forward and
 * back, and measures several times during each movement that every visible
 * arrow starts on the centre of its owner (a node or a variable's label);
 * once the movement is over, its tip must sit on the border of a node.
 */

type Loose = string[];

async function measure(widget: import('@playwright/test').Locator, last: boolean): Promise<Loose> {
  return widget.evaluate((el, last) => {
    const svg = el.querySelector('svg.algo-picture')!;
    const centres = [...svg.querySelectorAll<SVGCircleElement>('circle[data-key^="node-"]')]
      .map((c) => [c.cx.baseVal.value, c.cy.baseVal.value]);
    const labels = [...svg.querySelectorAll<SVGRectElement>('rect[data-key^="var-"]')]
      .map((r) => [r.x.baseVal.value + r.width.baseVal.value / 2, r.y.baseVal.value + r.height.baseVal.value / 2]);
    const owners = centres.concat(labels);
    const near = (list: number[][], x: number, y: number) => Math.min(...list.map(([cx, cy]) => Math.hypot(x - cx, y - cy)));
    const loose: string[] = [];
    for (const line of svg.querySelectorAll<SVGElement>('[data-ends]')) {
      if (line.getAttribute('visibility') === 'hidden') continue;
      const n = (line.getAttribute('d') || '').match(/-?[\d.]+/g)!.map(Number);
      const start = near(owners, n[0], n[1]);
      const tip = near(centres, n[n.length - 2], n[n.length - 1]);
      const inset = Number(line.dataset.inset || 0);
      if (start > 0.5 || (last && Math.abs(tip - inset) > 1)) loose.push(`${line.dataset.key}: ${start.toFixed(1)}/${tip.toFixed(1)}`);
    }
    return loose;
  }, last);
}

for (const index of [0, 1, 2, 3]) {
  test(`tree widget ${index} on /reference/baeume: no arrow comes loose while it moves`, async ({ page, e2eEmail }, testInfo) => {
    test.setTimeout(300_000);
    await loginAsE2eUser(page, e2eEmail, testInfo);
    const errors: string[] = [];
    page.on('pageerror', (error) => errors.push(String(error)));
    await page.goto('/reference/baeume');
    const widget = page.locator('.algo-stepper').nth(index);
    await widget.scrollIntoViewIfNeeded();
    const total = Number((await widget.locator('.algo-count').textContent())!.match(/von (\d+)/)![1]);
    const loose: string[] = [];
    const step = async (direction: 'next' | 'prev', at: number) => {
      await widget.locator(`[data-go=${direction}]`).click();
      for (const wait of [40, 90, 120, 120, 120]) {
        await page.waitForTimeout(wait);
        for (const problem of await measure(widget, false)) loose.push(`step ${at} (${direction}): ${problem}`);
      }
      // once the movement is over (data-moving, stepper_motion.js); a step
      // with pulses one after another takes longer
      await expect(widget.locator('svg.algo-picture')).not.toHaveAttribute('data-moving', '', { timeout: 10_000 });
      for (const problem of await measure(widget, true)) loose.push(`step ${at} (${direction}), at rest: ${problem}`);
    };
    for (let at = 1; at <= total; at++) await step('next', at);
    for (let at = total - 1; at >= 0; at--) await step('prev', at);
    expect(loose.slice(0, 10)).toEqual([]);
    expect(errors).toEqual([]);
  });
}
