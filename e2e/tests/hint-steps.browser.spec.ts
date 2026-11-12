import { expect, test } from './fixtures';
import { loginAsE2eUser } from './workspace';

// Tips in steps (hint_steps.js): after a tip is opened, the next one waits a
// minute. Meanwhile its title is there, muted, with a ring that empties over
// the minute; then the tip itself is there, and the ring is gone. After the
// last tip there is no ring. The document task of »Was das Auge nicht sieht«
// has three tips.

test('the next tip waits with a ring that empties', async ({ page, e2eEmail }, testInfo) => {
  await page.clock.install();
  await loginAsE2eUser(page, e2eEmail, testInfo);
  await page.goto('/formatting-marks');
  const box = page.locator('#aufgabe-einladung .hint-steps');
  await page.evaluate(() => Object.keys(localStorage).filter((k) => k.startsWith('hint-steps:')).forEach((k) => localStorage.removeItem(k)));
  await page.reload();
  const steps = box.locator('details.hint-step');
  const wait = box.locator('.hint-step-wait');
  const arc = wait.locator('.hint-step-wait-arc');
  const offset = () => arc.evaluate((c) => parseFloat((c as SVGCircleElement).style.strokeDashoffset || '0'));
  const ring = 2 * Math.PI * 6;

  await expect(steps.nth(0)).toBeVisible();
  await expect(steps.nth(1)).toBeHidden();
  await expect(wait).toHaveCount(0);

  await steps.nth(0).locator('summary').click();
  await expect(wait).toBeVisible();
  await expect(wait).toHaveText('Nächster Tipp');
  await expect(steps.nth(1)).toBeHidden();
  // nothing gone yet: the ring is full
  expect(await offset()).toBeLessThan(ring * 0.1);

  await page.clock.fastForward(30_000);
  await expect.poll(offset).toBeGreaterThan(ring * 0.4);
  expect(await offset()).toBeLessThan(ring * 0.6);
  await expect(steps.nth(1)).toBeHidden();

  await page.clock.fastForward(31_000);
  await expect(steps.nth(1)).toBeVisible();
  await expect(wait).toHaveCount(0);

  // the last tip: opened after the second, then no ring follows it
  await steps.nth(1).locator('summary').click();
  // the click is handled (the ring is back) before the clock moves on
  await expect(wait).toBeVisible();
  await page.clock.fastForward(61_000);
  await expect(steps.nth(2)).toBeVisible();
  await steps.nth(2).locator('summary').click();
  await expect(wait).toHaveCount(0);
});
