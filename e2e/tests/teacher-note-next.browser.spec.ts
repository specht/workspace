import { expect, test } from './fixtures';
import { loginAsE2eUser } from './workspace';

// Below the teacher note: where a class can go next, the activities that
// build on this one (PageKinds.builds_on), and the way to them on /lehrplan.
// HS_SHOTS=dir keeps a screenshot of both.

test('the teacher note says where a class can go next', async ({ page }, testInfo) => {
  await loginAsE2eUser(page, 'e2e-teacher@example.com', testInfo);
  await page.goto('/tcpip');
  await page.locator('.teacher-note-toggle').click();
  const next = page.locator('.teacher-note-next');
  await expect(next).toBeVisible();
  await expect(next).toContainText('Wie es weitergehen kann:');
  await expect(next.locator("a[href='/webserver']")).toBeVisible();
  if (process.env.HS_SHOTS) await next.screenshot({ path: `${process.env.HS_SHOTS}/teacher-note-next.png` });
  await next.getByRole('link', { name: 'im Lehrplan zeigen' }).click();
  await expect(page).toHaveURL(/\/lehrplan\?zeige=tcpip/);
  await expect(page.locator(".cg-node.cg-hit[data-slug='tcpip']")).toHaveCount(1);
  // the way onward: what builds on it is highlighted too
  await expect(page.locator(".cg-node.cg-hit[data-slug='webserver']")).toHaveCount(1);
  if (process.env.HS_SHOTS) { await page.waitForTimeout(800); await page.screenshot({ path: `${process.env.HS_SHOTS}/lehrplan-zeige.png` }); }
});
