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

// On a page of the section Office the note's demonstration is for the
// program chosen at the top of the page (<!-- language: … --> in
// lehrkraft.md), and changes with the switch.
test('the teacher note shows the ways of the chosen office program', async ({ page }, testInfo) => {
  await loginAsE2eUser(page, 'e2e-teacher@example.com', testInfo);
  await page.goto('/formatting-marks');
  const office = page.locator('.code-language-switch-office');
  await office.locator("[data-set-language='word']").click();
  await page.locator('.teacher-note-toggle').click();
  const note = page.locator('.teacher-note-body');
  await expect(note.getByText('der Knopf ¶ im Register')).toBeVisible();
  await expect(note.getByText('Beim Speichern fragt LibreOffice')).toBeHidden();
  await office.locator("[data-set-language='libreoffice']").click();
  await expect(note.getByText('Beim Speichern fragt LibreOffice')).toBeVisible();
  await expect(note.getByText('der Knopf ¶ im Register')).toBeHidden();
  if (process.env.HS_SHOTS) await note.locator('tr:has(> th:text-is("Vorführen"))').screenshot({ path: `${process.env.HS_SHOTS}/teacher-note-office.png` });
  await office.locator("[data-set-language='word']").click();
});
