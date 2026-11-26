import { expect, test } from './fixtures';
import { loginAsE2eUser } from './workspace';

// A reference in a panel from the right (side_panel.rb, side_panel.js),
// »Auf einen Blick« in the Routenplaner: the tab on the right edge and the
// links in the task cards open it; a meeting point, which dims what comes
// after it, leaves the panel alone; on a wide screen the page moves aside.
test('the reference panel opens from the tab and from a link, undimmed by a meeting point', async ({ page, browser, e2eEmail }, testInfo) => {
  test.setTimeout(120_000);
  const errors: string[] = [];
  page.on('pageerror', (e) => errors.push(e.message));
  await loginAsE2eUser(page, e2eEmail, testInfo);

  // the teacher sets a meeting point early on the page
  const teacherContext = await browser.newContext({ baseURL: testInfo.project.use.baseURL });
  const teacher = await teacherContext.newPage();
  await loginAsE2eUser(teacher, 'e2e-teacher@example.com', testInfo);
  await teacher.goto('/klasse?group=E2E&tab=fortschritt&activity=routenplaner');
  const choice = teacher.locator('.class-meeting-choice');
  await choice.selectOption('');
  await teacher.waitForLoadState('load');
  await choice.selectOption('lesen');
  await expect(teacher.locator('.class-meeting-status')).toContainText('Treffpunkt nach Aufgabe');

  try {
    await page.setViewportSize({ width: 1400, height: 900 });
    await page.emulateMedia({ colorScheme: 'light' });
    await page.evaluate(() => { try { localStorage.clear(); } catch (e) { /* none */ } }).catch(() => {});
    await page.goto('/routenplaner');
    await expect(page.locator('aside.meeting-point')).toBeVisible({ timeout: 10_000 });
    await expect(page.locator('.meeting-later').first()).toBeAttached();

    const panel = page.locator('aside.side-panel#auf-einen-blick');
    const tab = page.locator('.side-panel-tab');
    await expect(panel).toBeHidden();
    await expect(tab).toBeVisible();
    // out of the chapters, so never dimmed
    await expect(page.locator('.side-panel-box.meeting-later, .meeting-later .side-panel-box')).toHaveCount(0);

    // the tab opens it, the page moves aside, both readable
    await tab.click();
    await expect(panel).toBeVisible();
    await expect(panel).toHaveCSS('opacity', '1');
    await expect(panel.locator('.side-panel-title')).toHaveText(/Auf einen Blick/);
    // the parts of the page's language only: one listing of the graph
    await expect(panel.locator('.code-language:visible').first()).toBeVisible();
    const pad = await page.evaluate(() => getComputedStyle(document.body).paddingRight);
    expect(parseFloat(pad)).toBeGreaterThan(400);
    await page.screenshot({ path: testInfo.outputPath('side-panel-wide-light.png') });

    // closed, it stays closed; a link in a task opens it again
    await panel.locator('.btn-close').click();
    await expect(panel).toBeHidden();
    await expect(tab).toBeVisible();
    const link = page.locator('.task-card:visible a[href="#auf-einen-blick"]').first();
    await link.scrollIntoViewIfNeeded();
    const y = await page.evaluate(() => window.scrollY);
    await link.click();
    await expect(panel).toBeVisible();
    // no jump to the end of the page
    expect(Math.abs((await page.evaluate(() => window.scrollY)) - y)).toBeLessThan(50);

    // open on a reload, on a wide screen
    await page.reload();
    await expect(panel).toBeVisible();
    await panel.locator('.btn-close').click();
    await expect(panel).toBeHidden();

    // on a phone, dark: the tab low on the right, the panel across the screen
    await page.setViewportSize({ width: 390, height: 800 });
    await page.emulateMedia({ colorScheme: 'dark' });
    await page.goto('/routenplaner');
    await expect(tab).toBeVisible();
    await tab.click();
    await expect(panel).toBeVisible();
    const box = await panel.boundingBox();
    expect(box!.width).toBeGreaterThan(385);
    await page.screenshot({ path: testInfo.outputPath('side-panel-phone-dark.png') });
    await page.keyboard.press('Escape');
    await expect(panel).toBeHidden();
  } finally {
    await choice.selectOption('');
    await teacherContext.close();
  }
  expect(errors).toEqual([]);
});
