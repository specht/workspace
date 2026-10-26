import { expect, test } from './fixtures';
import { loginAsE2eUser } from './workspace';

// The class page has three tabs with one job each; the chosen tab stays in
// the address. »Lernende« shows the server's cards and each Workspace live.
test('the class page: tabs, menu and the Workspaces of the group', async ({ page }, testInfo) => {
  test.setTimeout(90_000);
  const errors: string[] = [];
  page.on('pageerror', (e) => errors.push(e.message));
  await loginAsE2eUser(page, 'e2e-teacher@example.com', testInfo);
  await page.goto('/klasse?group=E2E&activity=bif');

  await expect(page.getByRole('tab', { name: 'Fortschritt' })).toHaveAttribute('aria-selected', 'true');
  await expect(page.locator('table.class-board')).toBeVisible();
  await page.getByRole('button', { name: 'Jetzt nachsehen' }).click();
  await expect(page.locator('.class-live-menu .class-live-check').first()).toBeVisible();
  await page.keyboard.press('Escape');

  await page.getByRole('tab', { name: /Lernende/ }).click();
  await expect(page).toHaveURL(/tab=lernende/);
  await expect(page.locator('#class-panel-fortschritt')).toBeHidden();
  await expect(page.locator('.class-people-table')).toBeVisible();
  // The server's CPU and RAM in one row, from the start; the values follow.
  await expect(page.locator('.class-server-load')).toBeVisible();
  await expect(page.locator('.class-server-load p').first()).toContainText('% von', { timeout: 20_000 });

  await page.reload();
  await expect(page.getByRole('tab', { name: /Lernende/ })).toHaveAttribute('aria-selected', 'true');
  await page.getByRole('tab', { name: /Lernende/ }).press('ArrowRight');
  await expect(page.getByRole('tab', { name: 'Startseite' })).toHaveAttribute('aria-selected', 'true');
  await expect(page.locator('.class-pins')).toBeVisible();
  expect(errors).toEqual([]);
});

// The class page opens where the teacher left it: the last group, and in
// each group the last activity.
test('the class page remembers the group and each group\'s activity', async ({ page }, testInfo) => {
  await loginAsE2eUser(page, 'e2e-teacher@example.com', testInfo);
  const testGroup = 'Testgruppe (E2E Teacher)';
  await page.goto('/klasse?group=E2E&activity=bif');
  await page.goto('/klasse?group=' + encodeURIComponent(testGroup) + '&activity=tcpip');
  await page.goto('/klasse');
  await expect(page.locator(".class-board-select select[name='group']")).toHaveValue(testGroup);
  await expect(page.locator('.class-activity-select select')).toHaveValue('tcpip');
  await page.locator(".class-board-select select[name='group']").selectOption('E2E');
  await expect(page.locator('.class-activity-select select')).toHaveValue('bif');
  await page.goto('/klasse');
  await expect(page.locator(".class-board-select select[name='group']")).toHaveValue('E2E');
});

test('toolbar buttons sit on one line', async ({ page }, testInfo) => {
  await loginAsE2eUser(page, 'e2e-teacher@example.com', testInfo);
  await page.goto('/klasse?group=E2E&tab=fortschritt&activity=sandbox-datenstrukturen');
  const live = await page.getByRole('button', { name: 'Jetzt nachsehen' }).boundingBox();
  const template = await page.getByRole('button', { name: 'Vorlage aktuell?' }).boundingBox();
  expect(live && template).toBeTruthy();
  expect(Math.abs((live!.y + live!.height / 2) - (template!.y + template!.height / 2))).toBeLessThan(1);
});

// Every teacher has exactly one test account in a test group of its own,
// and sees no other teacher's test group; it can reset its test account.
test('each teacher has its own test group and resets its test account', async ({ page }, testInfo) => {
  test.setTimeout(120_000);
  // A teacher with * only: the class page shows just its test group
  await loginAsE2eUser(page, 'e2e-materials-teacher@example.com', testInfo);
  await page.goto('/klasse?tab=lernende');
  const groups = page.locator(".class-board-select select[name='group'] option");
  await expect(groups).toHaveCount(1);
  await expect(groups.first()).toHaveText(/^Testgruppe \(E2E Materials Teacher\)/);
  await expect(page.locator('.class-people-table')).toContainText('Testkonto (E2E Materials Teacher)');

  // A test result, then the reset: the result is gone
  const student = await (await page.context().browser()!.newContext()).newPage();
  await loginAsE2eUser(student, 'e2e-materials-teacher+test@example.com', testInfo);
  const saved = await (await student.request.post('/api/task/check', { data: { task: 'tcpip/traceroute-erklaeren', answer: 'Jeder Router auf dem Weg zieht von der TTL des Pakets eins ab, und bei null verwirft er es und schickt eine Meldung zurück; so findet traceroute einen Router nach dem anderen.' } })).json();
  expect(saved.status).toBe('sent');
  page.once('dialog', (d) => d.accept());
  await page.getByRole('button', { name: 'Testkonto zurücksetzen' }).click();
  await expect(page.locator('.class-test-reset-note')).toContainText('Zurückgesetzt', { timeout: 60_000 });
  const states = await (await student.request.post('/api/task/states', { data: { activity: 'tcpip' } })).json();
  expect(states.states['traceroute-erklaeren']).toBeUndefined();

  // A group administrator: its group and its own test group, no other
  const teacher = await (await page.context().browser()!.newContext()).newPage();
  await loginAsE2eUser(teacher, 'e2e-teacher@example.com', testInfo);
  await teacher.goto('/klasse');
  const names = await teacher.locator(".class-board-select select[name='group'] option").allTextContents();
  expect(names.filter((n) => n.startsWith('Testgruppe'))).toEqual(['Testgruppe (E2E Teacher) (1)']);
});

// Activities are found by typing: the board's activity and the one to pin
// are search fields (searchable_select), and a choice in either works.
test('the class page finds activities by typing', async ({ page }, testInfo) => {
  await loginAsE2eUser(page, 'e2e-teacher@example.com', testInfo);
  await page.goto('/klasse?group=E2E&activity=bif&tab=fortschritt');
  const board = page.locator('.class-activity-select input[role=combobox]');
  await board.click();
  await board.fill('routen');
  await page.locator('.class-activity-select .suggest-menu .dropdown-item').first().click();
  await expect(page).toHaveURL(/activity=routenplaner/);
  await expect(page.locator('.class-activity-select input[role=combobox]')).toHaveValue(/Routenplaner/);

  await page.getByRole('tab', { name: 'Startseite' }).click();
  const pin = page.locator('.class-pins input[role=combobox]');
  await pin.click();
  // by its address, which every option can be found by
  await pin.fill('sql-joins');
  await page.locator('.class-pins .suggest-menu .dropdown-item').first().click();
  const pinned = page.locator(".class-pin:has(a[href='/sql-joins'])");
  await expect(pinned).toBeVisible();
  await pinned.locator('.class-unpin').click();
  await expect(pinned).toHaveCount(0);
});
