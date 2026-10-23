import { expect, test } from './fixtures';
import { loginAsE2eUser } from './workspace';

// Copy buttons: by default on every code block, for everyone. On the class
// page a teacher can choose »Kopierknöpfe: Wo sinnvoll« for a group; its
// students then get buttons only on the blocks a page marks (_copy) and type
// the rest. Teachers and visitors keep the default.
test('a group can go back to copy buttons only where the page marks them', async ({ browser }, testInfo) => {
  const teacher = await (await browser.newContext()).newPage();
  await loginAsE2eUser(teacher, 'e2e-teacher@example.com', testInfo);
  const student = await (await browser.newContext()).newPage();
  await loginAsE2eUser(student, 'e2e-0@example.com', testInfo);
  const visitor = await (await browser.newContext()).newPage();
  const setting = async (on: boolean) => {
    const reply = await teacher.request.post('/api/set_group_copy_all_code', { data: { group: 'E2E', on } });
    expect(reply.status()).toBe(200);
  };

  try {
    await visitor.goto('/raytracing');
    const marked = await visitor.locator('pre.copy-code').count();
    const blocks = await visitor.locator('pre:not(pre pre)').count();
    expect(blocks).toBeGreaterThan(marked);
    // The default: every block, also for visitors and teachers
    await expect(visitor.locator('.code-copy')).toHaveCount(blocks);
    await teacher.goto('/raytracing');
    await expect(teacher.locator('.code-copy')).toHaveCount(blocks);

    // »Wo sinnvoll« on the class page, saved at once
    await setting(true);
    await teacher.goto('/klasse?group=E2E&tab=fortschritt');
    const menu = teacher.getByRole('combobox', { name: 'Kopierknöpfe' });
    await expect(menu).toHaveValue('all');
    const saved = teacher.waitForResponse((r) => r.url().endsWith('/api/set_group_copy_all_code'));
    await menu.selectOption('marked');
    expect((await saved).status()).toBe(200);
    await teacher.reload();
    await expect(teacher.getByRole('combobox', { name: 'Kopierknöpfe' })).toHaveValue('marked');

    await student.goto('/raytracing');
    await expect(student.locator('.code-copy')).toHaveCount(marked);
    // Teachers and visitors keep every button
    await teacher.goto('/raytracing');
    await expect(teacher.locator('.code-copy')).toHaveCount(blocks);
    await visitor.reload();
    await expect(visitor.locator('.code-copy')).toHaveCount(blocks);
  } finally {
    await setting(true);
    await teacher.context().close();
    await student.context().close();
    await visitor.context().close();
  }
});
