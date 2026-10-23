import { expect, test } from './fixtures';
import { loginAsE2eUser } from './workspace';

// Copy buttons: by default only on the blocks a page marks (_copy), because
// typing is learning. On the class page a teacher makes every code block
// copyable for a group (»Kopierknöpfe: An jedem Codeblock«); its students get a
// button on each block, teachers keep seeing the page as written.
test('a teacher makes every code block copyable for a group', async ({ browser }, testInfo) => {
  const teacher = await (await browser.newContext()).newPage();
  await loginAsE2eUser(teacher, 'e2e-teacher@example.com', testInfo);
  const student = await (await browser.newContext()).newPage();
  await loginAsE2eUser(student, 'e2e-0@example.com', testInfo);
  const setting = async (on: boolean) => {
    const reply = await teacher.request.post('/api/set_group_copy_all_code', { data: { group: 'E2E', on } });
    expect(reply.status()).toBe(200);
  };

  try {
    await setting(false);
    await student.goto('/raytracing');
    const marked = await student.locator('pre.copy-code').count();
    const blocks = await student.locator('pre:not(pre pre)').count();
    expect(blocks).toBeGreaterThan(marked);
    await expect(student.locator('.code-copy')).toHaveCount(marked);

    // The switch on the class page, saved at once
    await teacher.goto('/klasse?group=E2E&tab=fortschritt');
    const menu = teacher.getByRole('combobox', { name: 'Kopierknöpfe' });
    await expect(menu).toHaveValue('marked');
    const saved = teacher.waitForResponse((r) => r.url().endsWith('/api/set_group_copy_all_code'));
    await menu.selectOption('all');
    expect((await saved).status()).toBe(200);
    await teacher.reload();
    await expect(teacher.getByRole('combobox', { name: 'Kopierknöpfe' })).toHaveValue('all');

    await student.reload();
    await expect(student.locator('.code-copy')).toHaveCount(blocks);
    // The teacher's own view stays as written
    await teacher.goto('/raytracing');
    await expect(teacher.locator('.code-copy')).toHaveCount(marked);
  } finally {
    await setting(false);
    await teacher.context().close();
    await student.context().close();
  }
});
