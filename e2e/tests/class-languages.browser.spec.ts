import { expect, test } from './fixtures';
import { loginAsE2eUser } from './workspace';

// »Sprachen der Klasse« (class_settings_html, class_languages.js): a teacher
// ranks the languages of a group; each activity shows the first of them it
// offers (CodeLanguages.resolve). Added with a tap, moved and removed by the
// keyboard, saved at once, never empty.
test('a teacher ranks the languages of a group, and the group sees them', async ({ browser }, testInfo) => {
  test.setTimeout(120_000);
  const errors: string[] = [];
  const teacher = await (await browser.newContext()).newPage();
  teacher.on('pageerror', (e) => errors.push(e.message));
  await loginAsE2eUser(teacher, 'e2e-teacher@example.com', testInfo);
  await teacher.goto('/klasse?group=E2E&tab=einstellungen');
  const before = JSON.parse((await teacher.locator('.class-languages').getAttribute('data-languages'))!).ranked as string[];
  const ranked = () => teacher.locator('.cl-list .cl-item').evaluateAll((items) => items.map((i) => (i as HTMLElement).dataset.lang));
  const saved = () => teacher.waitForResponse((r) => r.url().includes('/api/set_group_code_languages'));
  try {
    expect((await (await teacher.request.post('/api/set_group_code_languages', { data: { group: 'E2E', languages: ['ruby'] } })).json()).yay).toBe('sure');
    await teacher.reload();
    expect(await ranked()).toEqual(['ruby']);
    // the counts come from the activities
    await expect(teacher.locator(".cl-item[data-lang='python'] .cl-count")).toContainText(/in \d+ Aktivitäten/);

    // the last language cannot go
    const ruby = teacher.locator(".cl-item[data-lang='ruby']");
    await expect(ruby.locator('.cl-remove')).toHaveAttribute('aria-disabled', 'true');
    await ruby.focus();
    await teacher.keyboard.press('Delete');
    await expect(teacher.locator('.cl-say')).toHaveText('Die Klasse braucht mindestens eine Sprache.');
    expect(await ranked()).toEqual(['ruby']);

    // a tap adds JavaScript at the end, the keyboard moves it up
    let save = saved();
    await teacher.locator(".cl-more .cl-item[data-lang='javascript']").click();
    await save;
    expect(await ranked()).toEqual(['ruby', 'javascript']);
    save = saved();
    await teacher.locator(".cl-item[data-lang='javascript']").focus();
    await teacher.keyboard.press('ArrowUp');
    await expect(teacher.locator('.cl-say')).toHaveText('JavaScript ist jetzt an Stelle 1 von 2.');
    await save;
    expect(await ranked()).toEqual(['javascript', 'ruby']);
    // Enter on a language below adds it; Delete removes one from the list
    save = saved();
    await teacher.locator(".cl-more .cl-item[data-lang='python']").focus();
    await teacher.keyboard.press('Enter');
    await save;
    save = saved();
    await teacher.locator(".cl-list .cl-item[data-lang='ruby']").focus();
    await teacher.keyboard.press('Delete');
    await save;
    expect(await ranked()).toEqual(['javascript', 'python']);
    await expect(teacher.locator('.cl-preview')).toContainText('JavaScript');

    // kept after a reload
    await teacher.reload();
    expect(await ranked()).toEqual(['javascript', 'python']);

    // a student of the group: a Kata page offering JavaScript shows it, a
    // page with only Ruby and Python shows Python; unless the student chose
    // a language of their own on a page, which wins
    const student = await (await browser.newContext()).newPage();
    await loginAsE2eUser(student, 'e2e-auth-student-0@example.com', testInfo);
    const shown = async (slug: string) => {
      await student.goto('/' + slug);
      return student.locator('[data-code-language]').first().getAttribute('data-code-language');
    };
    const kata = await shown('kata-loops');
    const page = await shown('rle');
    test.skip(kata !== 'javascript' && page === kata, 'the student has a language of their own');
    expect(kata).toBe('javascript');
    expect(page).toBe('python');
    await student.context().close();
  } finally {
    await teacher.request.post('/api/set_group_code_languages', { data: { group: 'E2E', languages: before } });
  }
  expect(errors).toEqual([]);
});

// The API takes only known languages, each once, at least one
test('the ranking refuses what is not a ranking', async ({ page }, testInfo) => {
  await loginAsE2eUser(page, 'e2e-teacher@example.com', testInfo);
  for (const languages of [[], ['cobol'], ['ruby', 'ruby'], 'ruby']) {
    const r = await page.request.post('/api/set_group_code_languages', { data: { group: 'E2E', languages } });
    expect(r.ok(), JSON.stringify(languages)).toBe(false);
  }
});
