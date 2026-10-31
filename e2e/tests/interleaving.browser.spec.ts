import { expect, test } from './fixtures';
import type { Locator, Page } from '@playwright/test';
import { loginAsE2eUser } from './workspace';

// The step-through widget of »Nebenläufigkeit« (interleaving.js) in a task
// card and its check (InterleavingCheck): the order clicked with a button
// per thread goes to the server, which runs it with the widget's rules; the
// order comes back from the stored answer after a reload.

const card = (page: Page, id: string) => page.locator(`.task-card[data-task="nebenlaeufigkeit/${id}"]`);

async function steps(widget: Locator, order: string) {
  for (const t of order.split(' ')) await widget.locator(`.il-step[data-thread='${t}']`).click();
}

// no stored answers: every run starts with an empty order
async function forget(page: Page, ids: string[]) {
  await page.evaluate(async (ids) => {
    for (const id of ids) {
      await new Promise((done) => (window as any).api_call('/api/task/delete', { task: `nebenlaeufigkeit/${id}` }, done, { no_please_wait: true }));
    }
  }, ids);
  await page.reload();
}

test('an order of steps: checked on the server, back after a reload', async ({ page, e2eEmail }, testInfo) => {
  const errors: string[] = [];
  page.on('pageerror', (e) => errors.push(e.message));
  await loginAsE2eUser(page, e2eEmail, testInfo);
  await page.goto('/nebenlaeufigkeit');
  await forget(page, ['verlorene-erhoehung', 'deadlock']);

  const task = card(page, 'verlorene-erhoehung');
  const widget = task.locator('.interleaving');

  await test.step('one thread after the other: no update lost, not what is asked', async () => {
    await steps(widget, 'A A A B B B');
    await expect(widget.locator('.il-say')).toContainText('Alle Threads sind fertig: counter = 2.');
    await expect(widget.locator(".il-step[data-thread='A']")).toBeDisabled();
    await task.locator('.task-submit').click();
    await expect(task.locator('.task-feedback')).toContainText('Wann müsste B den Zähler lesen');
  });

  await test.step('back to the start, both read before either writes: correct', async () => {
    await widget.locator('.il-reset').click();
    await steps(widget, 'A B A B A B');
    await expect(widget.locator('.il-say')).toContainText('counter = 1');
    await task.locator('.task-submit').click();
    await expect(task.locator('.task-feedback')).toContainText('Passt: Am Ende ist counter = 1.');
  });

  await test.step('the stored order comes back after a reload', async () => {
    await page.reload();
    await expect(widget.locator('.il-chip')).toHaveCount(6);
    await expect(widget.locator('.il-say')).toContainText('counter = 1');
  });

  await test.step('two locks taken in opposite order: a deadlock', async () => {
    const dead = card(page, 'deadlock');
    const w = dead.locator('.interleaving');
    await steps(w, 'A B');
    await expect(w.locator('.il-step')).toHaveCount(2);
    await expect(w.locator(".il-step[data-thread='A']")).toBeDisabled();
    await expect(w.locator(".il-step[data-thread='B']")).toBeDisabled();
    await expect(w.locator('.il-say')).toContainText('Das ist ein Deadlock.');
    await dead.locator('.task-submit').click();
    await expect(dead.locator('.task-feedback')).toContainText('Keiner der Threads kann weiter.');
  });

  expect(errors).toEqual([]);
});
