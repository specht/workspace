import { expect, test } from './fixtures';

// The text adventure's page starts with the game to play
// (textadventure_demo.js, the classes of the solution in JavaScript and the
// night in the school): typed commands get the game's answers, the dark
// cellar needs the torch, and the way out ends the game. No login needed.

const WAY = ['gehe westen', 'gehe unten', 'gehe unten', 'gehe westen', 'nimm taschenlampe', 'gehe osten',
  'gehe unten', 'nimm lehrerschlüssel', 'gehe oben', 'gehe oben', 'gehe oben', 'benutze lehrerschlüssel',
  'gehe norden', 'nimm haustürschlüssel', 'gehe süden', 'gehe unten', 'gehe unten', 'benutze haustürschlüssel', 'gehe süden'];

test('the night in the school can be played on the page', async ({ page }) => {
  const errors: string[] = [];
  page.on('pageerror', (e) => errors.push(e.message));
  await page.goto('/textadventure');
  const demo = page.locator('.adventure-demo');
  const input = demo.locator('.adventure-input');
  await expect(demo.locator('.adventure-screen')).toContainText('Raum 204');
  const type = async (line: string) => {
    await input.fill(line);
    await input.press('Enter');
  };
  // without light the cellar shows nothing
  for (const line of ['gehe westen', 'gehe unten', 'gehe unten', 'gehe unten', 'nimm lehrerschlüssel']) await type(line);
  await expect(demo.locator('.adventure-answer').last()).toHaveText('Im Dunkeln findest du nichts.');
  await demo.locator('.adventure-restart').click();
  for (const line of WAY) await type(line);
  await expect(demo.locator('.adventure-screen')).toContainText('Du bist draußen. Geschafft!');
  await expect(demo.locator('.adventure-end')).toBeVisible();
  await expect(input).toBeDisabled();
  expect(errors).toEqual([]);
});
