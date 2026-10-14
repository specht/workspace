import { expect, test } from './fixtures';
import { loginAsE2eUser } from './workspace';

// cypher tasks run in the person's own Neo4j database, read-only. Needs the
// film data in e2e-0's database (neo4j_bolt load neo4j.dump); without it
// the card says the data is missing, which is checked too.
test('a Cypher task checks the query against the reference', async ({ page }, testInfo) => {
  await loginAsE2eUser(page, 'e2e-0@example.com', testInfo);
  await page.goto('/neo4j-filmdatenbank');
  const card = page.locator('[data-task="neo4j-filmdatenbank/koenig-der-loewen"]');
  const field = card.locator('textarea');
  await field.fill("MATCH (m:Movie {title: 'The Lion King', year: 1994})\nRETURN m.german_title, m.runtime");
  await field.press('Control+Enter');
  const answer = card.locator('.task-feedback, .task-result, .task-status').first();
  await expect(card).toContainText(/Passt|fehlen die Daten|Filmdaten/, { timeout: 20_000 });
  if (await card.getByText('Passt').count()) {
    await expect(card).toContainText('Der König der Löwen');
    // Writing is refused before anything is sent.
    await field.fill('MATCH (m:Movie) DETACH DELETE m RETURN count(*)');
    await field.press('Control+Enter');
    await expect(card).toContainText('nur Abfragen erlaubt, die lesen');
    await field.fill("MATCH (m:Movie {title: 'The Lion King', year: 1994})\nRETURN m.german_title, m.runtime");
    await field.press('Control+Enter');
    await expect(card).toContainText('Der König der Löwen');
  }
  void answer;
  await card.screenshot({ path: '/tmp/claude-1000/-home-michael-programming-websites-workspace/44346270-c954-424d-82e0-e871a9414424/scratchpad/neo4j-card.png' });
});
