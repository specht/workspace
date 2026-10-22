import type { Page, TestInfo } from '@playwright/test';
import { expect, test } from './fixtures';
import { loginAsE2eUser } from './workspace';

// The profile lists the browsers an account is logged in with, each with a
// coarse device label from the User-Agent at the login, and ends one of
// them (include/login_sessions.rb).

const EDGE_WINDOWS =
  'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/141.0.0.0 Safari/537.36 Edg/141.0.0.0';
const FIREFOX_LINUX =
  'Mozilla/5.0 (X11; Linux x86_64; rv:144.0) Gecko/20100101 Firefox/144.0';

function baseUrl(testInfo: TestInfo): string {
  const value = testInfo.project.use.baseURL;
  if (typeof value !== 'string')
    throw new Error('E2E project must define baseURL');
  return value;
}

async function endSession(page: Page, id: string): Promise<number> {
  return page.evaluate(async sessionId => {
    const response = await fetch('/api/end_login_session', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ id: sessionId }),
    });
    return response.status;
  }, id);
}

test('the profile lists the browsers of an account and ends one of them', async ({
  browser,
  e2eEmail,
}, testInfo) => {
  const first = await browser.newContext({ baseURL: baseUrl(testInfo), userAgent: EDGE_WINDOWS });
  const second = await browser.newContext({ baseURL: baseUrl(testInfo), userAgent: FIREFOX_LINUX });
  try {
    const a = await first.newPage();
    const b = await second.newPage();
    await loginAsE2eUser(a, e2eEmail, testInfo);

    // Earlier logins of this account (other tests) go first, so that the
    // list holds exactly the two browsers of this test.
    await a.goto('/profil#account-appearance');
    const elsewhere = a.locator('#bu_sign_out_elsewhere');
    if (await elsewhere.count() > 0) {
      await elsewhere.click();
      await expect(elsewhere).toHaveCount(0);
    }

    await loginAsE2eUser(b, e2eEmail, testInfo);

    // The same address with its #: reload, or the page stays as it was.
    await a.reload();
    await expect(a.getByRole('heading', { name: 'Wo du angemeldet bist' })).toBeVisible();
    const rows = a.locator('#login_sessions .login-session');
    await expect(rows).toHaveCount(2);

    const here = rows.first();
    await expect(here).toHaveClass(/current/);
    await expect(here).toContainText('Edge auf Windows');
    await expect(here).toContainText('dieser Browser');
    await expect(here).toContainText('angemeldet heute');
    await expect(here.locator('a[href="/logout"]')).toBeVisible();
    await expect(here.locator('.bu-end-session')).toHaveCount(0);

    const other = rows.nth(1);
    await expect(other).toContainText('Firefox auf Linux');
    await expect(other).toContainText('angemeldet heute');
    await expect(other.locator('.bi-pc-display')).toHaveCount(1);
    await expect(a.locator('.other-sessions-count')).toHaveText('1');

    // The page names sessions by a hash, never by the sid in the cookie.
    const html = await a.content();
    for (const cookie of await first.cookies()) {
      if (cookie.value.length >= 16)
        expect(html).not.toContain(cookie.value);
    }

    // This browser ends with /logout, not here; an unknown id is refused.
    const hereId = await here.getAttribute('data-id');
    expect(hereId).toMatch(/^[0-9a-f]{32}$/);
    expect(await endSession(a, hereId!)).toBe(400);
    expect(await endSession(a, '0'.repeat(32))).toBe(404);
    expect(await endSession(a, 'not-an-id')).toBe(400);

    await other.locator('.bu-end-session').click();
    await expect(rows).toHaveCount(1);
    await expect(a.locator('#bu_sign_out_elsewhere')).toHaveCount(0);

    // The second browser is logged out; the first is not.
    await b.goto('/');
    await expect(b.getByRole('link', { name: 'Anmelden', exact: true })).toBeVisible();
    await expect(b.locator('#bu_launch')).toBeHidden();
    await a.goto('/');
    await expect(a.locator('#bu_launch')).toBeVisible();
  } finally {
    await first.close();
    await second.close();
  }
});
