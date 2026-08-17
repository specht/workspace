import type { Browser, BrowserContext, Page, TestInfo } from '@playwright/test';
import { expect, test } from './fixtures';
import { E2E_LOGIN_CODE, loginAsE2eUser } from './workspace';

const WRONG_LOGIN_CODE = '654321';

function baseUrl(testInfo: TestInfo): string {
  const value = testInfo.project.use.baseURL;
  if (typeof value !== 'string')
    throw new Error('E2E project must define baseURL');
  return value;
}

async function requestLoginCode(page: Page, email: string): Promise<string> {
  await page.goto('/login');
  const emailInput = page.locator('#ti_email');
  await emailInput.fill(email);
  await emailInput.press('Tab');
  await expect(page.locator('#bu_submit_email')).toBeEnabled();
  await page.locator('#bu_submit_email').click();
  await expect(page.locator('#ti_code')).toBeVisible();

  const tag = await page.evaluate(() =>
    (window as typeof window & {tag: string}).tag,
  );
  expect(tag).toMatch(/^[0-9a-z]{12}$/);
  return tag;
}

async function completeLogin(
  page: Page,
  tag: string,
  code: string,
): Promise<{status: number; body: Record<string, unknown>}> {
  return await page.evaluate(async ({loginTag, loginCode}) => {
    const response = await fetch('/api/complete_login', {
      method: 'POST',
      credentials: 'include',
      headers: {'content-type': 'application/json'},
      body: JSON.stringify({tag: loginTag, code: loginCode}),
    });

    return {
      status: response.status,
      body: await response.json() as Record<string, unknown>,
    };
  }, {loginTag: tag, loginCode: code});
}

async function requestLoginDirect(page: Page, email: string) {
  return await page.evaluate(async loginEmail => {
    const response = await fetch('/api/request_login', {
      method: 'POST',
      credentials: 'include',
      headers: {'content-type': 'application/json'},
      body: JSON.stringify({email: loginEmail}),
    });

    return {
      status: response.status,
      body: await response.json() as Record<string, unknown>,
    };
  }, email);
}

function authTeacherEmail(e2eEmail: string): string {
  const match = e2eEmail.match(/^e2e-(\d+)@example\.com$/);
  if (!match)
    throw new Error(`Unexpected E2E email: ${e2eEmail}`);
  return `e2e-auth-teacher-${match[1]}@example.com`;
}

async function teacherAdminContext(
  browser: Browser,
  teacherEmail: string,
  testInfo: TestInfo,
): Promise<{context: BrowserContext; page: Page}> {
  const context = await browser.newContext({baseURL: baseUrl(testInfo)});
  const page = await context.newPage();
  await loginAsE2eUser(page, teacherEmail, testInfo);
  await page.goto('/admin');
  await expect(page.getByRole('heading', {name: 'Server', exact: true})).toBeVisible();
  return {context, page};
}

test('login codes are bounded, one-time and visible to the teacher', async ({
  browser,
  page,
  e2eEmail,
}, testInfo) => {
  const firstTag = await requestLoginCode(page, e2eEmail);
  const {context: teacherContext, page: teacherPage} = await teacherAdminContext(
    browser,
    authTeacherEmail(e2eEmail),
    testInfo,
  );

  const codeSection = teacherPage.locator('#login_codes_here');
  const studentCodeRow = () =>
    codeSection.locator('tbody tr').filter({hasText: e2eEmail});

  try {
    await expect(codeSection).toBeVisible();
    await expect(studentCodeRow()).toContainText(E2E_LOGIN_CODE);

    const codeInput = page.locator('#ti_code');
    await codeInput.fill(E2E_LOGIN_CODE);
    await codeInput.press('Tab');
    const completeResponse = page.waitForResponse(response =>
      response.url().endsWith('/api/complete_login')
      && response.request().method() === 'POST',
    );
    await page.locator('#bu_submit_code').click();
    expect((await completeResponse).status()).toBe(200);
    await page.waitForURL(url => url.pathname === '/');
    await expect(page.locator('#bu_launch')).toBeVisible();
    await expect(studentCodeRow()).toHaveCount(0);

    const replayContext = await browser.newContext({baseURL: baseUrl(testInfo)});
    try {
      const replay = await replayContext.request.post('/api/complete_login', {
        data: {tag: firstTag, code: E2E_LOGIN_CODE},
        headers: {origin: new URL(baseUrl(testInfo)).origin},
      });
      expect(replay.status()).toBe(401);

      const forgedOrigin = new URL(baseUrl(testInfo));
      forgedOrigin.hostname = `student.${forgedOrigin.hostname}`;
      const forgedPost = await replayContext.request.post('/api/request_login', {
        data: {email: e2eEmail},
        headers: {origin: forgedOrigin.origin},
      });
      expect(forgedPost.status()).toBe(403);

      const forgedWebSocket = await replayContext.request.get('/ws', {
        headers: {origin: forgedOrigin.origin},
      });
      expect(forgedWebSocket.status()).toBe(403);
    } finally {
      await replayContext.close();
    }

    const allCookies = await page.context().cookies();
    const mainCookies = await page.context().cookies(baseUrl(testInfo));
    const sessionCookies = allCookies.filter(cookie => cookie.name === 'hs_sid');
    expect(sessionCookies).toHaveLength(1);
    const sessionCookie = sessionCookies[0];
    const serverCookie = mainCookies.find(cookie => cookie.name === 'hs_server_sid');
    expect(sessionCookie).toBeDefined();
    expect(sessionCookie?.httpOnly).toBe(true);
    expect(sessionCookie?.sameSite).toBe('Lax');
    expect(sessionCookie?.domain).toBe(new URL(baseUrl(testInfo)).hostname);
    expect(serverCookie).toBeDefined();
    expect(serverCookie?.httpOnly).toBe(true);
    expect(serverCookie?.sameSite).toBe('Lax');

    const sibling = new URL(baseUrl(testInfo));
    sibling.hostname = `student.${sibling.hostname}`;
    const siblingCookies = await page.context().cookies(sibling.toString());
    expect(siblingCookies.some(cookie => cookie.name === 'hs_sid')).toBe(false);
    expect(siblingCookies.some(cookie => cookie.name === 'hs_server_sid')).toBe(true);

    await page.goto('/logout');
    await page.waitForURL(url => url.pathname === '/');
    await expect(page.getByRole('link', {name: 'Anmelden'})).toBeVisible();
    const loggedOutCookies = await page.context().cookies(baseUrl(testInfo));
    expect(loggedOutCookies.some(cookie => cookie.name === 'hs_sid')).toBe(false);
    expect(
      loggedOutCookies.some(cookie => cookie.name === 'hs_server_sid'),
    ).toBe(false);

    // A successful login clears the creation throttle, so a fresh browser can
    // request another code immediately. Five wrong guesses then lock that code
    // and the request throttle prevents instantly obtaining another five tries.
    const lockoutContext = await browser.newContext({baseURL: baseUrl(testInfo)});
    const lockoutPage = await lockoutContext.newPage();
    try {
      const lockoutTag = await requestLoginCode(lockoutPage, e2eEmail);
      await expect(codeSection).toBeVisible();
      await expect(studentCodeRow()).toContainText(E2E_LOGIN_CODE);

      for (let attempt = 1; attempt <= 5; attempt++) {
        const result = await completeLogin(
          lockoutPage,
          lockoutTag,
          WRONG_LOGIN_CODE,
        );
        expect(result.status).toBe(401);
        expect(result.body.error).toBe('invalid_login_code');
      }

      await expect(studentCodeRow()).toHaveCount(0);

      const lockedOut = await completeLogin(
        lockoutPage,
        lockoutTag,
        E2E_LOGIN_CODE,
      );
      expect(lockedOut.status).toBe(401);

      const throttled = await requestLoginDirect(lockoutPage, e2eEmail);
      expect(throttled.status).toBe(429);
      expect(throttled.body.error).toBe('login_request_rate_limited');
    } finally {
      await lockoutContext.close();
    }
  } finally {
    await teacherContext.close();
  }
});
