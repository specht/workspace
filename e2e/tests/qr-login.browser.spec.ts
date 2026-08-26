import { expect, test } from './fixtures';
import type { BrowserContext, TestInfo } from '@playwright/test';
import { loginAsE2eUser } from './workspace';

function baseUrl(testInfo: TestInfo): string {
  const value = testInfo.project.use.baseURL;
  if (typeof value !== 'string') {
    throw new Error('Playwright baseURL must be configured');
  }
  return value;
}

test('a logged-in phone can approve a QR login on another computer', async ({
  browser,
  e2eEmail,
}, testInfo) => {
  const phoneContext = await browser.newContext({ baseURL: baseUrl(testInfo) });
  const desktopContext = await browser.newContext({ baseURL: baseUrl(testInfo) });
  const anonymousContext = await browser.newContext({ baseURL: baseUrl(testInfo) });

  try {
    const phone = await phoneContext.newPage();
    const desktop = await desktopContext.newPage();
    const anonymous = await anonymousContext.newPage();

    await loginAsE2eUser(phone, e2eEmail, testInfo);

    await desktop.goto('/login');
    const qrPanel = desktop.locator('#qr_login_panel');
    const qrImage = desktop.locator('#qr_code');
    await expect(qrPanel).toBeHidden();
    expect(await desktop.evaluate(() => (
      window as typeof window & { qr_login_request?: unknown }
    ).qr_login_request ?? null)).toBeNull();

    await desktop.locator('#bu_qr_login').click();
    await expect(qrPanel).toBeVisible();
    await expect(qrImage).toBeVisible();
    await expect(qrImage).toHaveAttribute('src', /^data:image\/svg\+xml;base64,/);

    const pairing = await desktop.evaluate(() => {
      const request = (window as typeof window & {
        qr_login_request?: {
          approval_url: string;
          tag: string;
        };
      }).qr_login_request;
      if (!request) throw new Error('QR login request was not initialized');
      return request;
    });

    await anonymous.goto(pairing.approval_url);
    await expect(anonymous.locator('#qr_login_code')).toHaveText(
      pairing.tag.substring(0, 4).toUpperCase(),
    );
    await anonymous.locator('#bu_approve_qr_login').click();
    await expect(anonymous.locator('#qr_login_needs_login')).toBeVisible();

    await phone.goto(pairing.approval_url);
    await expect(phone.locator('#qr_login_code')).toHaveText(
      pairing.tag.substring(0, 4).toUpperCase(),
    );
    await phone.locator('#bu_approve_qr_login').click();
    await expect(phone.locator('#qr_login_success')).toBeVisible();

    await desktop.waitForURL(url => url.pathname === '/');
    await expect(desktop.locator('#bu_launch')).toBeVisible();

    const sessionCookie = async (context: BrowserContext) => (
      await context.cookies()
    ).find(cookie => cookie.name === 'hs_sid' || cookie.name === '__Host-hs_sid');
    const phoneSession = await sessionCookie(phoneContext);
    const desktopSession = await sessionCookie(desktopContext);
    expect(phoneSession).toBeDefined();
    expect(desktopSession).toBeDefined();
    expect(desktopSession?.value).not.toBe(phoneSession?.value);

    await phone.goto('/');
    await expect(phone.locator('#bu_launch')).toBeVisible();
  } finally {
    await anonymousContext.close();
    await desktopContext.close();
    await phoneContext.close();
  }
});
