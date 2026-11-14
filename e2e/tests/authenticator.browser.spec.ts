import crypto from 'node:crypto';
import { expect, test } from './fixtures';
import { loginAsE2eUser } from './workspace';

// Signing in with an Authenticator app (authenticator_page.rb, totp.rb): set
// up in the profile with the key the page shows and one code, then the
// login page asks for the app's code instead of sending an e-mail, the
// code signs in, »Code per E-Mail schicken« goes back to the e-mail; at
// the end the app is switched off again. The test is the app: it computes
// the codes from the key as an Authenticator app does (RFC 6238).

function totp(secret: string, step: number): string {
  const alphabet = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ234567';
  const bits = secret.replace(/\s/g, '').split('').map((c) => alphabet.indexOf(c).toString(2).padStart(5, '0')).join('');
  const key = Buffer.from(bits.slice(0, bits.length - (bits.length % 8)).match(/.{8}/g)!.map((b) => parseInt(b, 2)));
  const counter = Buffer.alloc(8);
  counter.writeBigUInt64BE(BigInt(step));
  const mac = crypto.createHmac('sha1', key).update(counter).digest();
  const offset = mac[mac.length - 1] & 0x0f;
  return String((mac.readUInt32BE(offset) & 0x7fffffff) % 1_000_000).padStart(6, '0');
}

const step = () => Math.floor(Date.now() / 1000 / 30);

test('sign in with an Authenticator app', async ({ page, e2eEmail }, testInfo) => {
  await loginAsE2eUser(page, e2eEmail, testInfo);

  // set up: the QR code and the key, one code to confirm
  await page.goto('/profil#authenticator');
  await page.locator('#bu_authenticator_start').click();
  await expect(page.locator('#authenticator_qr')).toHaveAttribute('src', /^data:image\/svg\+xml;base64,/);
  const secret = (await page.locator('#authenticator_secret').textContent())!.trim();
  // on a phone the link opens the app with the same key
  await expect(page.locator('#authenticator_link')).toHaveAttribute('href', new RegExp(`^otpauth://totp/.*secret=${secret.replace(/\s/g, '')}`));
  expect(secret.replace(/\s/g, '')).toMatch(/^[A-Z2-7]{32}$/);
  await page.locator('#ti_authenticator_code').fill('000000' === totp(secret, step()) ? '111111' : '000000');
  await page.locator('#bu_authenticator_confirm').click();
  await expect(page.locator('#authenticator_message')).toContainText('Der Code passt nicht');
  await page.locator('#ti_authenticator_code').fill(totp(secret, step()));
  await page.locator('#bu_authenticator_confirm').click();
  await expect(page.locator('.authenticator-on')).toContainText('Eingerichtet');

  // signed out, in again: the page asks for the app's code
  await page.context().clearCookies();
  await page.goto('/login');
  await page.locator('#ti_email').fill(e2eEmail);
  await page.locator('#ti_email').press('Tab');
  await page.locator('#bu_submit_email').click();
  await expect(page.locator('#part1_hint')).toHaveText('Gib den Code aus deiner Authenticator-App ein.');
  await expect(page.locator('#bu_resend_code')).toBeEnabled();
  await expect(page.locator('#resend_label')).toHaveText('Code per E-Mail schicken');
  // the code used to confirm counts once: the app's next one
  await page.locator('#ti_code').fill(totp(secret, step() + 1));
  await page.locator('#bu_submit_code').click();
  await page.waitForURL((url) => !url.pathname.startsWith('/login'));
  await page.goto('/profil#authenticator');
  await expect(page.locator('.authenticator-on')).toBeVisible();

  // the e-mail instead: the button sends one and the page says so
  await page.context().clearCookies();
  await page.goto('/login');
  await page.locator('#ti_email').fill(e2eEmail);
  await page.locator('#ti_email').press('Tab');
  await page.locator('#bu_submit_email').click();
  await expect(page.locator('#resend_label')).toHaveText('Code per E-Mail schicken');
  await page.locator('#bu_resend_code').click();
  await expect(page.locator('#part1_hint')).toContainText('Die E-Mail mit dem Code ist unterwegs');
  await expect(page.locator('#resend_label')).toHaveText('Code erneut senden');
  await loginAsE2eUser(page, e2eEmail, testInfo);

  // switched off: two clicks, then the setup is offered again
  await page.goto('/profil#authenticator');
  await page.locator('#bu_authenticator_off').click();
  await expect(page.locator('#bu_authenticator_off')).toContainText('Wirklich ausschalten?');
  await page.locator('#bu_authenticator_off').click();
  await expect(page.locator('#bu_authenticator_start')).toBeVisible();
});
