import fs from 'node:fs';
import path from 'node:path';
import type { APIRequestContext, Browser, BrowserContext, TestInfo } from '@playwright/test';
import { expect, test } from './fixtures';
import { E2E_LOGIN_CODE } from './workspace';

/*
 * The transition overview on /personen (src/ruby/include/school_transition.rb,
 * docs/handoff-datenschutz.md »Übergangszeit«): the administration records how
 * the talks with a school stand. It must not change the school's decision,
 * its agreement, its approval, anyone's consent or anyone's access, and it
 * must not show up anywhere but /personen.
 *
 * Made-up school, group and student, created through the administration's
 * API and removed afterwards. Logs in as the first address in ADMIN_USERS
 * (src/ruby/credentials.rb), or E2E_ADMIN_EMAIL.
 */

test.describe.configure({ mode: 'serial' });

const RUN = Date.now().toString(36);
const NOTE = `E2E-Notiz ${RUN}`;
const student = `e2e-transition-${RUN}-student@example.com`;

function adminEmail(): string {
  if (process.env.E2E_ADMIN_EMAIL)
    return process.env.E2E_ADMIN_EMAIL;
  const credentials = fs.readFileSync(
    path.resolve(process.cwd(), '..', 'src', 'ruby', 'credentials.rb'), 'utf8');
  const email = credentials.match(/^ADMIN_USERS\s*=\s*\[\s*['"]([^'"]+)['"]/m)?.[1];
  if (!email)
    throw new Error('No ADMIN_USERS in credentials.rb; set E2E_ADMIN_EMAIL');
  return email;
}

function baseUrl(testInfo: TestInfo): string {
  const value = testInfo.project.use.baseURL;
  if (typeof value !== 'string')
    throw new Error('E2E project must define baseURL');
  return value;
}

type Reply = { status: number; body: Record<string, unknown> };

async function post(request: APIRequestContext, url: string, data: object = {}): Promise<Reply> {
  const response = await request.post(url, { data });
  const text = await response.text();
  let body: Record<string, unknown> = {};
  try {
    body = JSON.parse(text) as Record<string, unknown>;
  } catch {
    body = { text };
  }
  return { status: response.status(), body };
}

async function postOk(request: APIRequestContext, url: string, data: object = {}): Promise<Record<string, unknown>> {
  const reply = await post(request, url, data);
  expect(reply.status, `${url} ${JSON.stringify(reply.body)}`).toBe(200);
  return reply.body;
}

async function signIn(browser: Browser, testInfo: TestInfo, email: string): Promise<BrowserContext> {
  const context = await browser.newContext({ baseURL: baseUrl(testInfo) });
  const requested = await postOk(context.request, '/api/request_login', { email });
  await postOk(context.request, '/api/complete_login', { tag: requested.tag, code: E2E_LOGIN_CODE });
  await context.request.get('/');
  return context;
}

async function loggedIn(request: APIRequestContext): Promise<boolean> {
  return (await request.get('/profil', { maxRedirects: 0 })).status() === 200;
}

type School = Record<string, unknown> & { key: string; transition?: Record<string, unknown> };

async function school(key: string): Promise<School> {
  const body = await postOk(admin, '/api/people/schools');
  const found = (body.schools as School[]).find(s => s.key === key);
  expect(found, key).toBeTruthy();
  return found!;
}

async function consentOf(email: string): Promise<unknown> {
  const body = await postOk(admin, '/api/groups/list');
  for (const group of body.groups as { name: string; members?: { email: string; consent_at?: unknown }[] }[]) {
    const member = (group.members ?? []).find(m => m.email === email);
    if (member)
      return member.consent_at ?? null;
  }
  return undefined;
}

let adminContext: BrowserContext;
let admin: APIRequestContext;
let schoolKey = '';
let groupName = '';

test.beforeAll(async ({ browser }, testInfo) => {
  adminContext = await signIn(browser, testInfo, adminEmail());
  admin = adminContext.request;
  schoolKey = (await postOk(admin, '/api/people/save_school', { name: `E2E Übergang ${RUN}` })).key as string;
  groupName = (await postOk(admin, '/api/groups/create', { name: `E2E Übergang ${RUN}`, school: schoolKey })).created as string;
  await postOk(admin, '/api/groups/add_members', { name: groupName, lines: student });
});

test.afterAll(async () => {
  if (!admin)
    return;
  await post(admin, '/api/admin/delete_account', { email: student, confirmation: student });
  await post(admin, '/api/groups/delete', { name: groupName, confirmation: groupName });
  await post(admin, '/api/people/delete_school', { key: schoolKey });
  await adminContext.close();
});

test('the administration records progress; it survives a reload and leaves the legal fields alone', async () => {
  await postOk(admin, '/api/groups/set_consent', { name: groupName, email: student, date: '2026-10-01' });
  await postOk(admin, '/api/people/school_status', { key: schoolKey, decision_at: '2026-10-05' });

  await postOk(admin, '/api/people/school_transition', {
    key: schoolKey, transition_status: 'awaiting_documents', last_contact_at: '2026-11-04',
    next_follow_up_at: '2026-12-01', contacted_at: '2026-11-04', transition_note: NOTE,
  });
  // the legal fields are ignored here, they have a route of their own
  await postOk(admin, '/api/people/school_transition', { key: schoolKey, decision_at: '', agreement_at: '2026-11-01', approved_at: 1 });

  const s = await school(schoolKey);
  expect(s.transition_status).toBe('awaiting_documents');
  expect(s.next_follow_up_at).toBe('2026-12-01');
  expect(s.contacted_at).toBe('2026-11-04');
  expect(s.transition_note).toBe(NOTE);
  expect(s.decision_at).toBe('2026-10-05');
  expect(s.agreement_at ?? null).toBeNull();
  expect(s.approved_at ?? null).toBeNull();
  expect(s.transition?.category).not.toBe('documented');
  expect(await consentOf(student)).toBe('2026-10-01');
});

test('invalid values are refused, and so is a school that does not exist or the guests', async () => {
  for (const data of [{ transition_status: 'approved' }, { last_contact_at: '4.11.2026' }, { open_questions: 'x'.repeat(1001) }]) {
    const reply = await post(admin, '/api/people/school_transition', { key: schoolKey, ...data });
    expect(reply.status, JSON.stringify(data)).toBe(400);
  }
  expect((await post(admin, '/api/people/school_transition', { key: `nicht-da-${RUN}`, transition_status: 'in_discussion' })).status).toBe(404);
  const guests = (await postOk(admin, '/api/people/ensure_guests')).key as string;
  expect((await post(admin, '/api/people/school_transition', { key: guests, transition_status: 'in_discussion' })).status).toBe(404);
  expect((await school(schoolKey)).transition_status).toBe('awaiting_documents');
});

test('a student of a school without approval logs in, whatever the progress; nobody else may change it', async ({ browser }, testInfo) => {
  await postOk(admin, '/api/people/school_transition', { key: schoolKey, transition_status: 'follow_up' });
  const context = await signIn(browser, testInfo, student);
  try {
    expect(await loggedIn(context.request)).toBe(true);
    for (const url of ['/api/people/school_transition', '/api/people/school_template', '/api/people/schools']) {
      const reply = await post(context.request, url, { key: schoolKey, transition_status: 'not_started', template: 'reminder' });
      expect(reply.status, url).toBe(403);
    }
    // the progress is nowhere but on /personen
    for (const page of ['/impressum', `/datenschutz?schule=${schoolKey}`, '/gruppen', '/profil']) {
      const text = await (await context.request.get(page)).text();
      expect(text, page).not.toContain(NOTE);
    }
    expect(await loggedIn(context.request)).toBe(true);
  } finally {
    await context.close();
  }
  expect((await school(schoolKey)).transition_status).toBe('follow_up');
});

test('a message to copy is filled in for the school and sends nothing', async () => {
  const body = await postOk(admin, '/api/people/school_template', { key: schoolKey, template: 'reminder' });
  expect(body.text as string).toContain(`E2E Übergang ${RUN}`);
  expect(body.text as string).toContain('Vereinbarung zur Auftragsverarbeitung');
  expect((await post(admin, '/api/people/school_template', { key: schoolKey, template: 'drohbrief' })).status).toBe(400);
});

test('/personen shows the transition box and the school\'s progress', async ({ browser }, testInfo) => {
  const page = await adminContext.newPage();
  try {
    await page.goto(`${baseUrl(testInfo)}/personen`);
    await page.locator('#tab-schulen').click();
    await expect(page.locator('#transition_here')).toContainText('Übergangszeit bis');
    // the school's own row; a teacher's list below another school (row-detail)
    // can name it too, once earlier runs have left schools behind
    const row = page.locator('tr:not(.row-detail)', { hasText: `E2E Übergang ${RUN}` }).first();
    await expect(row).toContainText('Nachfassen fällig');
    await row.getByRole('button', { name: `Übergang bei E2E Übergang ${RUN}` }).click();
    const detail = page.locator('tr.transition-detail:visible');
    await expect(detail.getByLabel(`Notiz (ohne Namen von Personen), E2E Übergang ${RUN}`)).toHaveValue(NOTE);
  } finally {
    await page.close();
  }
});
