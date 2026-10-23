import fs from 'node:fs';
import path from 'node:path';
import type { APIRequestContext, Browser, BrowserContext, TestInfo } from '@playwright/test';
import { expect, test } from './fixtures';
import { E2E_LOGIN_CODE } from './workspace';

/*
 * Who may use the Workspace (src/ruby/include/access.rb), end to end: the
 * cases the review of October 2026 listed. Each test uses made-up addresses
 * in made-up schools and groups, created through the administration's API
 * and removed afterwards, accounts included.
 *
 * No E2E account is an administrator, so this file logs in as the first
 * address in ADMIN_USERS (src/ruby/credentials.rb), or E2E_ADMIN_EMAIL.
 */

test.describe.configure({ mode: 'serial' });

const RUN = Date.now().toString(36);
const created: string[] = [];
const teachers: string[] = [];

function address(role: string): string {
  const email = `e2e-access-${RUN}-${role}@example.com`;
  created.push(email);
  return email;
}

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

// A logged-in browser context, through the same API the login page uses.
async function signIn(browser: Browser, testInfo: TestInfo, email: string): Promise<BrowserContext> {
  const context = await browser.newContext({ baseURL: baseUrl(testInfo) });
  const requested = await postOk(context.request, '/api/request_login', { email });
  await postOk(context.request, '/api/complete_login', { tag: requested.tag, code: E2E_LOGIN_CODE });
  // The next page sets the domain-wide hs_server_sid for the Workspace proxy.
  await context.request.get('/');
  expect(await loggedIn(context.request)).toBe(true);
  return context;
}

async function loggedIn(request: APIRequestContext): Promise<boolean> {
  const response = await request.get('/profil', { maxRedirects: 0 });
  return response.status() === 200;
}

function workspaceUrl(testInfo: TestInfo, serverTag: string): string {
  const base = new URL(baseUrl(testInfo));
  return `${base.protocol}//${serverTag}.${base.host}/`;
}

// What the Workspace proxy (nginx) answers for this browser: 403/404 when
// it refuses, anything below 400 when code-server answers.
async function workspaceStatus(request: APIRequestContext, url: string): Promise<number> {
  try {
    return (await request.get(url, { maxRedirects: 0, timeout: 10_000 })).status();
  } catch {
    return 0;
  }
}

async function person(admin: APIRequestContext, email: string): Promise<Record<string, unknown> | undefined> {
  const body = await postOk(admin, '/api/admin/people', { query: email, limit: 10 });
  return (body.people as Record<string, unknown>[]).find(p => p.email === email);
}

let adminContext: BrowserContext;
let admin: APIRequestContext;
const schools: string[] = [];
const groups: string[] = [];
let schoolA = '';
let schoolB = '';
let groupA = '';
let groupB = '';
let guestGroup = '';

async function newGroup(name: string, school: string): Promise<string> {
  const body = await postOk(admin, '/api/groups/create', { name, school });
  groups.push(body.created as string);
  return body.created as string;
}

async function addMembers(group: string, ...emails: string[]): Promise<Record<string, unknown>> {
  return await postOk(admin, '/api/groups/add_members', { name: group, lines: emails.join('\n') });
}

test.beforeAll(async ({ browser }, testInfo) => {
  adminContext = await signIn(browser, testInfo, adminEmail());
  admin = adminContext.request;
  for (const name of [`E2E Zugang A ${RUN}`, `E2E Zugang B ${RUN}`]) {
    const body = await postOk(admin, '/api/people/save_school', { name });
    schools.push(body.key as string);
  }
  [schoolA, schoolB] = schools;
  const guests = (await postOk(admin, '/api/people/ensure_guests')).key as string;
  groupA = await newGroup(`E2E Zugang A ${RUN}`, schoolA);
  groupB = await newGroup(`E2E Zugang B ${RUN}`, schoolB);
  guestGroup = await newGroup(`E2E Zugang Gäste ${RUN}`, guests);
});

test.afterAll(async () => {
  if (!admin)
    return;
  for (const email of teachers)
    await post(admin, '/api/people/remove_teacher', { email });
  for (const email of created)
    await post(admin, '/api/admin/delete_account', { email, confirmation: email });
  for (const name of groups)
    await post(admin, '/api/groups/delete', { name, confirmation: name });
  for (const key of schools)
    await post(admin, '/api/people/delete_school', { key });
  await adminContext.close();
});

test('removed from the group during a session: the session and the running Workspace end', async ({ browser }, testInfo) => {
  test.setTimeout(300_000);
  const student = address('removed');
  await addMembers(groupA, student);
  const context = await signIn(browser, testInfo, student);
  try {
    const started = await postOk(context.request, '/api/start_server');
    const url = workspaceUrl(testInfo, started.server_tag as string);
    await expect.poll(() => workspaceStatus(context.request, url), { timeout: 180_000, intervals: [2_000] })
      .toBeLessThan(400);

    await postOk(admin, '/api/groups/remove_member', { name: groupA, email: student });

    expect(await loggedIn(context.request)).toBe(false);
    // nginx is reloaded in the background; the old cookie opens nothing.
    await expect.poll(() => workspaceStatus(context.request, url), { timeout: 30_000 })
      .toBeGreaterThanOrEqual(403);
    expect([403, 404]).toContain(await workspaceStatus(context.request, url));
    const again = await post(context.request, '/api/request_login', { email: student });
    expect(again.status).toBe(404);
  } finally {
    await context.close();
  }
});

test('suspended: no session, no login, no shared link, nothing deleted; lifting restores access', async ({ browser }, testInfo) => {
  const student = address('suspended');
  await addMembers(groupA, student);
  const context = await signIn(browser, testInfo, student);
  try {
    const shared = await postOk(context.request, '/api/share_server');
    const shareTag = shared.share_tag as string;

    await postOk(admin, '/api/admin/suspend_access', { email: student, reason: 'E2E: Widerspruch wird geprüft' });

    expect(await loggedIn(context.request)).toBe(false);
    const login = await post(context.request, '/api/request_login', { email: student });
    expect(login.status).toBe(403);
    expect(login.body.error).toBe('suspended');
    const share = await post(context.request, '/api/start_server_with_share_tag', { share_tag: shareTag });
    expect(share.status).toBe(404);
    const row = await person(admin, student);
    expect(row?.account).toBe(true);
    expect(row?.invited).toBe(true);
    expect(row?.suspended).toBeTruthy();

    await postOk(admin, '/api/admin/lift_suspension', { email: student });
    const back = await signIn(browser, testInfo, student);
    await back.close();
  } finally {
    await context.close();
  }
});

test('a guest without the Nutzungsordnung cannot start, restart or share a Workspace', async ({ browser }, testInfo) => {
  const guest = address('guest');
  await addMembers(guestGroup, guest);
  const context = await signIn(browser, testInfo, guest);
  try {
    for (const url of ['/api/start_server', '/api/restart_server', '/api/share_server']) {
      const reply = await post(context.request, url);
      expect(reply.status, url).toBe(403);
      expect(reply.body.error, url).toBe('terms_required');
    }
    await postOk(context.request, '/api/accept_terms');
    await postOk(context.request, '/api/share_server');
  } finally {
    await context.close();
  }
});

test('an announced deletion keeps the account usable until the day, even when no longer invited', async ({ browser }, testInfo) => {
  const student = address('announced');
  await addMembers(groupA, student);
  const context = await signIn(browser, testInfo, student);
  try {
    const announced = await postOk(admin, '/api/admin/announce_deletion', { email: student });
    expect(announced.deletion_at).toMatch(/^\d{4}-\d{2}-\d{2}$/);
    await postOk(admin, '/api/groups/remove_member', { name: groupA, email: student });

    // To take out the data: the session stays, a new login works.
    expect(await loggedIn(context.request)).toBe(true);
    const login = await post(context.request, '/api/request_login', { email: student });
    expect(login.status).toBe(200);

    // Without the announcement an uninvited account has no access.
    await postOk(admin, '/api/admin/cancel_deletion', { email: student });
    expect(await loggedIn(context.request)).toBe(false);
  } finally {
    await context.close();
  }
});

test('a school without approval does not lock its students out', async ({ browser }, testInfo) => {
  const student = address('school');
  await addMembers(groupA, student);
  await postOk(admin, '/api/people/school_status', { key: schoolA, decision_at: '2026-10-01', agreement_at: '2026-10-01' });
  const context = await signIn(browser, testInfo, student);
  try {
    // The school withdraws: the administration decides about each case
    // (suspend_access), the status alone ends no session.
    await postOk(admin, '/api/people/school_status', { key: schoolA, decision_at: '', agreement_at: '' });
    expect(await loggedIn(context.request)).toBe(true);
  } finally {
    await context.close();
  }
});

test('a student of one school is not added to a group of another school', async () => {
  const student = address('two-schools');
  await addMembers(groupA, student);

  const added = await addMembers(groupB, student);
  expect(added.added).toBe(0);
  expect(added.other_school).toEqual([student]);

  // The same refusal when a group would take its students to another school.
  const extra = await newGroup(`E2E Zugang A2 ${RUN}`, schoolA);
  await addMembers(extra, student);
  const moved = await post(admin, '/api/groups/change_school', { name: extra, school: schoolB });
  expect(moved.status).toBe(409);
  expect(moved.body.error).toBe('students_elsewhere');

  // The report of students in two schools has nobody of ours.
  const report = await postOk(admin, '/api/people/cross_school');
  expect((report.people as { email: string }[]).map(p => p.email)).not.toContain(student);
});

test('a login code requested but never used leaves an account whose only trace is the request, which can be deleted', async ({ browser }, testInfo) => {
  const student = address('never');
  await addMembers(groupA, student);
  const context = await browser.newContext({ baseURL: baseUrl(testInfo) });
  try {
    await postOk(context.request, '/api/request_login', { email: student });
  } finally {
    await context.close();
  }
  const row = await person(admin, student);
  expect(row?.account).toBe(true);
  expect(row?.active_source).toBe('code');
  expect(row?.active_on).toMatch(/^\d{4}-\d{2}-\d{2}$/);
  await postOk(admin, '/api/admin/delete_account', { email: student, confirmation: student });
  expect((await person(admin, student))?.account).toBe(false);
});

test('a deleted account that comes back with a backup is deleted again', async ({ browser }, testInfo) => {
  const student = address('restored');
  await addMembers(groupA, student);
  const context = await signIn(browser, testInfo, student);
  const before = new Date(Date.now() - 2_000).toISOString();
  try {
    await postOk(admin, '/api/admin/delete_account', { email: student, confirmation: student });
    expect(await loggedIn(context.request)).toBe(false);
  } finally {
    await context.close();
  }

  // As if a backup from before the deletion were restored: the account
  // exists again (a login request creates it; the address is still invited).
  const restored = await browser.newContext({ baseURL: baseUrl(testInfo) });
  try {
    await postOk(restored.request, '/api/request_login', { email: student });
  } finally {
    await restored.close();
  }
  expect((await person(admin, student))?.account).toBe(true);

  const repeated = await postOk(admin, '/api/admin/repeat_deletions', { backup_date: before });
  expect(repeated.deleted).toEqual([student]);
  expect((await person(admin, student))?.account).toBe(false);
});

// A teacher of groupA, made through the administration's API.
async function newTeacher(role: string): Promise<string> {
  const teacher = address(role);
  teachers.push(teacher);
  await postOk(admin, '/api/people/save_teacher', { email: teacher, school: schoolA });
  await postOk(admin, '/api/groups/add_teacher', { name: groupA, email: teacher });
  return teacher;
}

test('a teacher taken off the group while imitating a student is logged out at once', async ({ browser }, testInfo) => {
  const teacher = await newTeacher('imitating-teacher');
  const student = address('imitated');
  await addMembers(groupA, student);
  const context = await signIn(browser, testInfo, teacher);
  try {
    await postOk(context.request, '/api/impersonate', { email: student, reason: 'E2E: Nachahmen' });
    expect(await loggedIn(context.request)).toBe(true);

    await postOk(admin, '/api/groups/remove_teacher', { name: groupA, email: teacher });

    expect(await loggedIn(context.request)).toBe(false);
  } finally {
    await context.close();
  }
});

test('a teacher taken off the group no longer sees the Workspace it watched', async ({ browser }, testInfo) => {
  test.setTimeout(300_000);
  const teacher = await newTeacher('watching-teacher');
  const student = address('watched');
  await addMembers(groupA, student);
  const studentContext = await signIn(browser, testInfo, student);
  const context = await signIn(browser, testInfo, teacher);
  try {
    const started = await postOk(studentContext.request, '/api/start_server');
    const url = workspaceUrl(testInfo, started.server_tag as string);
    await expect.poll(() => workspaceStatus(studentContext.request, url), { timeout: 180_000, intervals: [2_000] })
      .toBeLessThan(400);
    // The teacher reaches it under the watch tag (its own address, with hs_watch_tag)
    const watched = await postOk(context.request, '/api/start_server_as_admin', { email: student, reason: 'E2E: Zuschauen' });
    const watchUrl = workspaceUrl(testInfo, watched.watch_tag as string);
    await expect.poll(() => workspaceStatus(context.request, watchUrl), { timeout: 60_000, intervals: [2_000] })
      .toBeLessThan(400);

    await postOk(admin, '/api/groups/remove_teacher', { name: groupA, email: teacher });

    // nginx is reloaded in the background; the watch is gone, the student keeps the Workspace.
    await expect.poll(() => workspaceStatus(context.request, watchUrl), { timeout: 30_000 })
      .toBeGreaterThanOrEqual(403);
    expect(await workspaceStatus(studentContext.request, url)).toBeLessThan(400);
  } finally {
    await context.close();
    await studentContext.close();
  }
});

test('an administrator gives a reason before looking into a Workspace', async () => {
  const student = address('look-in');
  await addMembers(groupA, student);
  const reply = await post(admin, '/api/start_server_as_admin', { email: student });
  expect(reply.status).toBe(400);
  expect(reply.body.error).toBe('reason_missing');
});
