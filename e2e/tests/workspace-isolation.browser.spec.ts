import { execFileSync } from 'node:child_process';
import crypto from 'node:crypto';
import http from 'node:http';
import type { APIRequestContext, Browser, BrowserContext, TestInfo } from '@playwright/test';
import { expect, test } from './fixtures';
import { E2E_LOGIN_CODE } from './workspace';

// Can a student get at the files of someone else's Workspace, a teacher's
// with an exam in it? (the guide »Leistungsüberprüfungen vorbereiten«,
// »Was abgeschirmt ist und was nicht«). The teacher's and a student's
// Workspace both running:
// - the address of the teacher's Workspace opens for the teacher only, not
//   for the student, not without login (nginx compares hs_server_sid);
// - a page of the student's (an app on a subdomain, opened in the teacher's
//   browser) that opens a WebSocket to the teacher's Workspace with the
//   teacher's cookie is refused by code-server (another Origin);
// - from inside the student's container the teacher's container is not
//   reachable (peer_firewall), and the student's container has only its
//   own folders mounted, no /user.

const TEACHER = 'e2e-teacher@example.com';
const STUDENT = 'e2e-0@example.com';

function fsTag(email: string): string {
  const hex = crypto.createHash('sha256').update(email).digest('hex');
  return BigInt(`0x${hex}`).toString(36).slice(0, 16);
}

function baseUrl(testInfo: TestInfo): string {
  return testInfo.project.use.baseURL as string;
}

async function signIn(browser: Browser, testInfo: TestInfo, email: string): Promise<BrowserContext> {
  const context = await browser.newContext({ baseURL: baseUrl(testInfo) });
  const requested = await (await context.request.post('/api/request_login', { data: { email } })).json();
  await context.request.post('/api/complete_login', { data: { tag: requested.tag, code: E2E_LOGIN_CODE } });
  await context.request.get('/');
  return context;
}

async function startWorkspace(request: APIRequestContext, testInfo: TestInfo): Promise<string> {
  const started = await (await request.post('/api/start_server', { data: {} })).json();
  const base = new URL(baseUrl(testInfo));
  const url = `${base.protocol}//${started.server_tag}.${base.host}/`;
  await expect.poll(() => status(request, url), { timeout: 180_000, intervals: [2_000] }).toBeLessThan(400);
  return url;
}

async function status(request: APIRequestContext, url: string): Promise<number> {
  try {
    return (await request.get(url, { maxRedirects: 0, timeout: 10_000 })).status();
  } catch {
    return 0;
  }
}

// a WebSocket handshake by hand: the status code of the answer
function upgrade(url: string, cookie: string, origin: string): Promise<number> {
  return new Promise((resolve) => {
    const u = new URL(url);
    const req = http.request({
      host: u.hostname, port: u.port, path: '/?type=ExtensionHost&reconnectionToken=e2e&reconnection=false&skipWebSocketFrames=false',
      headers: {
        Connection: 'Upgrade', Upgrade: 'websocket', 'Sec-WebSocket-Version': '13',
        'Sec-WebSocket-Key': crypto.randomBytes(16).toString('base64'), Origin: origin, Cookie: cookie,
      },
    });
    req.on('upgrade', (res, socket) => { socket.destroy(); resolve(res.statusCode ?? 101); });
    req.on('response', (res) => { res.resume(); resolve(res.statusCode ?? 0); });
    req.on('error', () => resolve(0));
    req.setTimeout(10_000, () => { req.destroy(); resolve(0); });
    req.end();
  });
}

const docker = (...args: string[]) => {
  try {
    return execFileSync('docker', args, { stdio: 'pipe' }).toString().trim();
  } catch (e: any) {
    return `ERR ${(e.stdout || '').toString()}${(e.stderr || '').toString()}`.trim();
  }
};

test('a student cannot reach the files of the teacher’s Workspace', async ({ browser }, testInfo) => {
  test.setTimeout(420_000);
  const teacher = await signIn(browser, testInfo, TEACHER);
  const student = await signIn(browser, testInfo, STUDENT);
  const anonymous = await browser.newContext({ baseURL: baseUrl(testInfo) });
  try {
    const teacherUrl = await startWorkspace(teacher.request, testInfo);
    const studentUrl = await startWorkspace(student.request, testInfo);

    // the address: the teacher only
    expect(await status(teacher.request, teacherUrl)).toBeLessThan(400);
    expect([403, 404]).toContain(await status(student.request, teacherUrl));
    expect([403, 404]).toContain(await status(anonymous.request, teacherUrl));

    // a WebSocket from a page of the student's, with the teacher's cookie
    const sid = (await teacher.cookies()).find((c) => c.name === 'hs_server_sid')!.value;
    const own = new URL(teacherUrl).origin;
    const foreign = `${new URL(studentUrl).protocol}//${new URL(studentUrl).hostname.split('.')[0]}-3000.${new URL(baseUrl(testInfo)).host}`;
    // the teacher's own page: open (101); the student's page with the same
    // cookie: refused by code-server (nginx let the cookie pass); no cookie:
    // refused by nginx
    expect(await upgrade(teacherUrl, `hs_server_sid=${sid}`, own)).toBe(101);
    expect(await upgrade(teacherUrl, `hs_server_sid=${sid}`, foreign)).toBe(403);
    expect(await upgrade(teacherUrl, '', own)).toBe(403);

    // from inside the student's container
    const studentContainer = `hs_code_${fsTag(STUDENT)}`;
    const teacherIp = docker('inspect', `hs_code_${fsTag(TEACHER)}`, '--format', '{{(index .NetworkSettings.Networks "workspace_user").IPAddress}}');
    expect(teacherIp).toMatch(/^\d+\.\d+\.\d+\.\d+$/);
    expect(docker('exec', studentContainer, 'sh', '-c', `nc -z -w 3 ${teacherIp} 8443 && echo open || echo blocked`)).toBe('blocked');
    const mounts = JSON.parse(docker('inspect', studentContainer, '--format', '{{json .Mounts}}')) as { Source: string; Destination: string }[];
    const own_ = `/user/${fsTag(STUDENT)}/`;
    for (const m of mounts) {
      if (m.Destination === '/etc/resolv.conf') continue;
      expect(m.Source, `${m.Destination} comes from the student's own folder`).toContain(own_);
    }
    expect(docker('exec', studentContainer, 'sh', '-c', 'ls /user 2>&1 || true')).toContain('No such file');
  } finally {
    await teacher.close();
    await student.close();
    await anonymous.close();
  }
});
