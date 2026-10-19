import { execFileSync } from 'node:child_process';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import type { APIRequestContext } from '@playwright/test';
import { expect, test } from './fixtures';
import { loginAsE2eUser } from './workspace';
import { WorkspaceContainer } from './workspace-container';

// An exam (»Leistungsüberprüfung«) gives every participant a MySQL and a
// Neo4j database of their own for that exam, apart from their normal ones:
// they work there (a table, a graph loaded with neo4j_bolt load), see nothing
// of their normal databases, and the logins go when the exam is deleted.
// The exam Workspace starts only while the exam runs.

const TEACHER = 'e2e-teacher@example.com';

// Three films, two directors' worth of edges, in neo4j_bolt's dump format.
const NEO4J_DUMP = [
  'n {"id":0,"labels":["Movie"],"properties":{"title":"Metropolis","year":1927}}',
  'n {"id":1,"labels":["Movie"],"properties":{"title":"M","year":1931}}',
  'n {"id":2,"labels":["Person"],"properties":{"name":"Fritz Lang"}}',
  'r {"from":2,"to":0,"type":"DIRECTED","properties":{}}',
  'r {"from":2,"to":1,"type":"DIRECTED","properties":{}}',
].join('\n') + '\n';

function examPackage(name: string): Buffer {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'exam-db-'));
  try {
    fs.mkdirSync(path.join(dir, '.workspace'));
    fs.writeFileSync(path.join(dir, '.workspace', 'config.yaml'), 'workspace_package: 1\ngit:\n  mode: none\n');
    fs.writeFileSync(path.join(dir, 'AUFGABE.md'), '# Datenbanken in der Prüfung\n');
    fs.writeFileSync(path.join(dir, 'filme.dump'), NEO4J_DUMP);
    const archive = path.join(os.tmpdir(), `${name}.tar.gz`);
    execFileSync('tar', ['-czf', archive, '-C', dir, '.']);
    const bytes = fs.readFileSync(archive);
    fs.rmSync(archive);
    return bytes;
  } finally {
    fs.rmSync(dir, { recursive: true, force: true });
  }
}

async function post(api: APIRequestContext, url: string, data: object = {}) {
  const response = await api.post(url, { data });
  return { status: response.status(), body: await response.json().catch(() => ({})) };
}

// A command in the exam Workspace that only works once the databases are
// set up (that happens just after the container starts).
async function eventually(container: WorkspaceContainer, command: string, timeoutMs = 120_000) {
  const deadline = Date.now() + timeoutMs;
  for (;;) {
    const result = await container.exec(command, { workdir: '/workspace' });
    if (result.exitCode === 0 || Date.now() > deadline) return result;
    await new Promise((resolve) => setTimeout(resolve, 2_000));
  }
}

const mysqlAs = (login: string, password: string) =>
  ['exec', 'workspace_mysql_1', 'mysql', '-N', '-u', login, `-p${password}`, '-e', 'SELECT 1;'];
const neo4jAs = (login: string, password: string) =>
  ['exec', 'workspace_neo4j_1', 'cypher-shell', '-a', 'neo4j://localhost:7687', '-u', login, '-p', password, '-d', login, 'RETURN 1;'];

function fails(command: string, args: string[]): boolean {
  try {
    execFileSync(command, args, { stdio: 'pipe' });
    return false;
  } catch {
    return true;
  }
}

test('an exam has MySQL and Neo4j databases of its own', async ({ browser, page, e2eEmail }, testInfo) => {
  test.setTimeout(420_000);
  const name = `e2e-datenbanken-${Date.now()}`;

  const teacherContext = await browser.newContext();
  const teacher = await teacherContext.newPage();
  await loginAsE2eUser(teacher, TEACHER, testInfo);
  await loginAsE2eUser(page, e2eEmail, testInfo);

  let tag = '';
  try {
    await test.step('The teacher uploads an exam package', async () => {
      const upload = await teacher.request.post('/api/upload_test_archive', {
        multipart: { file: { name: `${name}.tar.gz`, mimeType: 'application/gzip', buffer: examPackage(name) } },
      });
      expect(upload.ok(), await upload.text()).toBe(true);
      const exams = await post(teacher.request, '/api/get_my_test_archives');
      tag = exams.body.entries.find((e: any) => e.filename === `${name}.tar.gz`)?.tag;
      expect(tag).toBeTruthy();
    });

    await test.step('Before the exam runs, its Workspace does not start', async () => {
      expect((await post(page.request, '/api/start_server', { test_tag: tag })).status).toBe(403);
      const chosen = await post(teacher.request, '/api/test_set_users', { tag, emails: [e2eEmail], flag: true });
      expect(chosen.body.emails).toEqual([e2eEmail]);
      expect((await post(page.request, '/api/start_server', { test_tag: tag })).status).toBe(403);
      expect((await post(teacher.request, '/api/start_test', { tag })).status).toBe(200);
    });

    const exam = new WorkspaceContainer(`${e2eEmail}${tag}`);
    let normalDatabase = '';
    let mysqlLogin = '';
    let mysqlPassword = '';
    let neo4jPassword = '';

    await test.step('The student starts the exam Workspace', async () => {
      const started = await post(page.request, '/api/start_server', { test_tag: tag });
      expect(started.status, JSON.stringify(started.body)).toBe(200);
      await exam.waitUntilRunning(180_000);
      normalDatabase = (await post(page.request, '/api/get_my_mysql_databases')).body.result.databases[0];
      expect(normalDatabase).toBeTruthy();
    });

    await test.step('MySQL: an own database, apart from the normal one', async () => {
      const created = await eventually(exam,
        'mysql -N -e "CREATE TABLE exam_check (id INT); INSERT INTO exam_check VALUES (42); SELECT id FROM exam_check;"');
      expect(created.exitCode, created.stderr).toBe(0);
      expect(created.stdout.trim()).toBe('42');
      const env = await exam.exec('printf "%s\\n%s\\n%s" "$MYSQL_USER" "$MYSQL_PASSWORD" "$NEO4J_PASSWORD"', { workdir: '/workspace' });
      [mysqlLogin, mysqlPassword, neo4jPassword] = env.stdout.split('\n');
      expect(mysqlLogin).not.toBe(normalDatabase);
      const listed = await exam.exec('mysql -N -e "SHOW DATABASES;"', { workdir: '/workspace' });
      const names = listed.stdout.split('\n').map((line) => line.trim());
      expect(names).toContain(mysqlLogin);
      expect(names).not.toContain(normalDatabase);
      // and the normal database did not get the table
      const normal = await post(page.request, '/api/get_my_mysql_databases');
      expect(Object.keys(normal.body.result.tables[normalDatabase] || {})).not.toContain('exam_check');
    });

    await test.step('Neo4j: neo4j_bolt load fills the exam database', async () => {
      const loaded = await eventually(exam, 'neo4j_bolt load filme.dump');
      expect(loaded.exitCode, loaded.stderr).toBe(0);
      const dumped = await exam.exec('neo4j_bolt dump --data-only', { workdir: '/workspace' });
      expect(dumped.exitCode, dumped.stderr).toBe(0);
      const lines = dumped.stdout.split('\n');
      expect(lines.filter((l) => l.startsWith('n ')).length).toBe(3);
      expect(lines.filter((l) => l.startsWith('r ')).length).toBe(2);
      const other = await exam.exec(`neo4j_bolt -d '${normalDatabase}' dump --data-only`, { workdir: '/workspace' });
      expect(other.exitCode, 'the normal Neo4j database is out of reach').not.toBe(0);
    });

    await test.step('Both logins work from outside the Workspace too', async () => {
      expect(fails('docker', mysqlAs(mysqlLogin, mysqlPassword))).toBe(false);
      expect(fails('docker', neo4jAs(mysqlLogin, neo4jPassword))).toBe(false);
    });

    await test.step('After »Beenden« the exam Workspace no longer starts', async () => {
      expect((await post(teacher.request, '/api/stop_test', { tag })).status).toBe(200);
      expect((await post(page.request, '/api/start_server', { test_tag: tag })).status).toBe(403);
    });

    await test.step('Deleting the exam removes its logins', async () => {
      expect((await post(teacher.request, '/api/delete_test', { tag })).status).toBe(200);
      tag = '';
      expect(fails('docker', mysqlAs(mysqlLogin, mysqlPassword))).toBe(true);
      expect(fails('docker', neo4jAs(mysqlLogin, neo4jPassword))).toBe(true);
      // the student's normal database is still there
      expect((await post(page.request, '/api/get_my_mysql_databases')).body.result.databases).toContain(normalDatabase);
    });
  } finally {
    if (tag) await post(teacher.request, '/api/delete_test', { tag });
    await teacherContext.close();
  }
});
