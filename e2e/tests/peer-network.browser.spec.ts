import type { Browser, BrowserContext, TestInfo } from '@playwright/test';
import { test, expect } from './fixtures';
import {
  expectVsCodeReady,
  launchWorkspace,
  loginAsE2eUser,
  resetWorkspace,
} from './workspace';
import { WorkspaceContainer } from './workspace-container';

const PEER_EMAIL = 'e2e-peer@example.com';

async function launchPeerWorkspace(
  browser: Browser,
  testInfo: TestInfo,
): Promise<{context: BrowserContext; container: WorkspaceContainer}> {
  const baseURL = testInfo.project.use.baseURL;
  if (typeof baseURL !== 'string')
    throw new Error('E2E project must define baseURL');

  const context = await browser.newContext({baseURL});
  const page = await context.newPage();

  await loginAsE2eUser(page, PEER_EMAIL, testInfo);
  await resetWorkspace(page, PEER_EMAIL, testInfo);
  const workspace = await launchWorkspace(page, testInfo);
  await expectVsCodeReady(workspace, testInfo);

  const container = new WorkspaceContainer(PEER_EMAIL);
  await container.waitUntilRunning();

  return {context, container};
}

async function startHttpServer(
  container: WorkspaceContainer,
  port: number,
) {
  const result = await container.exec(
    `nohup python3 -m http.server ${port} --bind 0.0.0.0 ` +
    `>/tmp/e2e-peer-${port}.log 2>&1 </dev/null &`,
    {workdir: '/'},
  );

  expect(
    result.exitCode,
    `Could not start peer test server on ${port}: ${result.stderr}`,
  ).toBe(0);

  await expect.poll(async () => {
    const probe = await container.exec(
      `python3 - <<'PY'\n` +
      `import socket\n` +
      `try:\n` +
      `    s = socket.create_connection(("127.0.0.1", ${port}), 0.5)\n` +
      `    s.close()\n` +
      `except OSError:\n` +
      `    raise SystemExit(1)\n` +
      `PY`,
      {workdir: '/', timeoutMs: 2_000},
    );
    return probe.exitCode;
  }, {
    message: `peer test server on ${port} should listen locally`,
    timeout: 5_000,
  }).toBe(0);
}

async function expectTcpBlocked(
  from: WorkspaceContainer,
  host: string,
  port: number,
) {
  const result = await from.exec(
    `python3 - <<'PY'\n` +
    `import socket\n` +
    `try:\n` +
    `    s = socket.create_connection(("${host}", ${port}), 1.0)\n` +
    `    s.close()\n` +
    `except OSError:\n` +
    `    raise SystemExit(0)\n` +
    `raise SystemExit(1)\n` +
    `PY`,
    {workdir: '/', timeoutMs: 3_000},
  );

  expect(
    result.exitCode,
    `${host}:${port} unexpectedly reachable from ${from.name}`,
  ).toBe(0);
}

test('student peers only expose classroom networking ports', async ({
  browser,
  freshWorkspace,
  e2eEmail,
}, testInfo) => {
  // Requesting freshWorkspace is intentional: it creates the first student
  // through Workspace's real launch path before we test east-west traffic.
  void freshWorkspace;

  const first = new WorkspaceContainer(e2eEmail);
  await first.waitUntilRunning();

  const {context: peerContext, container: peer} = await launchPeerWorkspace(
    browser,
    testInfo,
  );

  try {
    const peerIpResult = await peer.exec(
      `hostname -i | awk '{print $1}'`,
      {workdir: '/'},
    );
    expect(peerIpResult.exitCode).toBe(0);
    const peerIp = peerIpResult.stdout.trim();
    expect(peerIp).toMatch(/^\d+\.\d+\.\d+\.\d+$/);

    // 1234 is the port used by the existing TCP/IP tutorial. 40404 exercises
    // the new general-purpose 40000-40999 classroom range.
    await startHttpServer(peer, 1234);
    await startHttpServer(peer, 40404);
    await startHttpServer(peer, 5500);

    for (const port of [1234, 40404]) {
      const allowed = await first.exec(
        `printf 'GET / HTTP/1.0\\r\\n\\r\\n' | ` +
        `timeout 3s netcat ${peerIp} ${port}`,
        {workdir: '/', timeoutMs: 5_000},
      );

      expect(
        allowed.exitCode,
        `netcat peer connection to ${peerIp}:${port} failed:\n${allowed.stderr}`,
      ).toBe(0);
      expect(allowed.stdout).toContain('200 OK');
    }

    // A normal development server must stay private unless Shared Live Apps
    // publishes it through nginx.
    await expectTcpBlocked(first, peerIp, 5500);

    // Most importantly, nginx authentication must not be bypassable by talking
    // directly to another student's unauthenticated code-server listener.
    await expectTcpBlocked(first, peerIp, 8443);

    // Infrastructure on the same Docker network must remain reachable.
    for (const [host, port] of [['mysql', 3306], ['neo4j', 7687]] as const) {
      const infrastructure = await first.exec(
        `python3 - <<'PY'\n` +
        `import socket\n` +
        `s = socket.create_connection(("${host}", ${port}), 2.0)\n` +
        `s.close()\n` +
        `PY`,
        {workdir: '/', timeoutMs: 4_000},
      );

      expect(
        infrastructure.exitCode,
        `${host}:${port} should remain reachable:\n${infrastructure.stderr}`,
      ).toBe(0);
    }
  } finally {
    await peer.exec(
      `pkill -f '[p]ython3 -m http.server (1234|40404|5500)' || true`,
      {workdir: '/'},
    );
    await peerContext.close();
  }
});
