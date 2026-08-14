import { expect, test } from './fixtures';

const PORT = 43127;
const APP_MARKER = 'Shared Live App E2E';

const serverSource = `
const crypto = require('node:crypto');
const http = require('node:http');

const marker = ${JSON.stringify(APP_MARKER)};
const server = http.createServer((request, response) => {
  response.writeHead(200, {'content-type': 'text/plain; charset=utf-8'});
  response.end([
    \`${'${marker}'}: ${'${request.url}'}\`,
    \`cookie=${'${request.headers.cookie ?? ""}'}\`,
    \`authorization=${'${request.headers.authorization ?? ""}'}\`,
    '',
  ].join('\\n'));
});

server.on('upgrade', (request, socket) => {
  const key = request.headers['sec-websocket-key'];
  if (!key) {
    socket.destroy();
    return;
  }

  const accept = crypto
    .createHash('sha1')
    .update(key + '258EAFA5-E914-47DA-95CA-C5AB0DC85B11')
    .digest('base64');

  socket.write([
    'HTTP/1.1 101 Switching Protocols',
    'Upgrade: websocket',
    'Connection: Upgrade',
    \`Sec-WebSocket-Accept: ${'${accept}'}\`,
    '',
    '',
  ].join('\\r\\n'));

  const message = Buffer.from('shared-websocket-ok');
  socket.write(Buffer.concat([
    Buffer.from([0x81, message.length]),
    message,
  ]));
});

server.listen(${PORT}, '0.0.0.0');
`;

test('a student can share and revoke a live app', async ({
  freshWorkspace,
  workspaceContainer,
}) => {
  const profile = await freshWorkspace.context().newPage();
  let serverRunning = false;

  const startServer = async () => {
    const start = await workspaceContainer.exec([
      'setsid node shared-live-app-server.js',
      '> shared-live-app-server.log 2>&1 < /dev/null &',
      'echo $! > shared-live-app-server.pid',
    ].join(' '));
    expect(start.exitCode, start.stderr).toBe(0);
    serverRunning = true;

    const ready = await workspaceContainer.exec([
      'for attempt in $(seq 1 40); do',
      `curl -fsS http://127.0.0.1:${PORT}/ready && exit 0;`,
      'sleep 0.25;',
      'done;',
      'cat shared-live-app-server.log >&2;',
      'exit 1',
    ].join(' '));
    expect(ready.exitCode, ready.stderr).toBe(0);
    expect(ready.stdout).toContain(`${APP_MARKER}: /ready`);
  };

  const stopServer = async () => {
    if (!serverRunning)
      return;

    const stop = await workspaceContainer.exec([
      'server_pid=$(cat shared-live-app-server.pid)',
      'kill -TERM -- -"$server_pid" 2>/dev/null || true',
      'for attempt in $(seq 1 40); do',
      '! kill -0 "$server_pid" 2>/dev/null && exit 0;',
      'sleep 0.25;',
      'done;',
      'exit 1',
    ].join('\n'));
    expect(stop.exitCode, stop.stderr).toBe(0);
    serverRunning = false;
  };

  const portRow = profile
    .locator('#live-apps tbody tr')
    .filter({has: profile.locator('td', {hasText: String(PORT)})});

  const sharePort = async () => {
    const responsePromise = profile.waitForResponse(response =>
      response.url().endsWith('/api/live_apps/share')
      && response.request().method() === 'POST',
    );
    await portRow.getByRole('button', {name: 'Teilen'}).click();
    const response = await responsePromise;
    expect(response.status()).toBe(200);

    const result = await response.json() as {url: string};
    await expect(portRow.getByRole('link', {name: 'Öffnen'})).toHaveAttribute(
      'href',
      result.url,
    );
    return result.url;
  };

  const unsharePort = async () => {
    const responsePromise = profile.waitForResponse(response =>
      response.url().endsWith('/api/live_apps/unshare')
      && response.request().method() === 'POST',
    );
    await portRow.getByRole('button', {name: 'Nicht mehr teilen'}).click();
    const response = await responsePromise;
    expect(response.status()).toBe(200);
    await expect(portRow.getByRole('button', {name: 'Teilen'})).toBeVisible();
  };

  const expectUnavailable = async (url: string) => {
    const unavailablePage = await profile.context().newPage();
    try {
      await expect.poll(async () => {
        const separator = url.includes('?') ? '&' : '?';
        const response = await unavailablePage.goto(
          `${url}${separator}revoked=${Date.now()}`,
        );
        return response?.status();
      }, {
        message: `Expected ${url} to become unavailable`,
        timeout: 15_000,
      }).toBe(404);
    } finally {
      await unavailablePage.close();
    }
  };

  await workspaceContainer.writeFile(
    'shared-live-app-server.js',
    serverSource,
  );

  try {
    await test.step('Detect the student HTTP server', async () => {
      const profileUpdatePromise = new Promise<string>((resolve, reject) => {
        const timer = setTimeout(() => {
          reject(new Error('Timed out waiting for live-app profile update'));
        }, 10_000);

        profile.on('websocket', socket => {
          if (!socket.url().endsWith('/ws/live_apps'))
            return;

          socket.on('framereceived', event => {
            clearTimeout(timer);
            resolve(String(event.payload));
          });
          socket.on('socketerror', error => {
            clearTimeout(timer);
            reject(new Error(`Live-app profile WebSocket failed: ${error}`));
          });
        });
      });

      await startServer();
      await profile.goto('/profil');
      const profileUpdate = JSON.parse(
        await profileUpdatePromise,
      ) as {action?: string};
      expect(profileUpdate.action).toBe('refresh_live_apps');
      await expect(portRow).toBeVisible();
      await expect(portRow).toContainText('shared-live-app-server.js');
      await expect(
        portRow.getByRole('button', {name: 'Teilen'}),
      ).toBeVisible();
    });

    let sharedUrl = '';
    await test.step('Share the port and access its HTTP endpoint', async () => {
      sharedUrl = await sharePort();
      expect(sharedUrl).toMatch(/^http:\/\/live-[a-z0-9]+\.workspace\.test:8025\/$/);

      const sharedPage = await profile.context().newPage();
      try {
        await sharedPage.goto(`${sharedUrl}student-app`);
        await expect(sharedPage.locator('body')).toHaveText(
          [
            `${APP_MARKER}: /student-app`,
            'cookie=',
            'authorization=',
          ].join('\n'),
        );

        const websocketMessage = await sharedPage.evaluate(async () => {
          const protocol = location.protocol === 'https:' ? 'wss:' : 'ws:';
          const socket = new WebSocket(`${protocol}//${location.host}/socket`);

          return await new Promise<string>((resolve, reject) => {
            const timer = window.setTimeout(() => {
              socket.close();
              reject(new Error('Timed out waiting for shared WebSocket'));
            }, 10_000);

            socket.onmessage = event => {
              window.clearTimeout(timer);
              socket.close();
              resolve(String(event.data));
            };
            socket.onerror = () => {
              window.clearTimeout(timer);
              reject(new Error('Shared WebSocket failed'));
            };
          });
        });
        expect(websocketMessage).toBe('shared-websocket-ok');
      } finally {
        await sharedPage.close();
      }

      const browser = profile.context().browser();
      if (!browser)
        throw new Error('Shared Live Apps test requires a browser context');

      const anonymousContext = await browser.newContext();
      try {
        const anonymousPage = await anonymousContext.newPage();
        const response = await anonymousPage.goto(`${sharedUrl}anonymous`);
        expect(response?.status()).toBe(401);
      } finally {
        await anonymousContext.close();
      }
    });

    await test.step('Unshare the port and revoke access', async () => {
      await unsharePort();
      await expectUnavailable(sharedUrl);
    });

    await test.step('Invalidate the share when its listener disappears', async () => {
      sharedUrl = await sharePort();

      await stopServer();
      await expect(portRow).toHaveCount(0, {timeout: 30_000});
      await expectUnavailable(sharedUrl);

      await startServer();
      await expect(portRow).toBeVisible({timeout: 30_000});
      await expect(
        portRow.getByRole('button', {name: 'Teilen'}),
      ).toBeVisible();
      await expectUnavailable(sharedUrl);
    });
  } finally {
    await stopServer();
    await profile.evaluate(async port => {
      await fetch('/api/live_apps/unshare', {
        method: 'POST',
        headers: {'content-type': 'application/json'},
        credentials: 'same-origin',
        body: JSON.stringify({port}),
      });
    }, PORT).catch(() => undefined);
    await profile.close();
  }
});
