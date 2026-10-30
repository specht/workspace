import { expect, test } from './fixtures';
import type { ContainerCommandResult } from './workspace-container';

// »TCP/IP« (src/content/tcpip): the network tools the page has students run,
// in the launched Workspace as the student. Only what works without the
// internet: the commands exist, the Workspace has an address, ping may send
// ICMP, and the page's netcat and HTTP steps work on localhost, with a web
// server of our own instead of the sites the page names.
const TOOLS = ['ping', 'nc', 'curl', 'wget', 'ip', 'ss', 'ifconfig', 'traceroute', 'telnet', 'jq'];
const DIR = '/workspace/.e2e/tcpip';

function expectSuccess(
  result: ContainerCommandResult,
  description: string,
) {
  expect(
    result.exitCode,
    `${description} failed\n` +
    `stdout:\n${result.stdout}\n` +
    `stderr:\n${result.stderr}`,
  ).toBe(0);
}

test(
  'TCP/IP: the network tools of the page work in the Workspace',
  async ({
    workspaceContainer: container,
  }) => {
    test.setTimeout(120_000);
    const run = (command: string) =>
      container.exec(command, { timeoutMs: 60_000, workdir: '/workspace' });

    await test.step('every command of the page exists', async () => {
      const missing = await run(
        `for t in ${TOOLS.join(' ')}; do command -v "$t" > /dev/null || echo "$t"; done`,
      );
      expectSuccess(missing, 'command -v');
      expect(missing.stdout.trim(), 'missing commands').toBe('');
    });

    await test.step('the Workspace has an address of its own (Die eigene Adresse)', async () => {
      const addresses = await run('ip -4 -o addr show');
      expectSuccess(addresses, 'ip addr');
      const own = addresses.stdout.match(/inet (\d+\.\d+\.\d+\.\d+)/g) ?? [];
      expect(own.filter(a => !a.startsWith('inet 127.')), 'an address besides 127.x').not.toHaveLength(0);
      expectSuccess(await run('ip link && ip route && ip neigh'), 'ip link, route, neigh');
    });

    await test.step('ping may send ICMP as the student', async () => {
      const ping = await run('ping -c 1 -W 2 127.0.0.1');
      expectSuccess(ping, 'ping 127.0.0.1');
      expect(ping.stdout).toMatch(/1 (packets )?received/);
    });

    await test.step('netcat: a server with nc -l -p 1234, a client with nc localhost 1234', async () => {
      const talk = await run(
        `rm -rf ${DIR} && mkdir -p ${DIR} && cd ${DIR} && ` +
        '(timeout 10 nc -l -p 1234 > received.txt &) && sleep 1 && ' +
        "ss -tln | grep -q ':1234 ' && echo listening && " +
        "printf 'Hallo vom Client\\n' | timeout 3 nc localhost 1234; sleep 1; cat received.txt",
      );
      expect(talk.stdout).toContain('listening');
      expect(talk.stdout).toContain('Hallo vom Client');
    });

    await test.step('HTTP by hand with nc -C, and curl, wget and jq against a local server', async () => {
      const http = await run(
        `mkdir -p ${DIR}/www && cd ${DIR}/www && ` +
        "printf 'Hallo aus dem Netz\\n' > seite.txt && printf '{\"fact\": \"Katzen schlafen viel.\"}\\n' > fact.json && " +
        '(timeout 20 python3 -m http.server 8765 --bind 127.0.0.1 > /dev/null 2>&1 &) && sleep 1.5 && ' +
        "printf 'GET /seite.txt HTTP/1.0\\n\\n' | timeout 5 nc -C localhost 8765; echo '--- curl'; " +
        'curl -sI http://localhost:8765/seite.txt; echo "--- wget"; ' +
        'wget -q -O - http://localhost:8765/seite.txt; echo "--- jq"; ' +
        'curl -s http://localhost:8765/fact.json | jq -r .fact',
      );
      const [byHand, curl, wget, jq] = http.stdout.split(/^--- \w+$/m);
      expect(byHand, 'nc -C').toMatch(/^HTTP\/1\.0 200/m);
      expect(byHand, 'nc -C').toContain('Hallo aus dem Netz');
      expect(curl, 'curl -I').toMatch(/^HTTP\/1\.0 200/m);
      expect(wget, 'wget').toContain('Hallo aus dem Netz');
      expect(jq?.trim(), 'jq').toBe('Katzen schlafen viel.');
    });
  },
);
