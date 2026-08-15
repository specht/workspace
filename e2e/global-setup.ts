import fs from 'node:fs';
import path from 'node:path';

export default async function globalSetup() {
  const userCount = Number.parseInt(process.env.E2E_USER_COUNT ?? '8', 10);

  if (!Number.isInteger(userCount) || userCount < 1)
    throw new Error('E2E_USER_COUNT must be a positive integer');

  // The E2E suite is intentionally local-development-only. Workspace's
  // development DATA_PATH is ./data, which is gitignored.
  const invitationsDir = path.resolve(process.cwd(), '..', 'data', 'invitations');
  fs.mkdirSync(invitationsDir, { recursive: true });

  const lines = ['> E2E'];
  for (let index = 0; index < userCount; index++)
    lines.push(`E2E Worker ${index} <e2e-${index}@example.com>`);
  lines.push('E2E Peer <e2e-peer@example.com>');

  fs.writeFileSync(
    path.join(invitationsDir, '_e2e.txt'),
    `${lines.join('\n')}\n`,
    'utf8',
  );
}
