import fs from 'node:fs';
import path from 'node:path';

const CONTENT_DIR = path.resolve(__dirname, '../../src/content');

/**
 * Read a source/example file from the tutorial itself so an E2E test types the
 * exact content shown to students instead of maintaining a duplicate copy.
 */
export function readTutorialFile(
  tutorial: string,
  filename: string,
): string {
  return fs
    .readFileSync(path.join(CONTENT_DIR, tutorial, filename), 'utf8')
    .trimEnd();
}
