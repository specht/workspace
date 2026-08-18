import assert from 'node:assert/strict';
import test from 'node:test';

import { resetScreenshotDatabases } from '../workspace-reset.js';

function response(status, body) {
    return {
        status,
        body: typeof body === 'string' ? body : JSON.stringify(body),
    };
}

function parseJsonResponse(result, label) {
    if (result.status < 200 || result.status >= 300) {
        throw new Error(`${label} failed (${result.status}): ${result.body}`);
    }
    return JSON.parse(result.body);
}

test('screenshot reset removes additional MySQL databases then resets MySQL and Neo4j', async () => {
    const calls = [];
    const browserRequest = async (pathname, options) => {
        calls.push([pathname, options.body]);
        if (pathname === '/api/get_my_mysql_databases') {
            return response(200, {
                result: { databases: ['student_db', 'db_alpha', 'db_beta'] },
            });
        }
        if (pathname === '/api/delete_mysql_database') return response(200, '');
        return response(200, { yay: 'sure' });
    };

    await resetScreenshotDatabases(browserRequest, parseJsonResponse);

    assert.deepEqual(calls, [
        ['/api/get_my_mysql_databases', {}],
        ['/api/delete_mysql_database', { database: 'db_alpha' }],
        ['/api/delete_mysql_database', { database: 'db_beta' }],
        ['/api/reset_mysql', {}],
        ['/api/reset_neo4j', {}],
    ]);
});

test('screenshot reset accepts a user with only the provisioned MySQL database', async () => {
    const calls = [];
    const browserRequest = async (pathname, options) => {
        calls.push([pathname, options.body]);
        if (pathname === '/api/get_my_mysql_databases') {
            return response(200, { result: { databases: ['student_db'] } });
        }
        return response(200, { yay: 'sure' });
    };

    await resetScreenshotDatabases(browserRequest, parseJsonResponse);

    assert.deepEqual(calls.map(([pathname]) => pathname), [
        '/api/get_my_mysql_databases',
        '/api/reset_mysql',
        '/api/reset_neo4j',
    ]);
});

test('screenshot reset stops before provisioning when an extra database deletion fails', async () => {
    const calls = [];
    const browserRequest = async (pathname, options) => {
        calls.push([pathname, options.body]);
        if (pathname === '/api/get_my_mysql_databases') {
            return response(200, {
                result: { databases: ['student_db', 'db_alpha'] },
            });
        }
        if (pathname === '/api/delete_mysql_database') {
            return response(500, 'delete failed');
        }
        return response(200, { yay: 'sure' });
    };

    await assert.rejects(
        resetScreenshotDatabases(browserRequest, parseJsonResponse),
        /MySQL database deletion \(db_alpha\) failed \(500\): delete failed/,
    );
    assert.deepEqual(calls.map(([pathname]) => pathname), [
        '/api/get_my_mysql_databases',
        '/api/delete_mysql_database',
    ]);
});
