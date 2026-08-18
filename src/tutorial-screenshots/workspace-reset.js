function requireSuccessfulResponse(result, label) {
    if (result.status < 200 || result.status >= 300) {
        throw new Error(`${label} failed (${result.status}): ${result.body}`);
    }
}

export async function resetScreenshotDatabases(browserRequest, jsonResponse) {
    const mysqlState = jsonResponse(await browserRequest('/api/get_my_mysql_databases', {
        body: {},
    }), 'Tutorial screenshot MySQL database list');
    const mysqlDatabases = mysqlState?.result?.databases;
    if (!Array.isArray(mysqlDatabases) || mysqlDatabases.length === 0) {
        throw new Error('Tutorial screenshot MySQL database list was invalid');
    }

    // The normal database list always starts with the provisioned primary database;
    // any following entries are additional per-user databases created from the
    // profile. Remove only those extras through the application's normal endpoint.
    for (const database of mysqlDatabases.slice(1)) {
        const deleted = await browserRequest('/api/delete_mysql_database', {
            body: { database },
        });
        requireSuccessfulResponse(
            deleted,
            `Tutorial screenshot MySQL database deletion (${database})`,
        );
    }

    jsonResponse(await browserRequest('/api/reset_mysql', {
        body: {},
    }), 'Tutorial screenshot MySQL reset');
    jsonResponse(await browserRequest('/api/reset_neo4j', {
        body: {},
    }), 'Tutorial screenshot Neo4j reset');
}
