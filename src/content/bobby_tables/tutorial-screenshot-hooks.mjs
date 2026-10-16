// Steps for »SQL Injection« (src/tutorial-screenshots/README.md,
// »Tutorial-specific hooks«). The page has no screenshots; its steps
// (<!-- tutorial-step -->) walk through it as a student would.
import fs from 'node:fs/promises';

const here = new URL('.', import.meta.url);

// Comments out the lines the page names and adds its code block behind
// them: »Kommentiere die Zeilen 21 bis 23 aus und füge dahinter ein:«.
async function applyFix(program, language) {
    const page = await fs.readFile(new URL('bobby_tables.md', here), 'utf8');
    const pattern = new RegExp('Kommentiere die Zeilen (\\d+) bis (\\d+) aus und füge dahinter ein:\\s*```' + language + '\\n([\\s\\S]*?)```');
    const match = page.match(pattern);
    if (!match) throw new Error(`hook: no fix for ${language} on the page`);
    const [first, last] = [Number(match[1]), Number(match[2])];
    const lines = program.split('\n');
    const commented = lines.map((line, i) => (i + 1 >= first && i + 1 <= last ? `# ${line}` : line));
    commented.splice(last, 0, ...match[3].replace(/\n$/, '').split('\n'));
    return commented.join('\n');
}

export default {
    actions: {
        // hook: test-database [fixed]: creates the test database in the
        // profile (»Weitere Datenbank erstellen«), as the page says, and
        // writes login.rb and login.py with its name and password filled in,
        // as a student copies them from the profile. The name also goes to
        // .testdb for the import. With fixed: the programs after »Die Lücke
        // schließen«, made from the page's own instructions.
        async 'test-database'({ page, args, writeFile }) {
            const call = (path, body = {}) => page.evaluate(async ({ path, body }) => {
                const response = await fetch(path, {
                    method: 'POST', headers: { 'Content-Type': 'application/json' }, body: JSON.stringify(body),
                });
                if (!response.ok) throw new Error(`${path}: HTTP ${response.status}`);
                return response.json();
            }, { path, body });
            let { result } = await call('/api/get_my_mysql_databases');
            let name = Object.keys(result.logins || {})[0];
            if (!name) {
                await call('/api/create_mysql_database');
                ({ result } = await call('/api/get_my_mysql_databases'));
                name = Object.keys(result.logins || {})[0];
            }
            if (!name) throw new Error('hook: test-database: no extra database in the profile');
            const password = result.logins[name].password;
            const fill = (text) => text.replace("'db_1234'", `'${name}'`).replace("'hier das Passwort'", `'${password}'`);
            let ruby = fill(await fs.readFile(new URL('login.rb', here), 'utf8'));
            let python = fill(await fs.readFile(new URL('login.py', here), 'utf8'));
            if (args.trim() === 'fixed') {
                ruby = await applyFix(ruby, 'ruby');
                python = await applyFix(python, 'python');
            }
            await writeFile('bobby_tables/login.rb', ruby);
            await writeFile('bobby_tables/login.py', python);
            await writeFile('bobby_tables/.testdb', `${name}\n`);
        },
    },
};
