// The engine of the query stepper (src/static/include/query_stepper.js)
// against what MySQL answers for the same queries on terra1.
//   node --test test/static/*.test.cjs
const test = require('node:test');
const assert = require('node:assert');
const path = require('node:path');
const fs = require('node:fs');
const QS = require('../../src/static/include/query_stepper.js');

const data = JSON.parse(fs.readFileSync(path.join(__dirname, '../../src/static/query-stepper/terra1.json')));
const result = (sql) => QS.run(sql, data).result;
const clauses = (sql) => QS.run(sql, data).steps.map((s) => s.clause);
const fails = (sql, pattern) => assert.throws(() => QS.run(sql, data), (e) => e instanceof QS.SqlError && pattern.test(e.message));

test('the queries of the SQL introduction', () => {
    assert.strictEqual(result('SELECT * FROM land;').rows.length, 197);
    assert.deepStrictEqual(result("SELECT name, hauptstadt FROM land WHERE name = 'Deutschland';").rows[0].cells, ['Deutschland', 'Berlin']);
    // Text compares without regard to case, as latin1_german1_ci does
    assert.strictEqual(result("SELECT * FROM land WHERE name = 'deutschland'").rows.length, 1);
    assert.deepStrictEqual(result('SELECT COUNT(*) FROM land;').rows[0].cells, ['197']);
    assert.deepStrictEqual(result('SELECT * FROM land LIMIT 5;').rows.length, 5);
    const sorted = result('SELECT name, einwohner FROM land ORDER BY einwohner DESC LIMIT 2').rows.map((r) => r.cells[0]);
    assert.deepStrictEqual(sorted, ['China', 'Indien']);
    const like = result("SELECT name FROM land WHERE name LIKE '%land%' ORDER BY name").rows.map((r) => r.cells[0]);
    assert.ok(like.includes('Deutschland') && like.every((n) => /land/i.test(n)));
    const continents = result('SELECT DISTINCT kontinent FROM land ORDER BY kontinent').rows.map((r) => r.cells[0]);
    assert.deepStrictEqual(continents, [...new Set(continents)]);
    assert.ok(continents.length >= 6 && continents.length <= 8);
});

test('the steps come in the order the database works', () => {
    assert.deepStrictEqual(
        clauses('SELECT DISTINCT kontinent, COUNT(*) AS n FROM land WHERE einwohner > 1 GROUP BY kontinent HAVING COUNT(*) > 5 ORDER BY n DESC LIMIT 3'),
        ['FROM', 'WHERE', 'GROUP BY', 'HAVING', 'SELECT', 'DISTINCT', 'ORDER BY', 'LIMIT']);
    assert.deepStrictEqual(clauses('SELECT o.name FROM ort o JOIN land l ON o.land = l.name'), ['FROM', 'JOIN', 'SELECT']);
});

test('groups, HAVING and aliases', () => {
    const rows = result('SELECT kontinent, COUNT(*) AS laender FROM land GROUP BY kontinent HAVING laender > 40 ORDER BY laender DESC').rows;
    assert.ok(rows.length >= 2 && rows.every((r) => Number(r.cells[1]) > 40));
    assert.ok(Number(rows[0].cells[1]) >= Number(rows[1].cells[1]));
    const total = result('SELECT kontinent, COUNT(*) FROM land GROUP BY kontinent').rows.reduce((s, r) => s + Number(r.cells[1]), 0);
    assert.strictEqual(total, 197);
});

test('JOIN and LEFT JOIN', () => {
    const europe = result("SELECT o.name, l.name FROM ort o JOIN land l ON o.land = l.name WHERE l.kontinent = 'Europa' ORDER BY o.einwohner DESC LIMIT 3").rows;
    assert.strictEqual(europe.length, 3);
    assert.ok(europe.every((r) => r.cells.length === 2));
    const withNone = result('SELECT l.name, o.name FROM land l LEFT JOIN ort o ON o.land = l.name WHERE o.name IS NULL').rows;
    assert.ok(withNone.length > 0 && withNone.every((r) => r.cells[1] === 'NULL'));
});

test('NULL is neither true nor false', () => {
    assert.strictEqual(result('SELECT * FROM land WHERE NULL = NULL').rows.length, 0);
    assert.strictEqual(result('SELECT * FROM land WHERE NOT (NULL = 1)').rows.length, 0);
});

test('mistakes MySQL also rejects, with an explanation', () => {
    fails('SELECT einwohner / flaeche AS dichte FROM land WHERE dichte > 0.0001', /SELECT kommt nach WHERE/);
    fails('SELECT name FROM land WHERE COUNT(*) > 3', /HAVING/);
    fails('SELECT kontinent, name FROM land GROUP BY kontinent', /weder in GROUP BY/);
    fails('SELECT name FROM ort o JOIN land l ON o.land = l.name', /mehrdeutig/);
    fails('SELECT * FROM laender', /gibt es nicht/);
    fails('SELECT * FROM land ORDER BY name WHERE kontinent = \'Asien\'', /falschen Stelle/);
    fails("SELECT * FROM land WHERE name = 'Deutschland", /hört nicht auf/);
});

// The queries of »Filmdatenbank« on the movie tables; values as MySQL gives
// them on the full data (directors are complete in the stepper's tables).
const movies = JSON.parse(fs.readFileSync(path.join(__dirname, '../../src/static/query-stepper/movies.json')));
const films = (sql) => QS.run(sql, movies).result.rows.map((r) => r.cells.join(' | '));

test('the movie tables, as in Filmdatenbank', () => {
    assert.deepStrictEqual(films("SELECT movie.title, genre.name FROM movie JOIN movie_genre ON movie.id = movie_genre.movie_id JOIN genre ON movie_genre.genre_id = genre.id WHERE movie.title = 'Back to the Future' ORDER BY genre.name"),
        ['Back to the Future | Adventure', 'Back to the Future | Comedy', 'Back to the Future | Sci-Fi']);
    assert.ok(films('SELECT genre.name, COUNT(*) FROM genre JOIN movie_genre ON genre.id = movie_genre.genre_id GROUP BY genre.id, genre.name').includes('Drama | 1361'));
    assert.deepStrictEqual(films("SELECT crew.name FROM movie JOIN movie_crew ON movie.id = movie_crew.movie_id JOIN crew ON movie_crew.crew_id = crew.id JOIN job ON movie_crew.job_id = job.id WHERE movie.title = 'Back to the Future' AND job.title = 'director'"),
        ['Robert Zemeckis']);
    assert.deepStrictEqual(films('SELECT COUNT(*) FROM movie m LEFT JOIN movie_genre mg ON mg.movie_id = m.id WHERE mg.movie_id IS NULL'), ['7']);
});

test('a self-join with many rows stays fast enough', () => {
    const start = Date.now();
    const top = films("SELECT a.name, b.name, COUNT(*) AS together FROM movie_crew AS ma JOIN movie_crew AS mb ON ma.movie_id = mb.movie_id JOIN job AS ja ON ma.job_id = ja.id JOIN job AS jb ON mb.job_id = jb.id JOIN crew AS a ON ma.crew_id = a.id JOIN crew AS b ON mb.crew_id = b.id WHERE ja.title = 'actor' AND jb.title = 'actor' AND a.id < b.id GROUP BY a.id, a.name, b.id, b.name ORDER BY together DESC, a.name, b.name LIMIT 3");
    assert.deepStrictEqual(top, ['Lois Maxwell | Desmond Llewelyn | 12', 'Rob Schneider | Adam Sandler | 12', 'Bernard Lee | Lois Maxwell | 11']);
    assert.ok(Date.now() - start < 5000, `took ${Date.now() - start} ms`);
});

test('the schema lines, as _schema(name) on a page draws them', () => {
    const html = QS.schemaHtml(data.tables);
    assert.ok(html.includes("<span class='schema-table'>land</span>(<u>LNR</u>, Name, Einwohner"));
    assert.ok(html.includes("<span class='schema-table'>ort</span>(Name, <span class='schema-fk' title='verweist auf land.Name'><span class='schema-arrow'>↑</span>Land</span>, Einwohner"));
    // the same markup as the Ruby side (src/ruby/include/schemas.rb, test/schemas_test.rb)
    const movieGenre = { movie_genre: { columns: ['movie_id', 'genre_id'], key: ['movie_id', 'genre_id'], references: { movie_id: 'movie.id', genre_id: 'genre.id' } } };
    assert.strictEqual(QS.schemaHtml(movieGenre), "<div><span class='schema-table'>movie_genre</span>(" +
        "<u><span class='schema-fk' title='verweist auf movie.id'><span class='schema-arrow'>↑</span>movie_id</span></u>, " +
        "<u><span class='schema-fk' title='verweist auf genre.id'><span class='schema-arrow'>↑</span>genre_id</span></u>)</div>");
});
