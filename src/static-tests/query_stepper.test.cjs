// The engine of the query stepper (src/static/include/query_stepper.js)
// against what MySQL answers for the same queries on the country dataset
// (country-archive/countries.sql).
//   node --test src/static-tests/*.test.cjs
const test = require('node:test');
const assert = require('node:assert');
const path = require('node:path');
const fs = require('node:fs');
const QS = require('../static/include/query_stepper.js');

const data = JSON.parse(fs.readFileSync(path.join(__dirname, '../static/query-stepper/countries.json')));
const result = (sql) => QS.run(sql, data).result;
const values = (sql) => result(sql).rows.map((r) => r.cells.join(' | '));
const clauses = (sql) => QS.run(sql, data).steps.map((s) => s.clause);
const fails = (sql, pattern) => assert.throws(() => QS.run(sql, data), (e) => e instanceof QS.SqlError && pattern.test(e.message));

test('the queries of the SQL introduction', () => {
    assert.strictEqual(result('SELECT * FROM country;').rows.length, 197);
    assert.deepStrictEqual(values("SELECT name, population FROM country WHERE name = 'Deutschland';"), ['Deutschland | 83467117']);
    // Text compares without regard to case, as utf8mb4_unicode_ci does
    assert.deepStrictEqual(values("SELECT name FROM country WHERE name = 'deutschland'"), ['Deutschland']);
    assert.deepStrictEqual(values('SELECT COUNT(*) FROM country;'), ['197']);
    assert.strictEqual(result('SELECT * FROM city LIMIT 5;').rows.length, 5);
    assert.deepStrictEqual(values('SELECT name FROM country ORDER BY population DESC LIMIT 3'), ['China', 'Indien', 'Vereinigte Staaten']);
    // Ä sorts as A
    assert.deepStrictEqual(values('SELECT name FROM country ORDER BY name LIMIT 5'), ['Afghanistan', 'Ägypten', 'Albanien', 'Algerien', 'Andorra']);
    assert.deepStrictEqual(values("SELECT name FROM country WHERE name LIKE '%land%' ORDER BY name"),
        ['Deutschland', 'Estland', 'Finnland', 'Griechenland', 'Irland', 'Island', 'Lettland', 'Neuseeland', 'Niederlande', 'Russland', 'Thailand']);
    assert.deepStrictEqual(values('SELECT DISTINCT continent FROM country ORDER BY continent'),
        ['Afrika', 'Asien', 'Europa', 'Nordamerika', 'Ozeanien', 'Südamerika']);
    assert.deepStrictEqual(values("SELECT COUNT(*) FROM city WHERE country_code = 'DEU'"), ['79']);
});

// Each step knows where its clause stands in the text, for the widget to mark it
test('every step marks its own clause in the query', () => {
    const sql = "SELECT DISTINCT continent, COUNT(*) AS n\nFROM country AS c\nLEFT JOIN border ON border.country_code = c.code\nWHERE population > 1\nGROUP BY continent HAVING COUNT(*) > 5\nORDER BY n DESC LIMIT 3;";
    const marked = QS.run(sql, data).steps.map((s) => [s.clause, sql.slice(s.span[0], s.span[1])]);
    assert.deepStrictEqual(marked, [
        ['FROM', 'FROM country AS c'],
        ['LEFT JOIN', 'LEFT JOIN border ON border.country_code = c.code'],
        ['WHERE', 'WHERE population > 1'],
        ['GROUP BY', 'GROUP BY continent'],
        ['HAVING', 'HAVING COUNT(*) > 5'],
        ['SELECT', 'SELECT DISTINCT continent, COUNT(*) AS n'],
        ['DISTINCT', 'DISTINCT'],
        ['ORDER BY', 'ORDER BY n DESC'],
        ['LIMIT', 'LIMIT 3'],
    ]);
});

test('the steps come in the order the database works', () => {
    assert.deepStrictEqual(
        clauses('SELECT DISTINCT continent, COUNT(*) AS n FROM country WHERE population > 1 GROUP BY continent HAVING COUNT(*) > 5 ORDER BY n DESC LIMIT 3'),
        ['FROM', 'WHERE', 'GROUP BY', 'HAVING', 'SELECT', 'DISTINCT', 'ORDER BY', 'LIMIT']);
    assert.deepStrictEqual(clauses('SELECT city.name FROM city JOIN country ON city.country_code = country.code'), ['FROM', 'JOIN', 'SELECT']);
});

test('groups, HAVING and aliases', () => {
    assert.deepStrictEqual(values('SELECT continent, COUNT(*) AS n FROM country GROUP BY continent HAVING n > 40 ORDER BY n DESC'),
        ['Afrika | 54', 'Asien | 49', 'Europa | 45']);
    const total = result('SELECT continent, COUNT(*) FROM country GROUP BY continent').rows.reduce((s, r) => s + Number(r.cells[1]), 0);
    assert.strictEqual(total, 197);
});

test('JOIN and LEFT JOIN', () => {
    assert.deepStrictEqual(values("SELECT c.name, k.name FROM city c JOIN country k ON c.country_code = k.code WHERE k.continent = 'Europa' ORDER BY c.population DESC LIMIT 3"),
        ['Moskau | Russland', 'London | Vereinigtes Königreich', 'Sankt Petersburg | Russland']);
    assert.deepStrictEqual(values('SELECT k.name, c.name FROM country k JOIN city c ON c.country_code = k.code AND c.is_capital WHERE k.code IN (\'DEU\', \'FRA\') ORDER BY k.name'),
        ['Deutschland | Berlin', 'Frankreich | Paris']);
    assert.deepStrictEqual(values('SELECT ct.name, k.name FROM continent ct LEFT JOIN country k ON k.continent = ct.name WHERE k.code IS NULL'), ['Antarktika | NULL']);
    assert.deepStrictEqual(values('SELECT COUNT(*) FROM country k LEFT JOIN border b ON b.country_code = k.code WHERE b.country_code IS NULL'), ['39']);
    // the neighbours: border joined with country a second time
    assert.deepStrictEqual(values("SELECT n.name FROM border b JOIN country n ON n.code = b.neighbor_code WHERE b.country_code = 'DEU' ORDER BY n.name"),
        ['Belgien', 'Dänemark', 'Frankreich', 'Luxemburg', 'Niederlande', 'Österreich', 'Polen', 'Schweiz', 'Tschechien']);
    // currencies (n:1) and official languages (n:m)
    assert.deepStrictEqual(values("SELECT currency.name, COUNT(*) AS n FROM country JOIN currency ON currency.code = country.currency_code GROUP BY currency.code, currency.name ORDER BY n DESC, currency.name LIMIT 3"),
        ['Euro | 27', 'CFA-Franc BCEAO | 8', 'US-Dollar | 7']);
    assert.deepStrictEqual(values("SELECT language.name FROM country_language JOIN language ON language.code = country_language.language_code WHERE country_language.country_code = 'CHE' ORDER BY language.name"),
        ['Bündnerromanisch', 'Deutsch', 'Französisch', 'Italienisch']);
    assert.deepStrictEqual(values("SELECT COUNT(*) FROM country WHERE driving_side = 'left'"), ['54']);
});

test('NULL is neither true nor false', () => {
    assert.strictEqual(result('SELECT * FROM country WHERE NULL = NULL').rows.length, 0);
    assert.strictEqual(result('SELECT * FROM country WHERE NOT (NULL = 1)').rows.length, 0);
});

test('mistakes MySQL also rejects, with an explanation', () => {
    fails('SELECT population / area AS density FROM country WHERE density > 100', /SELECT kommt nach WHERE/);
    fails('SELECT name FROM country WHERE COUNT(*) > 3', /HAVING/);
    fails('SELECT continent, name FROM country GROUP BY continent', /weder in GROUP BY/);
    fails('SELECT name FROM city JOIN country ON city.country_code = country.code', /mehrdeutig/);
    fails('SELECT * FROM countries', /gibt es nicht/);
    fails('SELECT * FROM country ORDER BY name WHERE continent = \'Asien\'', /falschen Stelle/);
    fails("SELECT * FROM country WHERE name = 'Deutschland", /hört nicht auf/);
});

// The queries of »Filmdatenbank« on the movie tables; values as MySQL gives
// them on the full data (directors are complete in the stepper's tables).
const movies = JSON.parse(fs.readFileSync(path.join(__dirname, '../static/query-stepper/movies.json')));
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
    assert.ok(html.includes("<span class='schema-table'>country</span>(<u>code</u>, name, population, area, <span class='schema-fk' title='verweist auf continent.name'><span class='schema-arrow'>↑</span>continent</span>, " +
        "<span class='schema-fk' title='verweist auf currency.code'><span class='schema-arrow'>↑</span>currency_code</span>, tld, calling_code, driving_side)"));
    assert.ok(html.includes("<span class='schema-table'>border</span>(<u><span class='schema-fk' title='verweist auf country.code'><span class='schema-arrow'>↑</span>country_code</span></u>, <u><span class='schema-fk' title='verweist auf country.code'><span class='schema-arrow'>↑</span>neighbor_code</span></u>)"));
    // the same markup as the Ruby side (src/ruby/include/schemas.rb, test/schemas_test.rb)
    const movieGenre = { movie_genre: { columns: ['movie_id', 'genre_id'], key: ['movie_id', 'genre_id'], references: { movie_id: 'movie.id', genre_id: 'genre.id' } } };
    assert.strictEqual(QS.schemaHtml(movieGenre), "<div><span class='schema-table'>movie_genre</span>(" +
        "<u><span class='schema-fk' title='verweist auf movie.id'><span class='schema-arrow'>↑</span>movie_id</span></u>, " +
        "<u><span class='schema-fk' title='verweist auf genre.id'><span class='schema-arrow'>↑</span>genre_id</span></u>)</div>");
});
