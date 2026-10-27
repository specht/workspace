// node --test src/vscode-extensions/er/core/
const test = require('node:test');
const assert = require('node:assert/strict');
const path = require('path');
const { execFileSync } = require('child_process');
const fs = require('fs');
const os = require('os');
const core = require('./er-core.js');

// The model of the Filmdatenbank, written so that the generated tables match
// the dataset from src/scripts/collect-movies (same table and column names).
const FILM = `# Filmdatenbank
entity movie
  id: int, primary key
  title: text
  original_title: text, optional
  german_title: text, optional
  year: int
  runtime: int

entity genre
  id: int, primary key
  name: text, unique

entity crew
  id: int, primary key
  name: text
  birth_year: int
  death_year: int, optional

entity job
  id: int, primary key
  title: text, unique

relationship has_genre: movie n:m genre
relationship credit: movie, crew, job as movie_crew
`;

test('the Filmdatenbank becomes the tables of the dataset', () => {
    const { model, result } = core.compile(FILM);
    assert.deepEqual(model.errors, []);
    const names = result.tables.map((t) => t.name);
    assert.deepEqual(names, ['movie', 'genre', 'crew', 'job', 'movie_genre', 'movie_crew']);
    const mg = result.tables.find((t) => t.name === 'movie_genre');
    assert.deepEqual(mg.columns.map((c) => c.name), ['movie_id', 'genre_id']);
    assert.deepEqual(mg.pk, ['movie_id', 'genre_id']);
    assert.deepEqual(mg.fks, [
        { cols: ['movie_id'], refTable: 'movie', refCols: ['id'] },
        { cols: ['genre_id'], refTable: 'genre', refCols: ['id'] },
    ]);
    const mc = result.tables.find((t) => t.name === 'movie_crew');
    assert.deepEqual(mc.pk, ['movie_id', 'crew_id', 'job_id']);
    const movie = result.tables.find((t) => t.name === 'movie');
    assert.equal(movie.columns.find((c) => c.name === 'original_title').nullable, true);
    assert.equal(movie.columns.find((c) => c.name === 'runtime').type, 'INT');
    assert.deepEqual(result.tables.find((t) => t.name === 'genre').uniques, [['name']]);
});

test('SQL starts by dropping the tables and names its source', () => {
    const { result } = core.compile(FILM);
    const sql = core.toSQL(result.tables, { source: 'film.er' });
    assert.match(sql, /^-- Erzeugt aus film\.er mit der ER-Werkstatt/);
    assert.match(sql, /SET FOREIGN_KEY_CHECKS = 0;\nDROP TABLE IF EXISTS movie, genre, crew, job, movie_genre, movie_crew;\nSET FOREIGN_KEY_CHECKS = 1;/);
    assert.match(sql, /CREATE TABLE movie_genre \(\n  movie_id +INT NOT NULL,\n  genre_id +INT NOT NULL,\n  PRIMARY KEY \(movie_id, genre_id\)\n\);/);
    assert.match(sql, /ALTER TABLE movie_crew\n  ADD FOREIGN KEY \(job_id\)\n  REFERENCES job \(id\);/);
    assert.match(sql, /  name +VARCHAR\(255\) NOT NULL,\n  PRIMARY KEY \(id\),\n  UNIQUE \(name\)/);
    assert.doesNotMatch(core.toSQL(result.tables, { drop: false }), /DROP TABLE/);
});

test('1:n puts the foreign key on the n side, 1:1 makes it unique', () => {
    const { model, result } = core.compile(`entity movie
  id: int, primary key
entity copy
  number: int, primary key
relationship has: movie 1:n copy
entity class
  name: varchar(10), primary key
entity teacher
  short: varchar(5), primary key
relationship leads: teacher 1:1 class
`);
    assert.deepEqual(model.errors, []);
    const copy = result.tables.find((t) => t.name === 'copy');
    assert.deepEqual(copy.columns.map((c) => c.name), ['number', 'movie_id']);
    assert.equal(copy.columns[1].nullable, false);
    const cls = result.tables.find((t) => t.name === 'class');
    assert.deepEqual(cls.uniques, [['teacher_short']]);
    assert.ok(result.steps.some((s) => s.tag === '1:1'));
});

test('n:1 is the same as 1:n the other way round', () => {
    const a = core.compile('entity a\n  id: int, primary key\nentity b\n  id: int, primary key\nrelationship r: b n:1 a\n').result;
    assert.deepEqual(a.tables.find((t) => t.name === 'b').fks, [{ cols: ['a_id'], refTable: 'a', refCols: ['id'] }]);
});

test('a relationship of an entity with itself', () => {
    const { model, result } = core.compile(`entity person
  id: int, primary key
relationship follows: person n:m person
relationship mentor: person 1:n person
`);
    assert.deepEqual(model.errors, []);
    const follows = result.tables.find((t) => t.name === 'follows');
    assert.deepEqual(follows.columns.map((c) => c.name), ['from_person_id', 'to_person_id']);
    const person = result.tables.find((t) => t.name === 'person');
    const fk = person.columns.find((c) => c.name === 'mentor_person_id');
    assert.equal(fk.nullable, true, 'the first person could never be inserted otherwise');
});

test('relationship attributes go into the join table', () => {
    const { result } = core.compile(`entity customer
  id: int, primary key
entity copy
  number: int, primary key
relationship rents: customer n:m copy as rental
  start: datetime, primary key
  back: datetime, optional
`);
    const rental = result.tables.find((t) => t.name === 'rental');
    assert.deepEqual(rental.pk, ['customer_id', 'copy_number', 'start']);
    assert.equal(rental.columns.find((c) => c.name === 'back').nullable, true);
});

test('primary key is the key; key alone still works, with a hint', () => {
    const now = core.compile('entity movie\n  id: int, primary key\n  title: text\n');
    assert.deepEqual(now.model.errors, []);
    assert.deepEqual(now.model.warnings, []);
    assert.equal(now.model.entities[0].attrs[0].key, true);
    assert.deepEqual(now.result.tables[0].pk, ['id']);
    const old = core.compile('entity movie\n  id: int, key\n');
    assert.deepEqual(old.model.errors, []);
    assert.equal(old.model.entities[0].attrs[0].key, true);
    assert.match(old.model.warnings[0].msg, /Schreib »primary key« statt »key«/);
    assert.equal(old.model.warnings[0].line, 2);
    const loud = core.compile('entity movie\n  id: int, Primary Key\n  code: text, Key\n');
    assert.deepEqual(loud.model.errors.map((p) => p.msg).slice(0, 2), [
        '»Primary Key« schreibst du klein: »primary key«.',
        '»Key« schreibst du klein: »primary key«.',
    ]);
    const none = core.compile('entity movie\n  title: text\n');
    assert.match(none.model.errors[0].msg, /braucht einen Primärschlüssel \(»primary key«\)/);
});

test('problems are German, with line and columns', () => {
    const { model } = core.compile(`Entity movie
entity genre
  name: text
  id: Int, key
  year: number
relationship has_genre movie n:m genre
relationship x: movie n:m film
  stray: int
`);
    const msgs = model.errors.map((p) => `${p.line}: ${p.msg}`);
    assert.ok(msgs.some((m) => m.startsWith('1: Schlüsselwörter schreibst du klein')), msgs.join('\n'));
    assert.ok(msgs.some((m) => m.startsWith('4: »Int« schreibst du klein: »int«')), msgs.join('\n'));
    assert.ok(msgs.some((m) => m.startsWith('5: »number« ist weder ein Datentyp')), msgs.join('\n'));
    assert.ok(msgs.some((m) => m.startsWith('6: Nach dem Namen der Beziehung kommt ein Doppelpunkt')), msgs.join('\n'));
    assert.ok(msgs.some((m) => m.startsWith('7: Die Entität »film« ist nicht definiert')), msgs.join('\n'));
    const typo = model.errors.find((p) => p.line === 5);
    assert.deepEqual([typo.from, typo.to], [8, 14]);
});

test('every entity needs a key; a name is used once', () => {
    const { model } = core.compile('entity a\n  name: text\nentity a\n  id: int, primary key\n');
    const msgs = model.errors.map((p) => p.msg).join('\n');
    assert.match(msgs, /braucht einen Primärschlüssel/);
    assert.match(msgs, /gibt es schon/);
});

test('a clash between a join table and an entity is reported', () => {
    const { model } = core.compile('entity movie\n  id: int, primary key\nentity genre\n  id: int, primary key\nentity movie_genre\n  id: int, primary key\nrelationship g: movie n:m genre\n');
    assert.match(model.errors.map((p) => p.msg).join('\n'), /bräuchte eine Tabelle `movie_genre`.*»as«/);
});

test('summary for the checks', () => {
    const { model, result } = core.compile(FILM);
    const s = core.summary(model, result);
    assert.deepEqual(s.relationships.map((r) => [r.name, r.ends, r.cardinality, r.table]), [
        ['has_genre', ['movie', 'genre'], ['n', 'm'], 'movie_genre'],
        ['credit', ['movie', 'crew', 'job'], ['n', 'n', 'n'], 'movie_crew'],
    ]);
    assert.equal(s.entities[0].attributes[0].key, true);
});

test('diagrams: deterministic, kept positions, escaped labels', () => {
    for (const kind of ['chen', 'crowfoot', 'tables']) {
        const a = core.diagram(FILM, kind);
        const b = core.diagram(FILM, kind);
        assert.equal(a.svg, b.svg, `${kind} is deterministic`);
        assert.match(a.svg, /^<svg xmlns="http:\/\/www\.w3\.org\/2000\/svg" class="er-diagram" viewBox="[-\d]+ [-\d]+ \d+ \d+"/);
        // An SVG embedded in a page carries its <style> into the page: every
        // rule must stay inside the diagram (.row would hit Bootstrap's grid).
        const rules = a.svg.match(/<style>([\s\S]*?)<\/style>/)[1].split('\n').map((l) => l.trim()).filter(Boolean);
        assert.ok(rules.length > 10);
        for (const rule of rules) assert.match(rule, /^\.er-diagram \.[\w.-]+\{/);
        assert.equal((a.svg.match(/<g /g) || []).length, (a.svg.match(/<\/g>/g) || []).length);
    }
    const chen = core.diagram(FILM, 'chen');
    assert.match(chen.svg, /<polygon class="rel"[^>]*\/><text class="t tr" x="0" y="0">credit<\/text>/);
    // A dragged box stays where it was put.
    const moved = Object.assign({}, chen.positions, { 'E:movie': { x: 999, y: -999 } });
    const again = core.diagram(FILM, 'chen', { positions: moved });
    assert.deepEqual(again.positions['E:movie'], { x: 999, y: -999 });
    const tables = core.diagram(FILM, 'tables');
    assert.match(tables.svg, />movie_crew</);
    assert.match(tables.svg, /class="tag pk"[^>]*>PK, FK</);
    const tricky = core.diagram('entity Bücher\n  id: int, primary key\n', 'chen');
    assert.match(tricky.svg, />Bücher</);
    assert.equal(core.esc('<a href="x">'), '&lt;a href=&quot;x&quot;&gt;');
});

const SCHOOL = `entity student
  id: int, primary key
  name: text
entity course
  id: int, primary key
entity teacher
  id: int, primary key
entity room
  number: text, primary key
entity class
  id: int, primary key
relationship attends: student n:m course
relationship teaches: teacher 1:n course
relationship takes_place: course, room, teacher as schedule
relationship belongs_to: student n:1 class
relationship leads: teacher 1:1 class
`;

const overlapping = (nodes) => nodes.flatMap((a, i) => nodes.slice(i + 1)
    .filter((b) => Math.abs(a.x - b.x) < (a.w + b.w) / 2 && Math.abs(a.y - b.y) < (a.h + b.h) / 2)
    .map((b) => `${a.id} ${b.id}`));

test('crow\'s foot and tables in layers: what is referenced above, no box on another', () => {
    for (const text of [FILM, SCHOOL]) {
        for (const kind of ['crowfoot', 'tables']) {
            const { graph } = core.diagram(text, kind);
            assert.deepEqual(overlapping(graph.nodes), [], kind);
        }
    }
    const { graph } = core.diagram(FILM, 'tables');
    const at = Object.fromEntries(graph.nodes.map((n) => [n.label, n]));
    for (const [child, parent] of [['movie_genre', 'movie'], ['movie_genre', 'genre'], ['movie_crew', 'crew'], ['movie_crew', 'job']]) {
        assert.ok(at[parent].y < at[child].y, `${parent} above ${child}`);
    }
    // the three columns: type, name, key
    assert.match(core.diagram(FILM, 'tables').svg, /<text class="typ"[^>]*>int<\/text><text class="row"[^>]*>movie_id<\/text><text class="tag pk"[^>]*>PK, FK<\/text>/);
});

test('crow\'s foot: long lines go round, a kept box stays and a new one moves aside', () => {
    const { graph, positions, svg } = core.diagram(SCHOOL, 'tables');
    const long = graph.edges.filter((e) => e.via && e.via.length);
    assert.ok(long.length > 0, 'a line over two layers has stand-ins');
    assert.equal((svg.match(/<path class="cfl"/g) || []).length, graph.edges.length, 'one curve per line');
    // the positions of the last drawing passed back, as the preview does while typing: the same lines
    const typing = core.diagram(SCHOOL, 'tables', { positions });
    assert.equal(typing.svg, svg);
    // moved: no stand-ins any more for its lines
    const moved = Object.assign({}, positions, { [long[0].a]: { x: 5000, y: 5000 } });
    const again = core.diagram(SCHOOL, 'tables', { positions: moved });
    assert.deepEqual(again.positions[long[0].a], { x: 5000, y: 5000 });
    assert.doesNotMatch(again.svg, /NaN/);
    // a new table among kept ones does not land on one of them
    const grown = core.diagram(`${SCHOOL}entity lunch\n  id: int, primary key\nrelationship eats: student n:1 lunch\n`, 'tables', { positions });
    assert.deepEqual(overlapping(grown.graph.nodes), []);
    for (const [id, p] of Object.entries(positions)) assert.deepEqual(grown.positions[id], p);
});

test('crow\'s foot: two foreign keys to the same table get two lines', () => {
    const text = 'entity country\n  code: text, primary key\nrelationship borders: country n:m country\n';
    const { svg } = core.diagram(text, 'tables');
    const starts = [...svg.matchAll(/<path class="cfl" d="M([-\d.]+),([-\d.]+)/g)].map((m) => m.slice(1).join());
    assert.equal(starts.length, 2);
    assert.notEqual(starts[0], starts[1]);
});

test('schema lines: one per entity type or table, key underlined, foreign key with ↑', () => {
    const { model, result } = core.compile(FILM);
    const entities = core.schemaLines(model, result, 'entities');
    assert.deepEqual(entities.map((l) => l.name), ['movie', 'genre', 'crew', 'job']);
    assert.equal(core.schemaText(entities, 'html').split('</div>')[1], '<div><b>genre</b>(<u>id</u>, name)');
    const tables = core.schemaText(core.schemaLines(model, result, 'tables'), 'text').split('\n');
    assert.ok(tables.includes('movie_genre(↑m\u0332o\u0332v\u0332i\u0332e\u0332_\u0332i\u0332d\u0332, ↑g\u0332e\u0332n\u0332r\u0332e\u0332_\u0332i\u0332d\u0332)'));
    assert.ok(tables.includes('job(i\u0332d\u0332, title)'));
    const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'er-'));
    fs.writeFileSync(path.join(dir, 'film.er'), FILM);
    const out = execFileSync('node', [path.join(__dirname, 'er-core.js'), '--schema', 'entities', path.join(dir, 'film.er')], { encoding: 'utf8' });
    assert.equal(out.split('\n')[0], 'movie(i\u0332d\u0332, title, original_title, german_title, year, runtime)');
});

test('chen: attributes spread around their entity, clear of relationship lines', () => {
    const { graph } = core.diagram(FILM, 'chen');
    const byId = new Map(graph.nodes.map((n) => [n.id, n]));
    const angle = (from, to) => Math.atan2(to.y - from.y, to.x - from.x);
    const gap = (a, b) => { const d = Math.abs(a - b) % (2 * Math.PI); return Math.min(d, 2 * Math.PI - d); };
    for (const owner of graph.nodes.filter((n) => n.kind === 'E')) {
        const attrs = graph.nodes.filter((n) => n.owner === owner.id);
        const lines = graph.edges.filter((e) => e.kind === 'rel' && e.b === owner.id).map((e) => angle(owner, byId.get(e.a)));
        const angles = attrs.map((a) => angle(owner, a));
        // No attribute sits on a line to a relationship.
        for (const a of angles) for (const l of lines) assert.ok(gap(a, l) > 0.4, `${owner.label}: attribute on a line`);
        // No two attributes of one entity overlap.
        attrs.forEach((a, i) => attrs.slice(i + 1).forEach((b) => {
            assert.ok(Math.abs(a.x - b.x) >= (a.w + b.w) / 2 || Math.abs(a.y - b.y) >= (a.h + b.h) / 2, `${a.label} and ${b.label} overlap`);
        }));
    }
});

test('chen: attributes keep their order, clockwise from the top', () => {
    const { graph } = core.diagram('entity book\n  id: int, primary key\n  title: text\n  year: int\n  pages: int\n', 'chen');
    const book = graph.nodes.find((n) => n.id === 'E:book');
    const attrs = graph.nodes.filter((n) => n.kind === 'A');
    assert.deepEqual(attrs.map((a) => a.label), ['id', 'title', 'year', 'pages']);
    // Alone: evenly around the box, the first one on top.
    assert.ok(attrs[0].y < book.y && Math.abs(attrs[0].x - book.x) < 1);
    assert.ok(attrs[1].x > book.x && attrs[2].y > book.y && attrs[3].x < book.x);
});

test('chen: the two cardinalities of a relationship with itself do not overlap', () => {
    const svg = core.diagram('entity employee\n  id: int, primary key\nrelationship manages: employee 1:n employee\n', 'chen').svg;
    const cards = [...svg.matchAll(/<text class="card" x="([-\d.]+)" y="([-\d.]+)">/g)].map((m) => [Number(m[1]), Number(m[2])]);
    assert.equal(cards.length, 2);
    assert.ok(Math.hypot(cards[0][0] - cards[1][0], cards[0][1] - cards[1][1]) > 14);
});

test('command line: --json, --sql, --svg', () => {
    const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'er-'));
    const file = path.join(dir, 'film.er');
    fs.writeFileSync(file, FILM);
    const script = path.join(__dirname, 'er-core.js');
    const json = JSON.parse(execFileSync('node', [script, '--json', file], { encoding: 'utf8' }));
    assert.deepEqual(json.errors, []);
    assert.equal(json.tables.length, 6);
    const sql = execFileSync('node', [script, '--sql', file], { encoding: 'utf8' });
    assert.match(sql, /Erzeugt aus film\.er/);
    const svg = execFileSync('node', [script, '--svg', 'chen', file], { encoding: 'utf8' });
    assert.match(svg, /^<svg /);
    const missing = JSON.parse(execFileSync('node', [script, '--json', path.join(dir, 'nope.er')], { encoding: 'utf8' }));
    assert.equal(missing.missing, true);
    fs.writeFileSync(file, 'entity a\n');
    assert.throws(() => execFileSync('node', [script, '--sql', file], { stdio: 'pipe' }));
});

// The core names its version in every generated SQL file; it is the
// extension's version, so the two must not drift apart again.
test('the core version is the extension version', () => {
    const pkg = JSON.parse(fs.readFileSync(path.join(__dirname, '..', 'package.json'), 'utf8'));
    assert.equal(core.VERSION, pkg.version);
});
