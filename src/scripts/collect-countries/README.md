# Collect countries

`collect-countries.rb` builds the classroom country dataset for *Einführung in SQL*
(`src/content/mysql`) and the Query Stepper: the continents, the countries of the world,
their cities, which countries share a land border, their currencies and official
languages, in a basic version for the first part of the tutorial and a full one for
the second.

All data comes from [Wikidata](https://www.wikidata.org/) (CC0 1.0). The countries are
the 193 members of the United Nations, the two observer states, Kosovo and Taiwan; the
cities every human settlement with at least 100,000 inhabitants, and every capital.

Run:

```sh
./collect-countries.rb                    # writes ./country-archive
./collect-countries.rb --output /path/to/workspace-files/country-archive
./collect-countries.rb --offline          # only cached Wikidata answers
```

The Wikidata Query Service is asked with a pause in between (`--pause`, default 1 s)
and a User-Agent that names this repository. Every answer is cached in
`.wikidata-cache/` next to the output; a second run only asks for what is missing.
Delete `.wikidata-cache/` to get fresh data. A first run takes some minutes.

What a city is decides most of the code. Wikidata calls many things a human
settlement: metropolitan areas, boroughs, counties, the metropolitan cities of Italy.
The rules, in the order the script applies them, are in `load_cities`, and the
generated README lists them for the reader. When a new build shows a strange city,
look up its classes (P31) and add the class to `NOT_A_CITY`, or, if it is a single
item, to `NOT_A_CITY_ITEM`. Change a class only as that class: Wikidata's tree of
classes is not to be trusted (»city in the United States« is a subclass of »urban area
of the United States«).

Generated files (shared with `collect-movies` and `collect-music` via
`../lib/classroom_dataset.rb`):

- `continents.txt`, `countries.txt`, `cities.txt` — JSON Lines, with Wikidata item IDs
- `countries-basic.sql` — the basic version for the first steps: `continent`, `country`
  (its first five columns) and `city`, schema and data
- `countries.sql` — schema and complete data: also `border`, `currency`, `language`,
  `country_language`
- `countries-schema.sql`, `countries-data.sql` — schema only and data only
- `countries-data-cities.sql` (`continent`, `currency`, `country`, `city`),
  `countries-data-borders.sql` and `countries-data-languages.sql`
  (`border`) — data in two steps
- `countries-neo4j.dump` — logical dump compatible with `neo4j_bolt load`
- `README.md` — provenance, licence, the rules and what each column means

The tables and columns are a contract with the Einführung in SQL, its tasks
(`src/content/mysql/tasks.yaml`) and the Query Stepper
(`src/scripts/query-stepper-data.rb countries countries.sql` and `countries-basic
countries-basic.sql`). Change them together.
So are the numbers: the tasks expect the published row counts (`tables: { city: … }`)
and name results in their solutions, and the pictures of the tutorial show them. A
new build, published, needs the tasks, the stepper's tables and the screenshots
renewed with it; students who loaded the old build load the new one.

At the end the script prints a sanity summary (counts, countries without capital or
neighbours, Germany) and notes. Check it, and check by hand what the tutorial asks
about, before publishing to https://github.com/specht/workspace-files under
`country-archive/` with `../publish-datasets.rb countries` (it leaves out
`.wikidata-cache/` and refuses a build whose row counts the tasks do not expect).

Test without network: `ruby collect_countries_test.rb`.
