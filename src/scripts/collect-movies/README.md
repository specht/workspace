# Collect movies

`collect-movies.rb` builds the classroom movie dataset for *Filmdatenbank mit MySQL*
(`src/content/filmdatenbank`): films, their genres and the people who made them.

All published data comes from [Wikidata](https://www.wikidata.org/) (CC0 1.0). IMDb
is used only to choose the films: the script reads IMDb's public `title.ratings` and
`title.basics` files and keeps the IDs of non-adult feature films with at least
100,000 votes. It then finds those films in Wikidata by their IMDb ID (P345) and takes
titles, years, runtimes, genres, cast and crew from there. No IMDb data (ratings,
titles, names) ends up in the output; there is no `rating` column.

Run:

```sh
./collect-movies.rb                    # writes ./movie-archive
./collect-movies.rb --output /path/to/workspace-files/movie-archive
./collect-movies.rb --min-votes 50000  # more films
./collect-movies.rb --offline          # only cached Wikidata answers
```

The Wikidata Query Service is asked one batch at a time with a pause in between
(`--pause`, default 1 s) and a User-Agent that names this repository. Every answer is
cached in `.wikidata-cache/` next to the output, the IMDb files in `.downloads/`; a
second run, or a run after an interruption, only asks for what is missing. Delete
`.wikidata-cache/` to get fresh data from Wikidata.

Generated files (shared with `collect-music` via `../lib/classroom_dataset.rb`):

- `movies.txt`, `crew.txt`, `genres.txt` — JSON Lines, with Wikidata item IDs
- `mysql.sql` — schema and complete data
- `mysql-schema.sql`, `mysql-data.sql` — schema only and data only
- `mysql-data-movies.sql` (`genre`, `movie`, `movie_genre`) and `mysql-data-crew.sql`
  (`crew`, `job`, `movie_crew`) — data in two steps, as the tutorial builds its schema
- `neo4j.dump` — logical dump compatible with `neo4j_bolt load`
- `README.md` — provenance, licence and what each column means

The column lists are a contract with the tutorial: students build the tables from
their ER model (`src/content/filmdatenbank/film-2.er`) and then load the data files,
which name their columns. Change both together.

At the end the script prints a sanity summary (counts, films left out, people beyond
the limit per job, genres). Check it, and check by hand what the tutorial asks about
(*Back to the Future* from 1985, Christopher Nolan, the genre Animation, several people with
at least five films as director), before publishing to
https://github.com/specht/workspace-files under `movie-archive/` with
`../publish-datasets.rb movies` (it leaves out `.downloads/` and `.wikidata-cache/`).

Test without network: `ruby collect_movies_test.rb`.
