# Collect movies

`collect-movies.rb` builds the classroom movie dataset (the *Filmdatenbank*) from the
public IMDb datasets at https://datasets.imdbws.com/. It keeps non-adult feature films
with a known year, runtime and genre and at least 100,000 votes, their genres, and the
people IMDb lists as principals (actors, directors, writers, producers, composers …).

Run:

```sh
./collect-movies.rb
```

Useful options:

```sh
./collect-movies.rb --output /path/to/videothek
./collect-movies.rb --min-votes 50000
./collect-movies.rb --force-download
```

Generated files:

- `movies.txt`, `crew.txt`, `genres.txt` — JSON Lines source data
- `mysql.sql` — schema and complete data
- `mysql-schema.sql`, `mysql-data.sql` — schema only and data only
- `mysql-data-movies.sql` (`genre`, `movie`, `movie_genre`) and `mysql-data-crew.sql`
  (`crew`, `job`, `movie_crew`) — data in two steps, as the Filmdatenbank tutorial
  builds its schema
- `neo4j.dump` — logical dump compatible with `neo4j_bolt load`
- `README.md` — generated provenance, schema and import documentation

The file names are shared with `collect-music` and written by `../lib/classroom_dataset.rb`.
It refuses to write a table with a duplicate primary key and prints a sanity summary
(counts, removed duplicate credits, dropped impossible death years, titles that occur
more than once) at the end of the build. Check that summary and the Git diff before
publishing a refresh to https://github.com/specht/videothek.
