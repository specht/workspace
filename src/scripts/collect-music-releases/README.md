# Collect music releases

`discogs_prepare.rb` builds a compact classroom music dataset from the public monthly
Discogs XML dumps. It replaces the old hand-maintained `wanted-artists.txt` workflow:
albums are selected automatically by a numeric popularity proxy.

Discogs' public dump does not contain an IMDb-style vote count. The builder therefore
uses the number of release versions attached to Discogs master releases. Each album
keeps its own `versions` value, while artist popularity is measured by summing the
version counts of that artist's qualifying albums.

By default, an individual album needs at least 10 versions and an artist needs at least
100 versions across those qualifying albums. This lets artists with a substantial
regional catalogue qualify even when no single album has an unusually large
international release history. Compilations, unofficial releases, reissues, remasters
and tour recordings are skipped.

Run:

```sh
./discogs_prepare.rb
```

The script finds the newest complete monthly dump, downloads and caches `artists`,
`masters` and `releases`, and writes the generated dataset to `discogs/`.

Useful options:

```sh
./discogs_prepare.rb --min-artist-versions 150
./discogs_prepare.rb --min-album-versions 15
./discogs_prepare.rb --min-versions 15
./discogs_prepare.rb --dump-date 20260801
./discogs_prepare.rb --force-download
./discogs_prepare.rb --output /path/to/discogs
```

`--min-versions` remains as a compatibility alias for `--min-album-versions`.

Generated files:

- `artists.txt`, `albums.txt`, `tracks.txt`, `genres.txt` — JSON Lines source data
- `mysql.sql` — complete MySQL schema and data import
- `neo4j.dump` — logical dump compatible with `neo4j_bolt load`
- `README.md` — generated provenance, schema and import documentation

The generated Neo4j graph contains `Artist`, `Album`, `Track` and `Genre` nodes with
`RELEASED`, `HAS_TRACK`, `IN_GENRE` and `MEMBER_OF` relationships. The MySQL schema
contains the equivalent `artist`, `album`, `track`, `genre`, `album_genre` and
`artist_part_of` tables.

The old helper scripts and `wanted-artists.txt` are no longer needed by the new builder.
