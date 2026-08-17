# Collect music releases

`collect-music.rb` builds a compact classroom music dataset from the public monthly
Discogs XML dumps. Artists are selected explicitly with `wanted-artists.txt`, so the
result does not depend on a popularity proxy that can favor internationally released
catalogues over artists that are especially relevant in Germany.

`wanted-artists.txt` contains one Discogs artist URL per line. Blank lines and lines
starting with `#` are ignored, so the list can be grouped and commented. Bare numeric
Discogs artist IDs are accepted as well.

For every wanted artist, the builder keeps all usable canonical single-artist albums.
Compilations, unofficial releases, reissues, remasters and tour recordings are skipped.
Albums also need a year, at least one genre and a usable track list because those fields
are required by the generated classroom dataset.

The number of Discogs release versions is still stored as `versions` on each album. It
is useful for sorting and exercises, but it no longer controls whether an artist or
album is included.

Run:

```sh
./collect-music.rb
```

The script finds the newest complete monthly dump, downloads and caches `artists`,
`masters` and `releases`, reads `wanted-artists.txt` next to the script, and writes the
generated dataset to `discogs/`.

Useful options:

```sh
./collect-music.rb --wanted-artists /path/to/wanted-artists.txt
./collect-music.rb --dump-date 20260801
./collect-music.rb --force-download
./collect-music.rb --output /path/to/discogs
```

Generated files:

- `artists.txt`, `albums.txt`, `tracks.txt`, `genres.txt` — JSON Lines source data
- `mysql.sql` — complete MySQL schema and data import
- `neo4j.dump` — logical dump compatible with `neo4j_bolt load`
- `README.md` — generated provenance, schema and import documentation

The generated Neo4j graph contains `Artist`, `Album`, `Track` and `Genre` nodes with
`RELEASED`, `HAS_TRACK`, `IN_GENRE` and `MEMBER_OF` relationships. The MySQL schema
contains the equivalent `artist`, `album`, `track`, `genre`, `album_genre` and
`artist_part_of` tables.

To change the collection, edit `wanted-artists.txt` and rebuild the dataset.
