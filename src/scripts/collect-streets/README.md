# Collect streets

`collect-streets.py` builds the street data of »Ein Routenplaner für Berlin« from
OpenStreetMap: every way with a `highway` tag, from motorways to footpaths and
stairs, one line per way. It downloads a region from
[Geofabrik](https://download.geofabrik.de/) (cached in `cache/`) and writes the
whole region and the areas cut out of it to `street-archive/`.

```sh
pip install osmium
./collect-streets.py                                    # Berlin and its twelve Bezirke
./collect-streets.py --region europe/germany/hamburg --area altona=9.90,53.54,9.96,53.57
```

For Berlin the areas are its twelve Bezirke, cut along their boundaries in
OpenStreetMap (`admin_level=9`, drawn into a grid of about 14 × 22 m); other
areas are bounding boxes (`west,south,east,north`). An area keeps every way
with a point inside it, whole. Ferries count as ways (kind `ferry`), since
some places, like the Pfaueninsel, can only be reached by one. The format of
a line is described at the top of the script and in the `README.md` it
writes next to the files.

Next to each file of ways, `NAME-places.tsv` has the places to start from and
go to: chosen sights, every named square (`place=square`) and every school.
Never addresses.

Every file of ways is written compressed as well (Berlin is 70 MB as text, a
Bezirk 3 to 7 MB). `src/scripts/publish-datasets.rb streets` publishes the
compressed files and the places to `specht/workspace-files`, where the
template's `server.rb` downloads a Bezirk the first time it is asked for one.

Data © OpenStreetMap contributors, [ODbL 1.0](https://opendatacommons.org/licenses/odbl/1-0/).
