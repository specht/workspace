# Collect streets

`collect-streets.py` builds the street data of »Ein Routenplaner für Berlin« from
OpenStreetMap: every way with a `highway` tag, from motorways to footpaths and
stairs, one line per way. It downloads a region from
[Geofabrik](https://download.geofabrik.de/) (cached in `cache/`) and writes the
whole region and the areas cut out of it to `street-archive/`.

```sh
pip install osmium
./collect-streets.py                                    # Berlin, Steglitz, Steglitz-Zehlendorf
./collect-streets.py --region europe/germany/hamburg --area altona=9.90,53.54,9.96,53.57
```

An area is a bounding box (`west,south,east,north`) and keeps every way with a
point inside it, whole. The format of a line is described at the top of the
script. Berlin is about 70 MB as text, so it is written compressed as well.

Data © OpenStreetMap contributors, [ODbL 1.0](https://opendatacommons.org/licenses/odbl/1-0/).
