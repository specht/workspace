#!/usr/bin/env python3
"""The streets and paths of a region from OpenStreetMap, for »Ein Routenplaner
für Berlin«: every way with a highway tag (from motorways to footpaths and
stairs), one line per way, and areas cut out of it by bounding box.

    pip install osmium
    ./collect-streets.py                               # Berlin and the areas below
    ./collect-streets.py --region europe/germany/hamburg --area altona=9.90,53.54,9.96,53.57

The region comes from download.geofabrik.de (cached in cache/); the files go
to street-archive/. Each line of a file is one way, tab-separated:

    kind      the highway tag: residential, footway, primary, steps, …, or ferry
    oneway    yes (only in the direction of the points), -1 (only against it), or empty
    name      the name of the street, or empty
    points    "ID LON,LAT ID LON,LAT …": the nodes of the way in order; ways
              that meet share the ID of the node where they meet

For Berlin the areas are its twelve Bezirke, cut along their boundaries in
OpenStreetMap (a way belongs to every Bezirk one of its points lies in):
steglitz-zehlendorf.tsv, mitte.tsv, … Other areas are bounding boxes.

Next to each file of ways, NAME-places.tsv has the places to start from and
go to: public places only (sights, stations, parks, squares), and every
school with a name. Never addresses: nobody should plan the way to their own
home. A line:

    kind      sight, square or school
    name      what the place is called
    position  LON,LAT

OpenStreetMap data © OpenStreetMap contributors, ODbL 1.0.
"""
import argparse
import datetime
import gzip
import math
import os
import sys
import urllib.request
from collections import defaultdict

import osmium

HERE = os.path.dirname(os.path.abspath(__file__))
# Not a way to travel on: planned or under construction, platforms, race tracks
SKIP = {'construction', 'proposed', 'abandoned', 'platform', 'raceway', 'bus_stop', 'rest_area',
        'services', 'elevator', 'emergency_bay', 'escape', 'traffic_sign', 'street_lamp', 'corridor'}
# The twelve Bezirke of Berlin, as their boundaries are called in OpenStreetMap
BEZIRKE = ['Mitte', 'Friedrichshain-Kreuzberg', 'Pankow', 'Charlottenburg-Wilmersdorf', 'Spandau',
           'Steglitz-Zehlendorf', 'Tempelhof-Schöneberg', 'Neukölln', 'Treptow-Köpenick',
           'Marzahn-Hellersdorf', 'Lichtenberg', 'Reinickendorf']
# The grid the Bezirke are drawn into, in degrees: about 14 m by 22 m
CELL = 0.0002


def slug(name):
    for a, b in (('ä', 'ae'), ('ö', 'oe'), ('ü', 'ue'), ('ß', 'ss')):
        name = name.lower().replace(a, b)
    return name


# The sights: the name in OpenStreetMap, a tag that picks the right object of
# that name (»Brandenburger Tor« is also a station and a taxi rank), for a
# name that is there twice a rough position near the one meant, and a name to
# show instead if the one in OpenStreetMap says too little
SIGHTS = [
    ('Brandenburger Tor', 'building=gate'), ('Berliner Fernsehturm', 'man_made=communications_tower'),
    ('Reichstagsgebäude', 'building'), ('Rotes Rathaus', 'amenity=townhall'), ('Berliner Dom', 'building=cathedral'),
    ('Siegessäule', 'historic=monument'), ('Schloss Charlottenburg', 'historic=castle'), ('Schloss Bellevue', 'historic=castle'),
    ('Checkpoint Charlie', 'tourism=attraction'), ('Gendarmenmarkt', 'place=square'),
    ('Denkmal für die ermordeten Juden Europas', 'historic=monument'), ('Museumsinsel', 'tourism=attraction'),
    ('Museum für Naturkunde', 'tourism=museum'), ('Humboldt Forum', 'tourism=museum'),
    ('Kaiser-Wilhelm-Gedächtniskirche', 'historic=memorial'), ('Zoo Berlin', 'tourism=zoo'), ('Tierpark Berlin', 'tourism=zoo'),
    ('Mauerpark', 'leisure=park'), ('Treptower Park', 'leisure=park'), ('Viktoriapark', 'leisure=park', (13.381, 52.488)),
    ('Tempelhofer Feld', 'landuse=recreation_ground'), ('Olympiastadion Berlin', 'leisure=stadium'), ('Teufelsberg', 'tourism=viewpoint'),
    ('Gärten der Welt', 'tourism=theme_park'), ('Pfaueninsel', 'place=island'), ('Strandbad Wannsee', 'leisure=sports_centre'),
    ('East Side Gallery', 'tourism=information'), ('Botanischer Garten', 'leisure=park', (13.306, 52.456)),
    ('Bierpinsel', 'building=tower'), ('Titania-Palast', 'amenity=cinema'), ('Rathaus Steglitz', 'building=government'),
    ('Rathaus Zehlendorf', 'amenity=townhall'), ('Berlin Hauptbahnhof', 'railway=station'),
    ('Alexanderplatz', 'place=square'), ('Potsdamer Platz', 'railway=station'),
    ('Wannsee', 'railway=station', None, 'Bahnhof Wannsee'),
]
# Squares in OpenStreetMap that are no squares to go to
NOT_SQUARES = {'Sitzplatz'}


class Places(osmium.SimpleHandler):
    """Sights, squares and schools: nodes, ways and areas of several parts,
    each at the middle of its points; and the boundaries of the Bezirke"""

    def __init__(self, bezirke=()):
        super().__init__()
        self.found = {}  # name -> [(lon, lat)] of the objects that fit
        self.schools = {}  # (name, rounded position) -> (name, lon, lat)
        self.squares = {}  # the same for squares
        self.bezirke = {name: [] for name in bezirke}  # name -> its rings, [(lon, lat), …] each

    def check(self, tags, lon, lat):
        name = tags.get('name')
        if not name:
            return
        if tags.get('amenity') == 'school':
            self.schools.setdefault((name, round(lon, 3), round(lat, 3)), (name, lon, lat))
        if tags.get('place') == 'square' and name not in NOT_SQUARES:
            self.squares.setdefault((name, round(lon, 3), round(lat, 3)), (name, lon, lat))
        for sight in SIGHTS:
            if sight[0] != name:
                continue
            key, _, value = sight[1].partition('=')
            if key in tags and (not value or tags[key] == value):
                self.found.setdefault(name, []).append((lon, lat))

    def node(self, n):
        self.check(n.tags, n.location.lon, n.location.lat)

    def way(self, w):
        if 'highway' in w.tags or not len(w.nodes):
            return
        self.check(w.tags, sum(n.lon for n in w.nodes) / len(w.nodes), sum(n.lat for n in w.nodes) / len(w.nodes))

    def area(self, a):
        name = a.tags.get('name')
        if name in self.bezirke and a.tags.get('boundary') == 'administrative' and a.tags.get('admin_level') == '9':
            for outer in a.outer_rings():
                self.bezirke[name].append([(n.lon, n.lat) for n in outer])
                for inner in a.inner_rings(outer):
                    self.bezirke[name].append([(n.lon, n.lat) for n in inner])
        if a.from_way():
            return  # the way itself was already looked at
        points = [(n.lon, n.lat) for ring in a.outer_rings() for n in ring]
        if points:
            self.check(a.tags, sum(p[0] for p in points) / len(points), sum(p[1] for p in points) / len(points))

    def places(self):
        out = []
        for sight in SIGHTS:
            name, near = sight[0], (sight[2] if len(sight) > 2 else None)
            shown = sight[3] if len(sight) > 3 else name
            hits = self.found.get(name)
            if not hits:
                print(f'Not found: {name}', file=sys.stderr)
                continue
            lon, lat = min(hits, key=lambda p: (p[0] - near[0]) ** 2 + (p[1] - near[1]) ** 2) if near else hits[0]
            out.append(('sight', shown, lon, lat))
        out += [('square', name, lon, lat) for name, lon, lat in sorted(self.squares.values())]
        out += [('school', name, lon, lat) for name, lon, lat in sorted(self.schools.values())]
        return out


class Grid:
    """Areas drawn into a grid of CELL degrees: which area a point lies in.
    Each row is filled between the crossings of its middle with the rings
    (even-odd, so holes stay out)."""

    def __init__(self, areas):
        points = [p for rings in areas.values() for ring in rings for p in ring]
        self.west = min(p[0] for p in points)
        self.south = min(p[1] for p in points)
        self.cols = int((max(p[0] for p in points) - self.west) / CELL) + 1
        self.rows = int((max(p[1] for p in points) - self.south) / CELL) + 1
        self.names = list(areas)
        self.cells = bytearray(self.cols * self.rows)
        for index, rings in enumerate(areas.values(), 1):
            crossings = defaultdict(list)
            for ring in rings:
                for (x1, y1), (x2, y2) in zip(ring, ring[1:] + ring[:1]):
                    if y1 == y2:
                        continue
                    low, high = min(y1, y2), max(y1, y2)
                    first = max(0, math.ceil((low - self.south) / CELL - 0.5))
                    last = min(self.rows - 1, math.floor((high - self.south) / CELL - 0.5))
                    for row in range(first, last + 1):
                        y = self.south + (row + 0.5) * CELL
                        if low <= y < high:
                            crossings[row].append(x1 + (y - y1) * (x2 - x1) / (y2 - y1))
            for row, xs in crossings.items():
                xs.sort()
                for a, b in zip(xs[::2], xs[1::2]):
                    c0 = max(0, math.ceil((a - self.west) / CELL - 0.5))
                    c1 = min(self.cols - 1, math.floor((b - self.west) / CELL - 0.5))
                    if c0 <= c1:
                        self.cells[row * self.cols + c0:row * self.cols + c1 + 1] = bytes([index]) * (c1 - c0 + 1)

    def at(self, lon, lat):
        """The name of the area at a point, or None"""
        col = int((lon - self.west) / CELL)
        row = int((lat - self.south) / CELL)
        if 0 <= col < self.cols and 0 <= row < self.rows and self.cells[row * self.cols + col]:
            return self.names[self.cells[row * self.cols + col] - 1]
        return None


class Ways(osmium.SimpleHandler):
    def __init__(self, out):
        super().__init__()
        self.out = out
        self.count = 0

    def way(self, w):
        # a ferry is a way too: to the Pfaueninsel there is no other
        kind = w.tags.get('highway') or ('ferry' if w.tags.get('route') == 'ferry' else None)
        if not kind or kind in SKIP or w.tags.get('area') == 'yes':
            return
        oneway = w.tags.get('oneway', '')
        if oneway in ('true', '1'):
            oneway = 'yes'
        # a roundabout and a motorway go one way unless they say otherwise
        if not oneway and (w.tags.get('junction') in ('roundabout', 'circular') or kind == 'motorway'):
            oneway = 'yes'
        if oneway not in ('yes', '-1'):
            oneway = ''
        name = w.tags.get('name', '').replace('\t', ' ')
        points = ' '.join(f'{n.ref} {n.lon:.6f},{n.lat:.6f}' for n in w.nodes)
        self.out.write(f'{kind}\t{oneway}\t{name}\t{points}\n')
        self.count += 1


def download(region):
    os.makedirs(os.path.join(HERE, 'cache'), exist_ok=True)
    path = os.path.join(HERE, 'cache', region.replace('/', '-') + '.osm.pbf')
    if not os.path.exists(path):
        url = f'https://download.geofabrik.de/{region}-latest.osm.pbf'
        print(f'Downloading {url}', file=sys.stderr)
        request = urllib.request.Request(url, headers={'User-Agent': 'collect-streets (Hackschule Workspace)'})
        with urllib.request.urlopen(request) as response, open(path + '.part', 'wb') as f:
            while chunk := response.read(1 << 20):
                f.write(chunk)
        os.rename(path + '.part', path)
    return path


def main():
    parser = argparse.ArgumentParser(description=__doc__.split('\n\n')[0])
    parser.add_argument('--region', default='europe/germany/berlin', help='a region of download.geofabrik.de')
    parser.add_argument('--name', help='the name of the whole region\'s file (default: its last part)')
    parser.add_argument('--area', action='append', default=[], metavar='NAME=WEST,SOUTH,EAST,NORTH',
                        help='an area to cut out as a box (for Berlin, the Bezirke are cut out anyway)')
    parser.add_argument('--out', default=os.path.join(HERE, 'street-archive'))
    args = parser.parse_args()
    berlin = args.region == 'europe/germany/berlin'
    boxes = {}
    for item in args.area:
        name, box = item.split('=')
        boxes[name] = tuple(map(float, box.split(',')))

    # the places and, for Berlin, the boundaries of the Bezirke
    finder = Places(BEZIRKE if berlin else ())
    finder.apply_file(download(args.region), locations=True)
    places = finder.places()
    missing = [name for name, rings in finder.bezirke.items() if not rings]
    if missing:
        sys.exit(f'No boundary found for {", ".join(missing)}')
    grid = Grid({slug(name): rings for name, rings in finder.bezirke.items()}) if berlin else None
    areas = list(boxes) + (grid.names if grid else [])

    os.makedirs(args.out, exist_ok=True)
    whole_name = args.name or args.region.split('/')[-1]
    whole = os.path.join(args.out, whole_name + '.tsv')
    with open(whole, 'w') as out:
        handler = Ways(out)
        handler.apply_file(download(args.region), locations=True)
    print(f'{whole}: {handler.count} ways', file=sys.stderr)

    # the areas: every way with a point inside, whole
    files = {name: open(os.path.join(args.out, name + '.tsv'), 'w') for name in areas}
    counts = dict.fromkeys(areas, 0)
    with open(whole) as f:
        for line in f:
            items = line.rstrip('\n').split('\t', 3)[3].split(' ')[1::2]
            points = [tuple(map(float, item.split(','))) for item in items]
            inside = {name for name, (w, s, e, n) in boxes.items() if any(w <= x <= e and s <= y <= n for x, y in points)}
            if grid:
                inside |= {grid.at(x, y) for x, y in points} - {None}
            for name in inside:
                files[name].write(line)
                counts[name] += 1
    for name, f in files.items():
        f.close()
        print(f'{f.name}: {counts[name]} ways', file=sys.stderr)

    # the places, for the region and for each area
    def where(lon, lat):
        found = {name for name, (w, s, e, n) in boxes.items() if w <= lon <= e and s <= lat <= n}
        if grid and grid.at(lon, lat):
            found.add(grid.at(lon, lat))
        return found

    for name in [whole_name] + areas:
        rows = [p for p in places if name == whole_name or name in where(p[2], p[3])]
        with open(os.path.join(args.out, f'{name}-places.tsv'), 'w') as f:
            for kind, place, lon, lat in rows:
                f.write(f'{kind}\t{place}\t{lon:.6f},{lat:.6f}\n')
        counted = ', '.join(f'{sum(r[0] == kind for r in rows)} {kind}s' for kind in ('sight', 'square', 'school'))
        print(f'{name}-places.tsv: {counted}', file=sys.stderr)

    # every file of ways compressed as well, for workspace-files: Berlin is
    # 70 MB as text, a Bezirk 3 to 7 MB
    for name in [whole_name] + areas:
        path = os.path.join(args.out, name + '.tsv')
        with open(path, 'rb') as src, gzip.open(path + '.gz', 'wb', compresslevel=9) as dst:
            dst.write(src.read())

    with open(os.path.join(args.out, 'README.md'), 'w') as f:
        f.write(README.format(date=datetime.date.today().isoformat(), region=args.region,
                              areas='\n'.join(f'- `{name}.tsv.gz`' for name in [whole_name] + areas)))


README = '''# Streets

Generated: {date}, from `{region}` of download.geofabrik.de, by
`src/scripts/collect-streets/collect-streets.py` in specht/workspace.

The streets, paths and stairs (and ferries) of a region, one way per line,
tab-separated:

| Column | Meaning |
| --- | --- |
| kind | the `highway` tag of OpenStreetMap (`residential`, `footway`, `primary`, `steps`, …), or `ferry` |
| oneway | `yes` (only in the direction of the points), `-1` (only against it), or empty |
| name | the name of the street, or empty |
| points | `ID LON,LAT ID LON,LAT …`: the nodes of the way in order; ways that meet share the ID of the node where they meet |

The files, the whole region and its areas (a way belongs to every area one of
its points lies in):

{areas}

Next to each, `NAME-places.tsv` has public places to start from and go to,
`kind` (`sight`, `square` or `school`), `name` and `LON,LAT`. Never addresses.

Data © OpenStreetMap contributors, ODbL 1.0
(https://www.openstreetmap.org/copyright).
'''


if __name__ == '__main__':
    main()
