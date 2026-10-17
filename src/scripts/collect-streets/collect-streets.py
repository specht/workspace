#!/usr/bin/env python3
"""The streets and paths of a region from OpenStreetMap, for »Ein Routenplaner
für Berlin«: every way with a highway tag (from motorways to footpaths and
stairs), one line per way, and areas cut out of it by bounding box.

    pip install osmium
    ./collect-streets.py                               # Berlin and the areas below
    ./collect-streets.py --region europe/germany/hamburg --area altona=9.90,53.54,9.96,53.57

The region comes from download.geofabrik.de (cached in cache/); the files go
to street-archive/. Each line of a file is one way, tab-separated:

    kind      the highway tag: residential, footway, primary, steps, …
    oneway    yes (only in the direction of the points), -1 (only against it), or empty
    name      the name of the street, or empty
    points    "ID LON,LAT ID LON,LAT …": the nodes of the way in order; ways
              that meet share the ID of the node where they meet

Next to each file of ways, NAME-places.tsv has the places to start from and
go to: public places only (sights, stations, parks), and every school with a
name. Never addresses: nobody should plan the way to their own home. A line:

    kind      sight or school
    name      what the place is called
    position  LON,LAT

OpenStreetMap data © OpenStreetMap contributors, ODbL 1.0.
"""
import argparse
import gzip
import os
import sys
import urllib.request

import osmium

HERE = os.path.dirname(os.path.abspath(__file__))
# Not a way to travel on: planned or under construction, platforms, race tracks
SKIP = {'construction', 'proposed', 'abandoned', 'platform', 'raceway', 'bus_stop', 'rest_area',
        'services', 'elevator', 'emergency_bay', 'escape', 'traffic_sign', 'street_lamp', 'corridor'}
# The areas of the activity: Steglitz (the Ortsteil), the Bezirk
# Steglitz-Zehlendorf, as west, south, east, north
AREAS = {
    'steglitz': (13.300, 52.440, 13.355, 52.470),
    'steglitz-zehlendorf': (13.088, 52.383, 13.370, 52.470),
}


# The sights: the name in OpenStreetMap, a tag that picks the right object of
# that name (»Brandenburger Tor« is also a station and a taxi rank), and for
# a name that is there twice a rough position near the one meant
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
    ('Alexanderplatz', 'place=square'), ('Potsdamer Platz', 'railway=station'), ('Wannsee', 'railway=station'),
]


class Places(osmium.SimpleHandler):
    """Sights and schools: nodes, ways and areas of several parts, each at the
    middle of its points"""

    def __init__(self):
        super().__init__()
        self.found = {}  # name -> [(lon, lat)] of the objects that fit
        self.schools = {}  # (name, rounded position) -> (name, lon, lat)

    def check(self, tags, lon, lat):
        name = tags.get('name')
        if not name:
            return
        if tags.get('amenity') == 'school':
            self.schools.setdefault((name, round(lon, 3), round(lat, 3)), (name, lon, lat))
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
        if a.from_way():
            return  # the way itself was already looked at
        points = [(n.lon, n.lat) for ring in a.outer_rings() for n in ring]
        if points:
            self.check(a.tags, sum(p[0] for p in points) / len(points), sum(p[1] for p in points) / len(points))

    def places(self):
        out = []
        for sight in SIGHTS:
            name, near = sight[0], (sight[2] if len(sight) > 2 else None)
            hits = self.found.get(name)
            if not hits:
                print(f'Not found: {name}', file=sys.stderr)
                continue
            lon, lat = min(hits, key=lambda p: (p[0] - near[0]) ** 2 + (p[1] - near[1]) ** 2) if near else hits[0]
            out.append(('sight', name, lon, lat))
        out += [('school', name, lon, lat) for name, lon, lat in sorted(self.schools.values())]
        return out


class Ways(osmium.SimpleHandler):
    def __init__(self, out):
        super().__init__()
        self.out = out
        self.count = 0

    def way(self, w):
        kind = w.tags.get('highway')
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


def inside(points, box):
    west, south, east, north = box
    for item in points.split(' ')[1::2]:
        lon, lat = map(float, item.split(','))
        if west <= lon <= east and south <= lat <= north:
            return True
    return False


def main():
    parser = argparse.ArgumentParser(description=__doc__.split('\n\n')[0])
    parser.add_argument('--region', default='europe/germany/berlin', help='a region of download.geofabrik.de')
    parser.add_argument('--name', help='the name of the whole region\'s file (default: its last part)')
    parser.add_argument('--area', action='append', default=[], metavar='NAME=WEST,SOUTH,EAST,NORTH',
                        help='an area to cut out (default: the Steglitz areas, for Berlin)')
    parser.add_argument('--out', default=os.path.join(HERE, 'street-archive'))
    args = parser.parse_args()

    areas = dict(AREAS) if not args.area and args.region == 'europe/germany/berlin' else {}
    for item in args.area:
        name, box = item.split('=')
        areas[name] = tuple(map(float, box.split(',')))

    os.makedirs(args.out, exist_ok=True)
    whole = os.path.join(args.out, (args.name or args.region.split('/')[-1]) + '.tsv')
    with open(whole, 'w') as out:
        handler = Ways(out)
        handler.apply_file(download(args.region), locations=True)
    print(f'{whole}: {handler.count} ways', file=sys.stderr)

    # the areas: every way with a point inside the box, whole
    files = {name: open(os.path.join(args.out, name + '.tsv'), 'w') for name in areas}
    counts = dict.fromkeys(areas, 0)
    with open(whole) as f:
        for line in f:
            points = line.rstrip('\n').split('\t', 3)[3]
            for name, box in areas.items():
                if inside(points, box):
                    files[name].write(line)
                    counts[name] += 1
    for name, f in files.items():
        f.close()
        print(f'{f.name}: {counts[name]} ways', file=sys.stderr)

    # the places, for the region and for each area
    finder = Places()
    finder.apply_file(download(args.region), locations=True)
    places = finder.places()
    boxes = {(args.name or args.region.split('/')[-1]): None, **areas}
    for name, box in boxes.items():
        rows = [p for p in places if box is None or (box[0] <= p[2] <= box[2] and box[1] <= p[3] <= box[3])]
        with open(os.path.join(args.out, f'{name}-places.tsv'), 'w') as f:
            for kind, place, lon, lat in rows:
                f.write(f'{kind}\t{place}\t{lon:.6f},{lat:.6f}\n')
        print(f'{name}-places.tsv: {sum(r[0] == "sight" for r in rows)} sights, {sum(r[0] == "school" for r in rows)} schools', file=sys.stderr)

    # the whole region compressed as well: Berlin is 70 MB as text
    with open(whole, 'rb') as src, gzip.open(whole + '.gz', 'wb', compresslevel=9) as dst:
        dst.write(src.read())


if __name__ == '__main__':
    main()
