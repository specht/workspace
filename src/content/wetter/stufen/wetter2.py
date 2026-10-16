import json
import sys
import urllib.error
import urllib.parse
import urllib.request

# Die Adressen der beiden APIs. Antwortet Open-Meteo mit 429, nimm den
# Spiegel der Schule (»Wenn die API nicht mehr will«).
GEOCODING_API = 'https://geocoding-api.open-meteo.com/v1/search'
WEATHER_API = 'https://api.open-meteo.com/v1/forecast'


# Holt JSON von einer API. Aus params wird die Query: ?name=wert&…
def fetch_json(base, params):
    url = base + '?' + urllib.parse.urlencode(params)
    try:
        with urllib.request.urlopen(url) as response:
            return json.load(response)
    except urllib.error.HTTPError as error:
        # Bei einem Fehler sagt Open-Meteo in »reason«, was nicht stimmt
        data = json.load(error)
        sys.exit(f"Fehler {error.code}: {data.get('reason')}")
    except urllib.error.URLError as error:
        sys.exit(f'Keine Verbindung: {error.reason}')


# Was die Wettercodes der WMO bedeuten, grob zusammengefasst
def describe(code):
    if code == 0:
        return 'klar'
    if code in (1, 2):
        return 'teils bewölkt'
    if code == 3:
        return 'bedeckt'
    if code in (45, 48):
        return 'Nebel'
    if 51 <= code <= 57:
        return 'Nieselregen'
    if 61 <= code <= 67 or 80 <= code <= 82:
        return 'Regen'
    if 71 <= code <= 77 or code in (85, 86):
        return 'Schnee'
    if 95 <= code <= 99:
        return 'Gewitter'
    return f'Code {code}'


name = sys.argv[1] if len(sys.argv) > 1 else 'Berlin'

# 1. Wo liegt der Ort?
places = fetch_json(GEOCODING_API,
                    {'name': name, 'count': 1, 'language': 'de'})
results = places.get('results', [])
if not results:
    sys.exit(f'Keinen Ort namens {name} gefunden.')
place = results[0]

# 2. Das Wetter an diesem Ort
weather = fetch_json(WEATHER_API, {
    'latitude': place['latitude'], 'longitude': place['longitude'],
    'current': 'temperature_2m,weather_code',
    'daily': 'temperature_2m_min,temperature_2m_max,weather_code',
    'timezone': 'auto', 'forecast_days': 3,
})

now = weather['current']
print(f"{place['name']} ({place.get('admin1', '')}, {place['country']})")
print(f"Jetzt: {now['temperature_2m']} °C, {describe(now['weather_code'])}")

days = weather['daily']
for i, day in enumerate(days['time']):
    print(f"{day}: {days['temperature_2m_min'][i]} bis {days['temperature_2m_max'][i]} °C, "
          f"{describe(days['weather_code'][i])}")
