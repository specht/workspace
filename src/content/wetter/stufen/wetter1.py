import json
import urllib.request

# Das aktuelle Wetter in Berlin
url = 'https://api.open-meteo.com/v1/forecast?latitude=52.52&longitude=13.41&current=temperature_2m,wind_speed_10m'
with urllib.request.urlopen(url) as response:
    data = json.load(response)

current = data['current']
print(f"Temperatur: {current['temperature_2m']} °C")
print(f"Wind: {current['wind_speed_10m']} km/h")
