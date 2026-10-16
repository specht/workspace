require 'net/http'
require 'json'

# Die Adressen der beiden APIs. Antwortet Open-Meteo mit 429, nimm den
# Spiegel der Schule (»Wenn die API nicht mehr will«).
GEOCODING_API = 'https://geocoding-api.open-meteo.com/v1/search'
WEATHER_API = 'https://api.open-meteo.com/v1/forecast'

# Holt JSON von einer API. Aus params wird die Query: ?name=wert&…
def fetch_json(base, params)
  url = URI(base)
  url.query = URI.encode_www_form(params)
  response = Net::HTTP.get_response(url)
  data = JSON.parse(response.body)
  # Bei einem Fehler sagt Open-Meteo in »reason«, was nicht stimmt
  abort "Fehler #{response.code}: #{data['reason']}" unless response.is_a?(Net::HTTPSuccess)
  data
rescue SocketError, Net::OpenTimeout => e
  abort "Keine Verbindung: #{e.message}"
end

# Was die Wettercodes der WMO bedeuten, grob zusammengefasst
def describe(code)
  case code
  when 0 then 'klar'
  when 1, 2 then 'teils bewölkt'
  when 3 then 'bedeckt'
  when 45, 48 then 'Nebel'
  when 51..57 then 'Nieselregen'
  when 61..67, 80..82 then 'Regen'
  when 71..77, 85, 86 then 'Schnee'
  when 95..99 then 'Gewitter'
  else "Code #{code}"
  end
end

name = ARGV[0] || 'Berlin'

# 1. Wo liegt der Ort?
places = fetch_json(GEOCODING_API,
                    name: name, count: 1, language: 'de')
place = (places['results'] || []).first
abort "Keinen Ort namens #{name} gefunden." if place.nil?

# 2. Das Wetter an diesem Ort
weather = fetch_json(WEATHER_API,
                     latitude: place['latitude'], longitude: place['longitude'],
                     current: 'temperature_2m,weather_code',
                     daily: 'temperature_2m_min,temperature_2m_max,weather_code',
                     timezone: 'auto', forecast_days: 3)

now = weather['current']
puts "#{place['name']} (#{place['admin1']}, #{place['country']})"
puts "Jetzt: #{now['temperature_2m']} °C, #{describe(now['weather_code'])}"

days = weather['daily']
days['time'].each_with_index do |day, i|
  puts "#{day}: #{days['temperature_2m_min'][i]} bis #{days['temperature_2m_max'][i]} °C, " \
       "#{describe(days['weather_code'][i])}"
end
