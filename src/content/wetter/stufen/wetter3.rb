require 'net/http'
require 'json'

CACHE_DIR = File.join(__dir__, 'cache')
CACHE_SECONDS = 600   # zehn Minuten

# Holt JSON von einer API. Aus params wird die Query: ?name=wert&…
# Dieselbe Anfrage in den letzten zehn Minuten? Dann kommt die Antwort aus
# einer Datei im Ordner cache, und die API wird nicht gefragt.
def fetch_json(base, params)
  url = URI(base)
  url.query = URI.encode_www_form(params)
  file = File.join(CACHE_DIR, url.to_s.gsub(/[^A-Za-z0-9]+/, '_') + '.json')
  if File.exist?(file) && Time.now - File.mtime(file) < CACHE_SECONDS
    puts "(aus dem Cache: #{url.host})"
    return JSON.parse(File.read(file))
  end

  response = Net::HTTP.get_response(url)
  data = JSON.parse(response.body)
  # Bei einem Fehler sagt Open-Meteo in »reason«, was nicht stimmt
  abort "Fehler #{response.code}: #{data['reason']}" unless response.is_a?(Net::HTTPSuccess)
  Dir.mkdir(CACHE_DIR) unless Dir.exist?(CACHE_DIR)
  File.write(file, response.body)
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
places = fetch_json('https://geocoding-api.open-meteo.com/v1/search',
                    name: name, count: 1, language: 'de')
place = (places['results'] || []).first
abort "Keinen Ort namens #{name} gefunden." if place.nil?

# 2. Das Wetter an diesem Ort
weather = fetch_json('https://api.open-meteo.com/v1/forecast',
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
