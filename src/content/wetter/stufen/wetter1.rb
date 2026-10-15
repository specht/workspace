require 'net/http'
require 'json'

# Das aktuelle Wetter in Berlin
url = URI('https://api.open-meteo.com/v1/forecast?latitude=52.52&longitude=13.41&current=temperature_2m,wind_speed_10m')
response = Net::HTTP.get_response(url)
data = JSON.parse(response.body)

current = data['current']
puts "Temperatur: #{current['temperature_2m']} °C"
puts "Wind: #{current['wind_speed_10m']} km/h"
