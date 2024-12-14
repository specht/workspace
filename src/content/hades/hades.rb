require 'httparty'
require 'json'

HOST = "https://agr.nhcham.org"

def post(path, data, headers = nil)
    response = JSON.parse(HTTParty.post("#{HOST}/#{path}", body: data.to_json, headers: headers).body)
    puts response
    response
end

print "Bitte gib deine E-Mail ein: "
email = gets.strip

puts "Code wird angefordert..."
tag = post('/api/login', {email: email})['tag']

print "Bitte gib deinen Anmeldecode ein: "
code = gets.strip

sid = post('/api/confirm_login', {tag: tag, code: code})['sid']
headers = { 'X-Session-ID': sid }

puts "Aktiviere Hades..."
post('/api/activate_hades', nil, headers)

post('/api/logout', nil, headers)
