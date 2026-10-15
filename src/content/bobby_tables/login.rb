require 'io/console'
require 'mysql2'

# Name und Passwort der weiteren Datenbank aus deinem Profil
DATABASE = 'db_1234'
DATABASE_PASSWORD = 'hier das Passwort'

CLIENT = Mysql2::Client.new(
    host: ENV['MYSQL_HOST'],
    username: DATABASE,
    password: DATABASE_PASSWORD,
    database: DATABASE
)

print 'Login: '
login = $stdin.gets.chomp
print 'Passwort: '
password = $stdin.noecho(&:gets).chomp
puts

rows = CLIENT.query("SELECT login FROM users \
    WHERE login = '#{login}' \
    AND password = '#{password}' LIMIT 1;").to_a

if rows.empty?
    puts 'Fehler: Login oder Passwort falsch.'
else
    puts "Anmeldung erfolgreich. Herzlich willkommen, #{rows.first['login']}."
end
