require 'tty-prompt'
require 'mysql2'

PROMPT = TTY::Prompt.new
CLIENT = Mysql2::Client.new(
    host: ENV['MYSQL_HOST'],
    username: ENV['MYSQL_USER'],
    password: ENV['MYSQL_PASSWORD'],
    database: 'db_1234'
)

login = PROMPT.ask('Login:')
password = PROMPT.ask('Password:')

rows = CLIENT.query("SELECT login FROM users \
    WHERE login = '#{login}' \
    AND password = '#{password}' LIMIT 1;").to_a

if rows.empty?
    puts "❌ Fehler: Ungültige Zugangsdaten!"
else
    user = rows.first['login']
    puts "✅ Anmeldung erfolgreich."
    puts "Herzlich willkommen, #{user}."
end