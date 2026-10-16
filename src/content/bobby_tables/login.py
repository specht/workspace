import getpass
import os
import mysql.connector

# Name und Passwort der weiteren Datenbank aus deinem Profil
DATABASE = 'db_1234'
DATABASE_PASSWORD = 'hier das Passwort'

connection = mysql.connector.connect(
    host=os.environ['MYSQL_HOST'],
    user=DATABASE,
    password=DATABASE_PASSWORD,
    database=DATABASE,
)

login = input('Login: ')
password = getpass.getpass('Passwort: ')

cursor = connection.cursor()
cursor.execute(f"SELECT login FROM users "
               f"WHERE login = '{login}' "
               f"AND password = '{password}' LIMIT 1;")
rows = cursor.fetchall()

if not rows:
    print('Fehler: Login oder Passwort falsch.')
else:
    print(f'Anmeldung erfolgreich. Herzlich willkommen, {rows[0][0]}.')
