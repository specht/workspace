# Notes

## Postgres

https://github.com/anthonydb/practical-sql
./config.rb exec -it pgadmin /venv/bin/python3 setup.py add-user micha.specht@gmail.com 12345678
ALTER DATABASE my_database OWNER TO my_database_user;
CREATE DATABASE EXAMPLE_DB;
CREATE USER EXAMPLE_USER WITH ENCRYPTED PASSWORD 'Sup3rS3cret';
GRANT ALL PRIVILEGES ON DATABASE EXAMPLE_DB TO EXAMPLE_USER;
\c EXAMPLE_DB postgres
# You are now connected to database "EXAMPLE_DB" as user "postgres".
GRANT ALL ON SCHEMA public TO EXAMPLE_USER;
Simple LDAP with config file: https://github.com/glauth/glauth?tab=readme-ov-file