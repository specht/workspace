DEVELOPMENT = true

# IMPORTANT: This must be the path to the data directory on the host machine
PATH_TO_HOST_DATA = nil

WEBSITE_HOST = DEVELOPMENT ? "workspace.test:8025" : "code.hackschule.de"
WEB_ROOT = DEVELOPMENT ? "http://#{WEBSITE_HOST}" : "https://#{WEBSITE_HOST}"
PHPMYADMIN_WEB_ROOT = DEVELOPMENT ? "http://phpmyadmin.#{WEBSITE_HOST}" : "https://phpmyadmin.#{WEBSITE_HOST}"
PGADMIN_WEB_ROOT = DEVELOPMENT ? "http://pgadmin.#{WEBSITE_HOST}" : "https://pgadmin.#{WEBSITE_HOST}"
NEO4J_WEB_ROOT = DEVELOPMENT ? "http://neo4j.#{WEBSITE_HOST}/browser/" : "https://neo4j.#{WEBSITE_HOST}/browser/"
NEO4J_BOLT_HOST = "bolt.#{WEBSITE_HOST}"
ENABLE_CODEBITES = false

LOGIN_CODE_SALT = 'bitte_ein_salt_eintragen'

SMTP_SERVER = 'smtps.udag.de'
IMAP_SERVER = 'imaps.udag.de'
SMTP_USER = 'info@example.com'
SMTP_PASSWORD = 'smtp_password'
SMTP_DOMAIN = 'smtp_domain'
SMTP_FROM = 'Absender <E-Mail-Adresse>'

ADMIN_USERS = []

MYSQL_ROOT_PASSWORD = 'bitte_ein_passwort_eintragen'
MYSQL_PASSWORD_SALT = 'bitte_ein_langes_salt_generieren'
POSTGRES_ROOT_PASSWORD = 'bitte_ein_passwort_eintragen'
POSTGRES_PASSWORD_SALT = 'bitte_ein_langes_salt_generieren'
PGADMIN_PASSWORD = 'bitte_ein_passwort_eintragen'
NEO4J_ROOT_PASSWORD = 'bitte_ein_passwort_eintragen'
NEO4J_PASSWORD_SALT = 'bitte_ein_langes_salt_generieren'

if defined? Mail
    Mail.defaults do
    delivery_method :smtp, {
        :address => SMTP_SERVER,
        :port => 587,
        :domain => SMTP_DOMAIN,
        :user_name => SMTP_USER,
        :password => SMTP_PASSWORD,
        :authentication => 'login',
        :enable_starttls_auto => true
    }
    end
end
