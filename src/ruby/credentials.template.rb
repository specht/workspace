DEVELOPMENT = true

# IMPORTANT: This must be the path to the data directory on the host machine
PATH_TO_HOST_DATA = nil

WEBSITE_HOST = DEVELOPMENT ? "workspace.test:8025" : "code.hackschule.de"
WEB_ROOT = DEVELOPMENT ? "http://#{WEBSITE_HOST}" : "https://#{WEBSITE_HOST}"
PHPMYADMIN_WEB_ROOT = DEVELOPMENT ? "http://phpmyadmin.#{WEBSITE_HOST}" : "https://phpmyadmin.#{WEBSITE_HOST}"
NEO4J_WEB_ROOT = DEVELOPMENT ? "http://neo4j.#{WEBSITE_HOST}/browser/" : "https://neo4j.#{WEBSITE_HOST}/browser/"
NEO4J_BOLT_HOST = "bolt.#{WEBSITE_HOST}"

LOGIN_CODE_SALT = 'bitte_ein_salt_eintragen'

SMTP_SERVER = 'smtps.udag.de'
IMAP_SERVER = 'imaps.udag.de'
SMTP_USER = 'info@example.com'
SMTP_PASSWORD = 'smtp_password'
SMTP_DOMAIN = 'smtp_domain'
SMTP_FROM = 'Absender <E-Mail-Adresse>'

ADMIN_USERS = []

# Die Adresse, an die man schreibt: Einladungen, Impressum, Hilfe bei einer
# Seite. Erscheint auf der Startseite, im Impressum und als ${contact_email}
# in den Inhalten. Ohne diese Zeile fehlt die Adresse dort einfach.
CONTACT_EMAIL = 'kontakt@example.com'

MYSQL_ROOT_PASSWORD = 'bitte_ein_passwort_eintragen'
MYSQL_PASSWORD_SALT = 'bitte_ein_langes_salt_generieren'
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

# Aufgabenkarten für Lernende einschalten (docs/aufgaben.md). Ohne diese
# Zeile sehen nur Lehrkräfte die Aufgaben, als Vorschau.
# TASKS_ENABLED = true

# Den Fallserver für »Ein Krimi im Terminal« einschalten (docs/entwicklung.md,
# »Der Fallserver«): zwei kleine SSH-Server, museum.bitstein und
# alarmanlage.bitstein, mit einem Zugang für jede Person. Danach
# ./config.rb build case_museum case_alarm peer_firewall && ./config.rb up -d;
# Workspaces bekommen ihren Zugang beim nächsten Start.
# CASE_SERVER = true
