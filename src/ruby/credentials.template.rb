DEVELOPMENT = (ENV['DEVELOPMENT'] == '1')

# IMPORTANT: This must be the path to the data directory on the host machine
PATH_TO_HOST_DATA = nil

WEBSITE_HOST = "code.hackschule.de"
WEB_ROOT = DEVELOPMENT ? 'http://localhost:8025' : "https://#{WEBSITE_HOST}"

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
NEO4J_ROOT_PASSWORD = 'bitte_ein_passwort_eintragen'

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
