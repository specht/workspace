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

# Die öffentliche Adresse des Workspace, für die Links in den Briefen an
# Rechteinhaber auf /picture-rights, wenn diese Installation eine andere hat
# (etwa die zum Entwickeln). Ohne diese Zeile gilt WEB_ROOT.
# PUBLIC_WEB_ROOT = 'https://workspace.example.org'

# Impressum und Datenschutzerklärung (/impressum) und die Unterlagen für die
# Schule (/datenschutz, docs/datenschutz.md). Jede Zeile ist freiwillig; ohne
# sie fehlt die Angabe dort, und die Seiten sagen, dass sie fehlt.
# Wer den Workspace betreibt, mit der Adresse, unter der man ihn erreicht
# (die der Schule genügt, wenn Briefe dort ankommen):
# OPERATOR_NAME = 'Vorname Nachname'
# OPERATOR_ROLE = 'Lehrkraft für Informatik'
# OPERATOR_SCHOOL = 'Name der Schule'
# OPERATOR_ADDRESS = "Straße 1\n12345 Ort"
# Die oder der Datenschutzbeauftragte der Schule, mit Kontakt:
# DATA_PROTECTION_OFFICER = "Name\ndatenschutz@example.com"
# Wo der Server läuft; nur ein Rechenzentrum in der EU, denn die
# Datenschutzerklärung sagt, dass keine Daten die EU verlassen:
# HOSTING = 'Hetzner Online GmbH, Rechenzentrum in Deutschland'
# Wer die E-Mails verschickt:
# MAIL_PROVIDER = 'united-domains AG'
# Wer die Namen (DNS) verwaltet, dieselbe Wahl wie DNS_PROVIDER in der .env
# des Frontends (docker/traefik/docker-compose.yaml); ohne den Wert steht in
# der Datenschutzerklärung Cloudflare:
# DNS_PROVIDER = 'deSEC e. V.'
# Wo die verschlüsselten Sicherungen liegen und wie viele Monate sie
# höchstens aufbewahrt werden (so lange bleiben gelöschte Daten darin):
# BACKUP = 'in einer eigenen Storage Box im Rechenzentrum'
# BACKUP_RETENTION_MONTHS = 6
# Ab wie vielen Stunden ohne Meldung der Sicherung (internal/backup-done)
# /admin »Betrieb« sie als gestört zeigt und die Administration eine E-Mail
# bekommt (docs/entwicklung.md, »Betrieb überwachen«):
# BACKUP_MAX_AGE_HOURS = 26
# Die Rechtsgrundlage: :school erst, wenn die Schulleitung schriftlich
# entschieden hat, dass der Workspace im Unterricht eingesetzt wird (die
# Unterlagen unter /datenschutz helfen dabei); :consent, wenn er ein eigenes
# Angebot ist und jede Person (unter 16 die Eltern) einwilligt. Ohne diese
# Zeile sagt die Seite, dass sie noch nicht eingetragen ist.
# LEGAL_BASIS = :school
# Die Vorschrift, auf die sich die Schule stützt (für :school):
# SCHOOL_LAW = '§ 64 Abs. 11 des Schulgesetzes für das Land Berlin und § 2 der Digitale Lehr- und Lernmittel-Verordnung (DigLLV)'
# Die Aufsichtsbehörde für Beschwerden:
# SUPERVISORY_AUTHORITY = "Berliner Beauftragte für Datenschutz und Informationsfreiheit\nAlt-Moabit 59–61, 10555 Berlin\nmailbox@datenschutz-berlin.de"
# Was der Server kostet und wie eine Schule beitragen kann, wenn sie möchte;
# steht auf der Seite für die Schulleitung und in der Willkommensmail an
# Lehrkräfte. Die Nutzung hängt nie davon ab, und die Seite sagt das dazu.
# Geht Geld an eine Privatperson, sind Steuer und Nebentätigkeit zu
# bedenken; einfacher zahlt ein Förderverein oder der Schulträger den Server.
# COST_NOTE = 'Der Server kostet etwa 500 € im Jahr. Wer beitragen möchte, etwa über den Förderverein, schreibt an die Ansprechperson.'

# Bis zu diesem Tag läuft die Übergangszeit (docs/handoff-datenschutz.md): Bis
# dahin sperrt die Rechtslage einer Schule niemanden, /personen zeigt den Stand
# jeder Schule und die Tage bis dahin. Ohne die Zeile: 12. April 2027. Der Tag
# allein ändert nichts am Zugang; was danach geschieht, entscheidet der Betreiber.
# TRANSITION_DEADLINE = '2027-04-12'

# true, wenn auf dem Server nichts anderes läuft als der Workspace: Dann sagen
# die technischen Maßnahmen und die Risikoanalyse das. Nur eintragen, wenn es
# stimmt.
# DEDICATED_SERVER = true

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

# Git-Repositorys für Gruppen im Workspace selbst (docs/teacher-access.md,
# »Repositorys im Workspace«): ein kleiner SSH-Server, git.workspace, den nur
# Workspaces erreichen. Lehrkräfte legen die Repositorys auf der Klassenseite
# an. Danach ./config.rb build git_server peer_firewall && ./config.rb up -d.
# Gespeichert werden die Repositorys in data/git-repos.
# GIT_SERVER = true

# Wie viele Katas (Programmieraufgaben in der Karte, docs/aufgaben.md,
# »function«) gleichzeitig laufen; die anderen warten der Reihe nach. Ohne
# diese Zeile 4. Wie viele der Server verträgt, misst
# ./config.rb exec ruby ruby /src/scripts/measure-katas.rb
# KATA_PARALLEL_RUNS = 4
