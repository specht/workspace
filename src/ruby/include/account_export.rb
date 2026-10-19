require 'fileutils'
require 'json'
require 'time'
require_relative 'submission_view'

# »Meine Daten herunterladen« in the profile (Art. 15 Abs. 3 and Art. 20
# DSGVO): one ZIP with the Workspace, the hand-ins, the own MySQL
# database as SQL and everything stored about the account as JSON. Plain
# Ruby, tested on its own; Main builds the archive.
module AccountExport
    # What of a User node goes into the export; the rest are keys and tags
    # that open the Workspace (server_sid, share_tag …) and stay out.
    USER_KEYS = %w[email name brightness code_language preferred_language class_prefs last_seen deletion_at terms_accepted_at terms_version suspended_at suspended_reason].freeze

    README = <<~END_OF_TEXT
        Deine Daten aus dem Workspace
        =============================

        konto.json    alles, was zu deinem Konto gespeichert ist: Adresse, Name,
                      Gruppen, Einstellungen, Aufgaben mit deiner letzten
                      Antwort, Versuche, Hinweise, wer in deinen Workspace
                      gesehen hat, geteilte Web-Apps
        workspace/    deine Dateien, wie im Explorer des Workspace (ohne den
                      Zwischenspeicher .cache)
        abgaben/      deine Abgaben, je Aufgabe ein Ordner mit den Dateien,
                      wie du sie abgegeben hast
        mysql/        deine MySQL-Datenbanken, je eine .sql-Datei; mit
                      mysql DATENBANK < mysql/NAME.sql
                      lässt sich eine woanders wieder einspielen
        weggelassen.txt  Ordner, die ein Befehl wieder anlegt (node_modules,
                      Build-Ordner, Python-Umgebungen) und die deshalb fehlen;
                      mit dem Häkchen im Profil sind sie dabei
        neo4j.dump    deine Neo4j-Datenbank; in einer leeren Datenbank lädt
                      neo4j_bolt load neo4j.dump
                      sie wieder (im Workspace ist neo4j_bolt installiert)
    END_OF_TEXT

    # rows: :user (Hash), :groups (Array), :task_states, :attempts,
    # :look_ins, :live_apps (Arrays of Hashes, as Neo4j returns them).
    def self.account_hash(rows, now: Time.now)
        user = rows[:user] || {}
        {
            'exportiert' => now.utc.iso8601,
            'konto' => USER_KEYS.to_h { |key| [key, user[key.to_sym] || user[key]] }.compact,
            'gruppen' => rows[:groups] || [],
            'aufgaben' => (rows[:task_states] || []).map do |s|
                pick(s, %w[task status last_answer attempts first_status first_correct updated version])
            end,
            'versuche' => (rows[:attempts] || []).map { |a| pick(a, %w[task status ts version]) },
            'einsicht' => (rows[:look_ins] || []).map { |l| pick(l, %w[ts kind reason by_name]) },
            'geteilte_web_apps' => (rows[:live_apps] || []).map { |l| pick(l, %w[port active updated_at]) },
        }
    end

    def self.account_json(rows, now: Time.now)
        JSON.pretty_generate(account_hash(rows, :now => now))
    end

    def self.pick(node, keys)
        keys.to_h { |key| [key, node[key.to_sym] || node[key]] }.compact
    end
    private_class_method :pick

    # The folder name for a task's hand-in: activity/task -> activity-task.
    def self.folder_for(task)
        task.to_s.gsub(/[^a-z0-9_-]+/i, '-').gsub(/\A-+|-+\z/, '')
    end

    # Unpacks a stored hand-in (.tar.gz) into target: regular files only,
    # every path checked, so nothing lands outside target.
    def self.unpack_submission(archive, target)
        count = 0
        SubmissionView.each_entry(archive) do |entry|
            path = SubmissionView.clean_path(entry.full_name)
            next unless path

            destination = File.join(target, path)
            FileUtils.mkdir_p(File.dirname(destination))
            File.binwrite(destination, entry.read.to_s)
            count += 1
        end
        count
    end

    def self.filename(email, now: Time.now)
        "workspace-#{email.to_s.split('@').first.gsub(/[^a-z0-9._-]/i, '')}-#{now.strftime('%Y-%m-%d')}.zip"
    end
end
