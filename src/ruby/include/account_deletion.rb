require 'digest'
require 'json'
require 'set'

# Deleting an account with everything that belongs to it (Datenschutzerklärung,
# »Speicherdauer«; docs/datenschutz.md, Löschkonzept). Plain Ruby, tested on
# its own; Main#delete_account does the work.
#
# What belongs to a person:
# - the nodes in OWN_NODES, found by how they hang on the User node, and the
#   User node itself;
# - the folders of its Workspace and of the exams it took
#   (/user/<fs_tag of email + test tag>), and of a teacher's exam demos;
# - its MySQL database and login, the extra MySQL databases it made, its
#   Neo4j database, user and role;
# - its hand-ins under /internal/submissions.
#
# Exams (Test) a teacher owns go with the teacher; an exam a student took
# stays, only the student's relation to it goes (DETACH DELETE).
# account_deletion_test.rb checks that every label main.rb hangs on a User is
# either here or in NOT_OWNED.
module AccountDeletion
    OWN_NODES = {
        'Session' => '(n:Session)-[:FOR]->(u)',
        'LoginRequest' => '(n:LoginRequest)-[:FOR]->(u)',
        'QrLoginRequest' => '(n:QrLoginRequest)-[:APPROVED_BY]->(u)',
        'LookIn' => '(n:LookIn)-[:FOR]->(u)',
        'TaskState' => '(u)-[:HAS_TASK_STATE]->(n:TaskState)',
        'TaskAttempt' => '(n:TaskAttempt)-[:BY]->(u)',
        'Database' => '(u)-[:HAS]->(n:Database)',
        'LiveAppShare' => '(u)-[:SHARES_LIVE_APP]->(n:LiveAppShare)',
        'Test' => '(n:Test)-[:BELONGS_TO]->(u)',
        # a teacher's feedback on activities (ActivityFeedback)
        'ActivityFeedback' => '(n:ActivityFeedback)-[:BY]->(u)',
    }.freeze

    # Linked to a User, but not the person's own: the exam a student takes
    # belongs to its teacher.
    NOT_OWNED = {
        'Test' => 'TAKES',
    }.freeze

    # One Cypher statement per kind of node, each with $email.
    def self.node_statements
        OWN_NODES.values.map do |pattern|
            "MATCH (u:User {email: $email}) MATCH #{pattern} DETACH DELETE n;"
        end + ['MATCH (u:User {email: $email}) DETACH DELETE u;']
    end

    # --- Deletions after a restore ---------------------------------------
    #
    # Backups are not edited; a deleted account stays in them until they
    # expire (BACKUP_RETENTION_MONTHS). So that a restore does not bring it
    # back, every deletion is noted in a log outside Neo4j: a hash of the
    # address and the date, nothing else. After a restore, »Löschungen
    # wiederholen« deletes again every account deleted after the backup was
    # made. Entries go once no backup can be older than them.

    def self.address_hash(email, salt)
        Digest::SHA256.hexdigest("deleted-account:#{salt}:#{email}")
    end

    # seq: the deletion's number, counted up across the log and Neo4j
    # (Main#note_deletion), so that two deletions in the same second keep
    # their order.
    def self.log_line(email, salt, now: Time.now, seq: nil)
        entry = { 'hash' => address_hash(email, salt), 'deleted_at' => now.to_i }
        entry['seq'] = seq.to_i if seq
        JSON.generate(entry) + "\n"
    end

    # The number for the next deletion: above every one in the log and the
    # one Neo4j knows (the log forgets old entries, Neo4j may hold a later one).
    def self.next_seq(entries, neo4j_seq)
        ([neo4j_seq.to_i] + entries.map { |e| e['seq'].to_i }).max + 1
    end

    def self.parse_log(text)
        text.to_s.each_line.map { |line| JSON.parse(line) rescue nil }.compact
    end

    # The entries a backup made at backup_time does not know of.
    def self.entries_since(entries, backup_time)
        entries.select { |e| e['deleted_at'].to_i >= backup_time.to_i }
    end

    # The addresses among emails that were deleted after backup_time.
    def self.to_delete_again(entries, emails, salt, backup_time)
        hashes = Set.new(entries_since(entries, backup_time).map { |e| e['hash'] })
        emails.select { |email| hashes.include?(address_hash(email, salt)) }
    end

    # Without a date: every deletion also notes its time in Neo4j (Setting
    # last_deletion, Main#note_deletion). A restored Neo4j still holds the
    # time of the last deletion before its backup, so every entry after it
    # is an account the backup brought back; an account made again after its
    # deletion is not in that backup. Strictly after: an account deleted and
    # made again before the backup keeps its later life. By number where both
    # have one (two deletions in the same second), else by time (entries from
    # before the numbers).
    def self.entries_after(entries, time, seq = nil)
        entries.select do |e|
            seq && e['seq'] ? e['seq'].to_i > seq.to_i : e['deleted_at'].to_i > time.to_i
        end
    end

    def self.to_delete_after_restore(entries, emails, salt, last_deletion, seq = nil)
        hashes = Set.new(entries_after(entries, last_deletion, seq).map { |e| e['hash'] })
        emails.select { |email| hashes.include?(address_hash(email, salt)) }
    end

    # Announcing a deletion: the notice first (send), the deletion only once
    # it went out (schedule); a failed mail schedules nothing. Returns
    # :announced or :mail_failed.
    def self.announce(send:, schedule:, on_error: nil)
        begin
            send.call
        rescue StandardError => e
            on_error&.call(e)
            return :mail_failed
        end
        schedule.call
        :announced
    end

    # Entries older than the oldest backup can be; months: the retention,
    # plus one month for safety.
    def self.prune(entries, months, now: Time.now)
        return entries unless months.to_i > 0

        cutoff = now.to_i - (months.to_i + 1) * 31 * 24 * 3600
        entries.select { |e| e['deleted_at'].to_i >= cutoff }
    end

    # The address as typed, or nil when it cannot be an account: one line,
    # one @, nothing that would reach a shell or a query unescaped.
    def self.normalize_email(text)
        email = text.to_s.strip.downcase
        email.match?(/\A[^@\s'"`\\]+@[^@\s'"`\\]+\z/) ? email : nil
    end
end
