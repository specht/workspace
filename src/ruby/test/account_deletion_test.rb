require 'minitest/autorun'
require_relative '../include/account_deletion'

class AccountDeletionTest < Minitest::Test
    SOURCES = ([File.expand_path('../main.rb', __dir__)] + Dir[File.expand_path('../include/*.rb', __dir__)])
        .map { |path| File.read(path) }.join("\n")

    NODE = '\(\w*:(\w+)(?:\s*\{[^}]*\})?\s*\)'
    USER = '\((?:u|\w*:User)\b[^)]*\)'

    # Every label that main.rb links to a User: [label, relation].
    def linked_labels
        found = SOURCES.scan(/#{NODE}\s*-\[:(\w+)\]->\s*#{USER}/m) +
            SOURCES.scan(/#{USER}\s*<-\[:(\w+)\]-\s*#{NODE}/m).map(&:reverse) +
            SOURCES.scan(/#{USER}\s*-\[:(\w+)\]->\s*#{NODE}/m).map(&:reverse)
        found.reject { |label, _| label == 'User' }.uniq
    end

    def test_every_node_linked_to_a_user_is_deleted_or_not_owned
        labels = linked_labels
        assert_includes labels, ['QrLoginRequest', 'APPROVED_BY']
        labels.each do |label, relation|
            next if AccountDeletion::NOT_OWNED[label] == relation

            pattern = AccountDeletion::OWN_NODES[label]
            assert pattern, "#{label} (-[:#{relation}]-) hangs on a User but AccountDeletion does not delete it"
            assert_includes pattern, ":#{relation}]", "#{label}: deleted by another relation than #{relation}"
        end
    end

    def test_statements_take_only_the_address_as_parameter
        statements = AccountDeletion.node_statements
        assert_equal AccountDeletion::OWN_NODES.size + 1, statements.size
        statements.each do |statement|
            assert_includes statement, 'MATCH (u:User {email: $email})'
            assert_includes statement, 'DETACH DELETE'
        end
        assert_equal 'MATCH (u:User {email: $email}) DETACH DELETE u;', statements.last
    end

    def test_deletions_are_repeated_after_a_restore
        t0 = Time.at(1_000_000)
        log = AccountDeletion.log_line('old@example.org', 'salt', :now => t0) +
            AccountDeletion.log_line('new@example.org', 'salt', :now => t0 + 100) + "kaputt\n"
        refute_includes log, 'example.org'
        entries = AccountDeletion.parse_log(log)
        assert_equal 2, entries.size
        emails = %w[old@example.org new@example.org other@example.org]
        assert_equal ['new@example.org'], AccountDeletion.to_delete_again(entries, emails, 'salt', t0 + 50)
        assert_equal %w[old@example.org new@example.org], AccountDeletion.to_delete_again(entries, emails, 'salt', t0)
        assert_equal [], AccountDeletion.to_delete_again(entries, emails, 'other salt', t0)
    end

    # Without a date: Neo4j's own last deletion time says where the restored
    # backup ends.
    def test_after_a_restore_neo4j_says_which_deletions_it_misses
        salt = 'salz'
        t = Time.at(1_800_000_000)
        log = [
            JSON.parse(AccountDeletion.log_line('early@example.com', salt, :now => t)),
            JSON.parse(AccountDeletion.log_line('again@example.com', salt, :now => t + 10)),
            JSON.parse(AccountDeletion.log_line('late@example.com', salt, :now => t + 100)),
        ]
        # The backup was made after the second deletion: Neo4j knows t + 10.
        # again@ was made anew after its deletion and is in the backup; late@
        # was deleted after the backup and came back with it.
        existing = ['again@example.com', 'late@example.com', 'kept@example.com']
        assert_equal ['late@example.com'], AccountDeletion.to_delete_after_restore(log, existing, salt, (t + 10).to_i)
        assert_equal 1, AccountDeletion.entries_after(log, (t + 10).to_i).size
        # Nothing restored: Neo4j knows the last deletion, nothing to do
        assert_empty AccountDeletion.to_delete_after_restore(log, existing, salt, (t + 100).to_i)
    end

    # Two deletions in the same second, the backup between them: the
    # numbers keep them apart where the time cannot.
    def test_same_second_deletions_are_told_apart_by_their_number
        salt = 'salz'
        t = Time.at(1_800_000_000)
        log = [
            JSON.parse(AccountDeletion.log_line('first@example.com', salt, :now => t, :seq => 7)),
            JSON.parse(AccountDeletion.log_line('second@example.com', salt, :now => t, :seq => 8)),
        ]
        existing = ['first@example.com', 'second@example.com']
        # Neo4j from the backup knows the first deletion: time t, number 7
        assert_equal ['second@example.com'], AccountDeletion.to_delete_after_restore(log, existing, salt, t.to_i, 7)
        # by time alone the second would be missed
        assert_empty AccountDeletion.to_delete_after_restore(log, existing, salt, t.to_i)
    end

    def test_numbers_count_on_from_the_log_and_neo4j
        log = [{ 'seq' => 3 }, { 'seq' => 5 }, { 'deleted_at' => 1 }]
        assert_equal 6, AccountDeletion.next_seq(log, 4)
        assert_equal 10, AccountDeletion.next_seq(log, 9)
        assert_equal 1, AccountDeletion.next_seq([], nil)
    end

    # The notice first; a failed mail schedules nothing
    def test_a_deletion_is_scheduled_only_after_the_mail_went_out
        scheduled = []
        errors = []
        failed = AccountDeletion.announce(:send => -> { raise IOError, 'SMTP down' }, :schedule => -> { scheduled << :x },
            :on_error => ->(e) { errors << e.message })
        assert_equal :mail_failed, failed
        assert_empty scheduled
        assert_equal ['SMTP down'], errors
        order = []
        ok = AccountDeletion.announce(:send => -> { order << :mail }, :schedule => -> { order << :deletion })
        assert_equal :announced, ok
        assert_equal %i[mail deletion], order
    end

    def test_old_entries_go_once_no_backup_is_older
        now = Time.at(100_000_000)
        entries = [{ 'hash' => 'a', 'deleted_at' => now.to_i - 8 * 31 * 86_400 }, { 'hash' => 'b', 'deleted_at' => now.to_i - 86_400 }]
        assert_equal ['b'], AccountDeletion.prune(entries, 6, :now => now).map { |e| e['hash'] }
        assert_equal 2, AccountDeletion.prune(entries, nil, :now => now).size
    end

    def test_normalize_email
        assert_equal 'lena@example.org', AccountDeletion.normalize_email(' Lena@Example.org ')
        [nil, '', 'lena', 'a@b@c', "lena@example.org'; DROP", 'le na@example.org', "lena@example.org\nx"].each do |bad|
            assert_nil AccountDeletion.normalize_email(bad), bad.inspect
        end
    end
end
