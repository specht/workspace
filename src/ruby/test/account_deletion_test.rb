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

    def test_normalize_email
        assert_equal 'lena@example.org', AccountDeletion.normalize_email(' Lena@Example.org ')
        [nil, '', 'lena', 'a@b@c', "lena@example.org'; DROP", 'le na@example.org', "lena@example.org\nx"].each do |bad|
            assert_nil AccountDeletion.normalize_email(bad), bad.inspect
        end
    end
end
