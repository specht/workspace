require 'minitest/autorun'
require 'tmpdir'
require_relative '../include/database_provisioning'
require_relative '../include/workspace_credentials'

class DatabaseProvisioningTest < Minitest::Test
    def test_deterministic_workspace_password_is_unchanged
        assert_equal 'JqtR4ZS6-v35q',
            WorkspaceCredentials.password_for_email(
                'micha.specht@example.com',
                'test-salt',
            )
    end

    def test_mysql_and_neo4j_use_exactly_the_same_login
        login = 'student-k7q4'
        mysql = DatabaseProvisioning.mysql_statements(login, 'password')
        neo4j = DatabaseProvisioning.neo4j_statements(login, 'password')

        assert mysql.all? { |statement| !statement.include?('other-login') }
        assert neo4j.all? { |statement| !statement.include?('other-login') }
        assert_includes mysql.join("\n"), "'#{login}'@'%'"
        assert_includes mysql.join("\n"), "`#{login}`"
        assert_includes neo4j.join("\n"), "`#{login}`"

        environment = DatabaseProvisioning.workspace_environment(
            login,
            'mysql-password',
            'neo4j-password',
        )
        assert_equal login, environment['MYSQL_USER']
        assert_equal login, environment['MYSQL_DATABASE']
        assert_equal login, environment['NEO4J_USERNAME']
        assert_equal login, environment['NEO4J_DATABASE']
    end

    def test_neo4j_uses_a_valid_role_name_for_database_logins_with_punctuation
        login = 'e2e-0'
        role = DatabaseProvisioning.neo4j_role_name(login)
        statements = DatabaseProvisioning.neo4j_statements(login, 'password')
        joined = statements.join("\n")

        assert_equal 'workspace_e2e_dash_0', role
        assert_includes joined, "CREATE USER `#{login}`"
        assert_includes joined, "CREATE DATABASE `#{login}`"
        assert_includes joined, "CREATE ROLE `#{role}` IF NOT EXISTS"
        assert_includes joined, "GRANT ROLE `#{role}` TO `#{login}`"
        refute_includes joined, "CREATE ROLE `#{login}`"
    end

    def test_neo4j_role_name_is_valid_and_collision_safe_for_supported_logins
        assert_equal 'student42',
            DatabaseProvisioning.neo4j_role_name('student42')
        assert_equal 'workspace_2fast',
            DatabaseProvisioning.neo4j_role_name('2fast')
        assert_equal 'workspace_alice_dot_smith',
            DatabaseProvisioning.neo4j_role_name('alice.smith')
        assert_equal 'workspace_alice_dash_smith',
            DatabaseProvisioning.neo4j_role_name('alice-smith')

        %w[2fast alice.smith alice-smith].each do |login|
            assert_match(
                DatabaseProvisioning::NEO4J_ROLE_PATTERN,
                DatabaseProvisioning.neo4j_role_name(login),
            )
        end
        refute_equal(
            DatabaseProvisioning.neo4j_role_name('alice.smith'),
            DatabaseProvisioning.neo4j_role_name('alice-smith'),
        )
    end

    def test_mysql_creation_and_reset_explicitly_use_caching_sha2_password
        statements = DatabaseProvisioning.mysql_statements('student', 'password')

        assert_match(
            /CREATE USER .* IDENTIFIED WITH caching_sha2_password BY/,
            statements.first,
        )
        assert_match(
            /ALTER USER .* IDENTIFIED WITH caching_sha2_password BY/,
            statements[1],
        )
        escaped = "'a#{'\\' * 2}b\\'c'"
        assert_equal escaped, DatabaseProvisioning.mysql_string("a\\b'c")
        assert_equal escaped, DatabaseProvisioning.cypher_string("a\\b'c")
    end

    def test_neo4j_creation_does_not_reset_a_new_user_to_the_same_password
        statements = DatabaseProvisioning.neo4j_statements('student', 'password')

        password_changes = statements.select do |statement|
            statement.include?('SET PLAINTEXT PASSWORD')
        end
        assert_equal 1, password_changes.size
        assert password_changes.first.start_with?('CREATE USER ')
    end

    def test_neo4j_existing_identity_migration_does_not_create_user_or_database
        statements = DatabaseProvisioning.neo4j_existing_identity_statements(
            'student',
            'password',
        )

        refute statements.any? { |statement| statement.start_with?('CREATE USER ') }
        refute statements.any? { |statement| statement.start_with?('CREATE DATABASE ') }
        assert statements.any? { |statement| statement.start_with?('ALTER USER ') }
        assert statements.any? { |statement| statement.start_with?('GRANT ROLE ') }
    end

    def test_existing_my_cnf_is_updated_to_persisted_login
        Dir.mktmpdir do |directory|
            path = File.join(directory, '.my.cnf')
            File.write(path, <<~INI)
                [client]
                user = old-login
                password = password
                host = mysql
                database = old-login
                port = 3306
            INI
            File.chmod(0640, path)

            assert DatabaseProvisioning.sync_my_cnf(
                path,
                'new-login',
                'password',
            )
            assert_equal DatabaseProvisioning.my_cnf('new-login', 'password'),
                File.read(path)
            assert_equal 0640, File.stat(path).mode & 0777

            inode = File.stat(path).ino
            refute DatabaseProvisioning.sync_my_cnf(
                path,
                'new-login',
                'password',
            )
            assert_equal inode, File.stat(path).ino
        end
    end
end
