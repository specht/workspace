require 'minitest/autorun'
require 'stringio'
require_relative '../migrate_mysql_auth'

class FakeMysql
    attr_reader :queries

    def initialize(select_results:, alter_error: nil)
        @select_results = select_results
        @alter_error = alter_error
        @queries = []
    end

    def query(sql)
        @queries << sql
        if sql.lstrip.start_with?('SELECT')
            @select_results.shift || []
        else
            raise @alter_error if @alter_error

            []
        end
    end

    def escape(value)
        value.gsub('\\', '\\\\').gsub("'", "\\\\'")
    end
end

class MysqlAuthMigrationTest < Minitest::Test
    def account(user, host = '%')
        {
            'user' => user,
            'host' => host,
            'plugin' => 'mysql_native_password',
        }
    end

    def runner(mysql:, emails:, apply: false, out: StringIO.new, err: StringIO.new)
        MysqlAuthMigration::Runner.new(
            :mysql => mysql,
            :emails => emails,
            :root_password => 'root-secret',
            :password_salt => 'test-salt',
            :apply => apply,
            :out => out,
            :err => err,
        )
    end

    def test_email_to_mysql_login_mapping_matches_application_behavior
        assert_equal 'micha.specht',
            WorkspaceCredentials.mysql_login_for_email('Micha.Specht@example.com')
        assert_equal 'student',
            WorkspaceCredentials.mysql_login_for_email('Student@school@example.com')
    end

    def test_deterministic_password_reuses_workspace_algorithm
        assert_equal 'JqtR4ZS6-v35q',
            WorkspaceCredentials.password_for_email(
                'micha.specht@example.com',
                'test-salt',
            )
    end

    def test_duplicate_login_is_ambiguous_and_is_not_altered
        mysql = FakeMysql.new(
            :select_results => [[account('student')]],
        )
        output = StringIO.new
        migration = runner(
            :mysql => mysql,
            :emails => ['student@one.example', 'Student@two.example'],
            :out => output,
        )

        assert migration.run
        assert_includes output.string, 'ambiguous workspace users'
        assert_includes output.string, 'student@one.example, Student@two.example'
        assert_equal 1, mysql.queries.size
    end

    def test_dry_run_queries_only_native_password_accounts_and_does_not_alter
        mysql = FakeMysql.new(
            :select_results => [[account('student')]],
        )
        output = StringIO.new
        migration = runner(
            :mysql => mysql,
            :emails => ['student@example.com'],
            :out => output,
        )

        assert migration.run
        assert_equal [MysqlAuthMigration::NATIVE_ACCOUNTS_SQL], mysql.queries
        assert_includes mysql.queries.first,
            "WHERE plugin = 'mysql_native_password'"
        assert_includes output.string, 'Dry run only.'
    end

    def test_apply_generates_escaped_alter_user_and_does_not_print_password
        mysql = FakeMysql.new(
            :select_results => [
                [account("o'hara", "local'host")],
                [],
            ],
        )
        output = StringIO.new
        migration = runner(
            :mysql => mysql,
            :emails => ["o'hara@example.com"],
            :apply => true,
            :out => output,
        )

        assert migration.run
        alter = mysql.queries.find { |sql| sql.lstrip.start_with?('ALTER') }
        assert_includes alter, "ALTER USER 'o\\'hara'@'local\\'host'"
        assert_includes alter, 'IDENTIFIED WITH caching_sha2_password'
        expected_password = WorkspaceCredentials.password_for_email(
            "o'hara@example.com",
            'test-salt',
        )
        assert_includes alter, "BY '#{expected_password}'"
        refute_includes output.string, expected_password
        assert_equal 2,
            mysql.queries.count { |sql| sql == MysqlAuthMigration::NATIVE_ACCOUNTS_SQL }
    end

    def test_root_and_workspace_failures_make_apply_unsuccessful
        error = RuntimeError.new('server rejected ALTER USER')
        mysql = FakeMysql.new(
            :select_results => [
                [account('root', 'localhost')],
                [account('root', 'localhost')],
            ],
            :alter_error => error,
        )
        errors = StringIO.new
        migration = runner(
            :mysql => mysql,
            :emails => [],
            :apply => true,
            :err => errors,
        )

        refute migration.run
        assert_equal 1, migration.failures.size
        assert_includes errors.string, '[failure] "root"@"localhost"'
    end

    def test_account_still_native_after_alter_is_a_failure
        mysql = FakeMysql.new(
            :select_results => [
                [account('student')],
                [account('student')],
            ],
        )
        errors = StringIO.new
        migration = runner(
            :mysql => mysql,
            :emails => ['student@example.com'],
            :apply => true,
            :err => errors,
        )

        refute migration.run
        assert_equal 1, migration.failures.size
        assert_includes errors.string,
            'account still uses mysql_native_password'
    end

    def test_unmanaged_accounts_are_prominent_but_do_not_fail_dry_run
        mysql = FakeMysql.new(
            :select_results => [[account('healthchecker', 'localhost')]],
        )
        output = StringIO.new
        migration = runner(
            :mysql => mysql,
            :emails => [],
            :out => output,
        )

        assert migration.run
        assert_includes output.string,
            '[skip]    "healthchecker"@"localhost" - unmanaged/system account'
        assert_includes output.string, '1 unmanaged/system account(s) remain.'
    end
end
