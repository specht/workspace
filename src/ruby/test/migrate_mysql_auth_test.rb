require 'minitest/autorun'
require 'set'
require 'stringio'
require_relative '../migrate_mysql_auth'

class MigrationFakeMysql
    attr_reader :writes

    def initialize(accounts:, databases:)
        @accounts = accounts.map(&:dup)
        @databases = databases.to_set
        @writes = []
    end

    def query(sql)
        case sql
        when MysqlAuthMigration::MYSQL_ACCOUNTS_SQL
            @accounts.map(&:dup)
        when MysqlAuthMigration::MYSQL_DATABASES_SQL
            @databases.map { |name| { 'schema_name' => name } }
        when MysqlAuthMigration::NATIVE_ACCOUNTS_SQL
            @accounts.select do |account|
                account['plugin'] == 'mysql_native_password'
            end.map(&:dup)
        else
            @writes << sql
            apply(sql)
            []
        end
    end

    def escape(value)
        value.gsub('\\', '\\\\').gsub("'", "\\\\'")
    end

    private

    def apply(sql)
        if sql.start_with?('CREATE USER')
            login = sql[/CREATE USER IF NOT EXISTS '([^']+)'@'%'/, 1]
            @accounts << {
                'user' => login,
                'host' => '%',
                'plugin' => 'caching_sha2_password',
            } unless @accounts.any? { |account| account['user'] == login && account['host'] == '%' }
        elsif sql.lstrip.start_with?('ALTER USER')
            user, host = sql.match(/ALTER USER '([^']+)'@'([^']+)'/).captures
            account = @accounts.find do |row|
                row['user'] == user && row['host'] == host
            end
            account['plugin'] = 'caching_sha2_password'
        elsif sql.start_with?('CREATE DATABASE')
            @databases << sql[/`([^`]+)`/, 1]
        end
    end
end

class MigrationFakeWorkspaceStore
    attr_reader :constraint_calls, :persist_calls

    def initialize(users)
        @users = users.map(&:dup)
        @constraint_calls = 0
        @persist_calls = []
    end

    def users
        @users.map(&:dup)
    end

    def ensure_constraint!
        @constraint_calls += 1
    end

    def persist!(email, db_login)
        @persist_calls << [email, db_login]
        user = @users.find { |candidate| candidate[:email] == email }
        user[:db_login] ||= db_login
    end
end

class MigrationFakeNeo4jState
    attr_reader :users, :databases

    def initialize(users:, databases:)
        @users = users.to_set
        @databases = databases.to_set
    end
end

class MigrationFakeNeo4jProvisioner
    attr_reader :calls

    def initialize(state)
        @state = state
        @calls = []
    end

    def migrate_existing(login, password, database_exists:)
        @calls << [login, password, database_exists]
    end
end

class MysqlAuthMigrationTest < Minitest::Test
    def account(user, plugin: 'mysql_native_password', host: '%')
        {
            'user' => user,
            'host' => host,
            'plugin' => plugin,
        }
    end

    def components(users:, accounts: [], mysql_databases: [],
        neo4j_users: [], neo4j_databases: [], apply: false)
        mysql = MigrationFakeMysql.new(
            :accounts => accounts,
            :databases => mysql_databases,
        )
        store = MigrationFakeWorkspaceStore.new(users)
        state = MigrationFakeNeo4jState.new(
            :users => neo4j_users,
            :databases => neo4j_databases,
        )
        provisioner = MigrationFakeNeo4jProvisioner.new(state)
        output = StringIO.new
        errors = StringIO.new
        runner = MysqlAuthMigration::Runner.new(
            :mysql => mysql,
            :workspace_store => store,
            :neo4j_state => state,
            :neo4j_provisioner => provisioner,
            :mysql_root_password => 'root-password',
            :mysql_password_salt => 'mysql-salt',
            :neo4j_password_salt => 'neo4j-salt',
            :apply => apply,
            :out => output,
            :err => errors,
        )
        [runner, mysql, store, state, provisioner, output, errors]
    end

    def collision_users
        [
            { :email => 'specht@ovgu.de', :db_login => nil },
            { :email => 'specht@gymnasiumsteglitz.de', :db_login => nil },
        ]
    end

    def test_dry_run_describes_allocations_and_both_databases_without_mutation
        runner, mysql, store, _state, provisioner, output = components(
            :users => collision_users,
            :accounts => [account('specht'), account('healthchecker')],
            :mysql_databases => ['specht'],
            :neo4j_users => ['specht'],
            :neo4j_databases => ['specht'],
        )

        assert runner.run
        assert_empty mysql.writes
        assert_empty store.persist_calls
        assert_equal 0, store.constraint_calls
        assert_empty provisioner.calls
        assert_includes output.string, 'Database login allocation:'
        assert_includes output.string, 'MySQL:'
        assert_includes output.string, 'Neo4j:'
        assert_includes output.string, '[collision] old database "specht" is shared by 2 workspace users'
        assert_includes output.string, 'healthchecker'
        assert_includes output.string, 'Dry run only.'
    end

    def test_apply_resolves_collision_but_defers_missing_database_identities
        runner, mysql, store, state, provisioner, output = components(
            :users => collision_users,
            :accounts => [
                account('specht'),
                account('root', :host => 'localhost'),
                account('healthchecker', :host => 'localhost'),
            ],
            :mysql_databases => ['specht'],
            :neo4j_users => ['specht'],
            :neo4j_databases => ['specht'],
            :apply => true,
        )

        assert runner.run
        persisted = store.users.to_h do |user|
            [user[:email], user[:db_login]]
        end
        assert_equal 'specht', persisted['specht@gymnasiumsteglitz.de']
        assert_match(/\Aspecht-[a-z0-9]{4}\z/, persisted['specht@ovgu.de'])
        refute_includes state.users, persisted['specht@ovgu.de']
        refute_includes state.databases, persisted['specht@ovgu.de']
        assert_equal ['specht'], provisioner.calls.map(&:first)
        assert provisioner.calls.first.last
        assert_includes output.string,
            'specht@gymnasiumsteglitz.de keeps existing database "specht"'
        assert_includes output.string,
            'specht@ovgu.de uses'
        assert_includes output.string, 'on next login'
        assert mysql.writes.any? do |sql|
            sql.include?('ALTER USER') &&
                sql.include?('caching_sha2_password')
        end
        refute mysql.writes.any? { |sql| sql.include?('CREATE USER') }
        refute mysql.writes.any? { |sql| sql.include?('CREATE DATABASE') }
    end

    def test_apply_is_resumable_and_idempotent
        parts = components(
            :users => [{ :email => 'student@example.com', :db_login => nil }],
            :apply => true,
        )
        runner, mysql, store, state, provisioner = parts
        assert runner.run
        first_login = store.users.first[:db_login]

        second = MysqlAuthMigration::Runner.new(
            :mysql => mysql,
            :workspace_store => store,
            :neo4j_state => state,
            :neo4j_provisioner => provisioner,
            :mysql_root_password => 'root-password',
            :mysql_password_salt => 'mysql-salt',
            :neo4j_password_salt => 'neo4j-salt',
            :apply => true,
            :out => StringIO.new,
            :err => StringIO.new,
        )

        assert second.run
        assert_equal first_login, store.users.first[:db_login]
        assert_empty state.users
        assert_empty state.databases
        assert_empty provisioner.calls
    end

    def test_apply_migrates_present_artifacts_and_defers_missing_ones
        users = %w[useronly dbonly absent].map do |stem|
            { :email => "#{stem}@example.com", :db_login => nil }
        end
        runner, mysql, _store, state, provisioner, output = components(
            :users => users,
            :accounts => [account('useronly')],
            :neo4j_users => ['useronly'],
            :neo4j_databases => ['dbonly'],
            :apply => true,
        )

        assert runner.run
        calls = provisioner.calls.map { |call| [call.first, call.last] }
        assert_equal [['useronly', false]], calls
        assert_equal ['useronly'], state.users.to_a
        assert_equal ['dbonly'], state.databases.to_a
        assert mysql.writes.all? { |sql| sql.lstrip.start_with?('ALTER USER') }
        assert_includes output.string, '[defer-db]     "useronly"'
        assert_includes output.string, '[defer-user]   dbonly'
        assert_includes output.string, '[defer-user]   absent'
    end

    def test_generated_mysql_and_neo4j_passwords_are_not_printed
        runner, _mysql, _store, _state, _provisioner, output, errors = components(
            :users => [{ :email => 'student@example.com', :db_login => nil }],
            :accounts => [account('student')],
            :mysql_databases => ['student'],
            :neo4j_users => ['student'],
            :neo4j_databases => ['student'],
            :apply => true,
        )

        assert runner.run
        mysql_password = WorkspaceCredentials.password_for_email(
            'student@example.com',
            'mysql-salt',
        )
        neo4j_password = WorkspaceCredentials.password_for_email(
            'student@example.com',
            'neo4j-salt',
        )
        refute_includes output.string, mysql_password
        refute_includes output.string, neo4j_password
        refute_includes errors.string, mysql_password
        refute_includes errors.string, neo4j_password
    end

    def test_native_query_and_root_alter_use_mysql_escaping
        runner, = components(:users => [])
        sql = runner.send(
            :alter_mysql_user,
            "ro'ot",
            "local'host",
            "p'assword",
        )

        assert_includes MysqlAuthMigration::NATIVE_ACCOUNTS_SQL,
            "WHERE plugin = 'mysql_native_password'"
        assert_includes sql, "'ro\\'ot'@'local\\'host'"
        assert_includes sql, "BY 'p\\'assword'"
    end

    def test_neo4j_failure_makes_apply_unsuccessful_and_preserves_db_login
        runner, _mysql, store, _state, provisioner, _output, errors = components(
            :users => [{ :email => 'student@example.com', :db_login => nil }],
            :neo4j_users => ['student'],
            :neo4j_databases => ['student'],
            :apply => true,
        )
        provisioner.define_singleton_method(:migrate_existing) do |_login, _password, database_exists:|
            raise 'Neo4j unavailable'
        end

        refute runner.run
        assert_equal 'student', store.users.first[:db_login]
        assert_includes errors.string, 'Neo4j unavailable'
    end
end
