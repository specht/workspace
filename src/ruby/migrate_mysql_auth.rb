#!/usr/bin/env ruby

require 'open3'
require 'set'
require_relative 'include/database_identity'
require_relative 'include/database_provisioning'
require_relative 'include/workspace_credentials'

module MysqlAuthMigration
    MYSQL_ACCOUNTS_SQL = <<~SQL.freeze
        SELECT user, host, plugin
        FROM mysql.user;
    SQL
    MYSQL_DATABASES_SQL = <<~SQL.freeze
        SELECT schema_name
        FROM information_schema.schemata;
    SQL
    NATIVE_ACCOUNTS_SQL = <<~SQL.freeze
        SELECT user, host, plugin
        FROM mysql.user
        WHERE plugin = 'mysql_native_password';
    SQL

    class Neo4jWorkspaceStore
        def initialize(neo4j)
            @neo4j = neo4j
            @allocator = DatabaseIdentity::Neo4jAllocator.new(neo4j)
        end

        def users
            @neo4j.neo4j_query(<<~CYPHER).map do |row|
                MATCH (u:User)
                RETURN u.email AS email, u.db_login AS db_login
            CYPHER
                {
                    :email => row['email'],
                    :db_login => row['db_login'],
                }
            end
        end

        def ensure_constraint!
            @allocator.ensure_constraint!
        end

        def persist!(email, db_login)
            @allocator.claim!(email, db_login)
        end
    end

    class Neo4jState
        def initialize(neo4j)
            @neo4j = neo4j
        end

        def users
            @neo4j.neo4j_query(
                'SHOW USERS YIELD user RETURN user'
            ).map { |row| row['user'] }.to_set
        end

        def databases
            @neo4j.neo4j_query(
                'SHOW DATABASES YIELD name RETURN name'
            ).map { |row| row['name'] }.to_set
        end
    end

    class Neo4jProvisioner
        def initialize(root_password)
            @root_password = root_password
        end

        def migrate_existing(login, password, database_exists:)
            status = nil
            Open3.popen2e(
                'docker', 'exec', '-i', 'workspace_neo4j_1',
                'bin/cypher-shell', '-u', 'neo4j', '-p', @root_password,
            ) do |stdin, combined_output, wait_thread|
                statements = if database_exists
                    DatabaseProvisioning.neo4j_existing_identity_statements(
                        login,
                        password,
                    )
                else
                    DatabaseProvisioning.neo4j_existing_user_statements(
                        login,
                        password,
                    )
                end
                statements
                    .each { |statement| stdin.puts statement }
                stdin.close
                combined_output.read
                status = wait_thread.value
            end
            return if status.success?

            raise "Neo4j migration failed for #{login} (exit #{status.exitstatus})"
        end
    end

    class Runner
        attr_reader :failures

        def initialize(mysql:, workspace_store:, neo4j_state:,
            neo4j_provisioner:, mysql_root_password:, mysql_password_salt:,
            neo4j_password_salt:, apply: false, out: $stdout, err: $stderr)
            @mysql = mysql
            @workspace_store = workspace_store
            @neo4j_state = neo4j_state
            @neo4j_provisioner = neo4j_provisioner
            @mysql_root_password = mysql_root_password
            @mysql_password_salt = mysql_password_salt
            @neo4j_password_salt = neo4j_password_salt
            @apply = apply
            @out = out
            @err = err
            @failures = []
        end

        def run
            users = @workspace_store.users
            allocations = DatabaseIdentity.plan(users)
            mysql_accounts = fetch_mysql_accounts
            mysql_databases = fetch_mysql_databases
            neo4j_users = @neo4j_state.users
            neo4j_databases = @neo4j_state.databases

            print_header(users, allocations)
            print_mysql_plan(allocations, mysql_accounts, mysql_databases)
            print_neo4j_plan(allocations, neo4j_users, neo4j_databases)
            print_collisions(allocations)

            unless @apply
                @out.puts
                @out.puts 'Dry run only. Run with --apply to perform the migration.'
                return true
            end

            allocations = persist_allocations(allocations)
            migrate_mysql(allocations, mysql_accounts)
            migrate_neo4j(allocations, neo4j_users, neo4j_databases)
            verify(allocations)
            failures.empty?
        rescue DatabaseIdentity::AllocationError => error
            record_failure('database login allocation', error)
            false
        rescue StandardError => error
            record_failure('migration', error)
            false
        end

        private

        def fetch_mysql_accounts
            @mysql.query(MYSQL_ACCOUNTS_SQL).to_a
        end

        def fetch_mysql_databases
            @mysql.query(MYSQL_DATABASES_SQL)
                .map { |row| row['schema_name'] }.to_set
        end

        def print_header(users, allocations)
            @out.puts 'Workspace database identity migration'
            @out.puts
            @out.puts "Users: #{users.size}"
            @out.puts
            @out.puts 'Database login allocation:'
            allocations.each do |allocation|
                label = allocation.db_login == allocation.stem ? 'keep' : 'assign'
                label = 'stored' if allocation.stored
                @out.puts format(
                    '[%-6s] %-36s -> %s',
                    label,
                    allocation.email,
                    allocation.db_login,
                )
            end
        end

        def print_mysql_plan(allocations, accounts, databases)
            @out.puts
            @out.puts 'MySQL:'
            allocations.each do |allocation|
                account = accounts.find do |row|
                    row['user'] == allocation.db_login && row['host'] == '%'
                end
                if account.nil?
                    @out.puts "[defer-user]   #{allocation.db_login.inspect}@\"%\" - create on next login"
                elsif account['plugin'] == 'mysql_native_password'
                    @out.puts "[migrate-auth] #{allocation.db_login.inspect}@\"%\""
                else
                    @out.puts "[keep]         #{allocation.db_login.inspect}@\"%\""
                end
                unless databases.include?(allocation.db_login)
                    @out.puts "[defer-db]     #{allocation.db_login.inspect} - create on next login"
                end
            end

            managed_native_accounts(accounts, allocations).each do |account, owner|
                next if account['user'] == owner.db_login && account['host'] == '%'

                @out.puts "[migrate-auth] #{account_label(account)} <- #{owner.email}"
            end

            native_root_accounts(accounts).each do |account|
                @out.puts "[migrate-root] #{account_label(account)}"
            end
            unmanaged_native_accounts(accounts, allocations).each do |account|
                @out.puts "[system]       #{account_label(account)} - left untouched"
            end
        end

        def print_neo4j_plan(allocations, users, databases)
            @out.puts
            @out.puts 'Neo4j:'
            allocations.each do |allocation|
                login = allocation.db_login
                if users.include?(login)
                    @out.puts "[migrate-user] #{login}"
                else
                    @out.puts "[defer-user]   #{login} - create on next login"
                end
                if databases.include?(login)
                    @out.puts "[keep-db]      #{login}"
                else
                    @out.puts "[defer-db]     #{login} - create on next login"
                end
            end
        end

        def print_collisions(allocations)
            groups = allocations.group_by do |allocation|
                DatabaseIdentity.legacy_login_for_email(allocation.email)
            end
            collisions = groups.select { |_login, group| group.size > 1 }
            renames = groups.select do |legacy_login, group|
                group.size == 1 && group.first.db_login != legacy_login
            end
            return if collisions.empty? && renames.empty?

            @out.puts
            @out.puts 'Collision resolution:'
            collisions.sort.each do |legacy_login, group|
                ordered = group.sort_by do |allocation|
                    DatabaseIdentity.normalized_email(allocation.email)
                end
                keeper = ordered.find do |allocation|
                    allocation.db_login == legacy_login
                end
                @out.puts "[collision] old database #{legacy_login.inspect} is shared by #{group.size} workspace users"
                ordered.each { |allocation| @out.puts "  #{allocation.email}" }
                if keeper
                    @out.puts "[keep]      #{keeper.email} keeps existing database #{legacy_login.inspect}"
                end
                (ordered - [keeper]).compact.each do |allocation|
                    @out.puts "[assign]    #{allocation.email} uses #{allocation.db_login.inspect}; missing account/database will be created on next login"
                end
            end
            renames.sort.each do |legacy_login, group|
                allocation = group.first
                @out.puts "[assign]    #{allocation.email} uses #{allocation.db_login.inspect}; missing account/database will be created on next login and legacy account/database #{legacy_login.inspect} is left untouched"
            end
        end

        def persist_allocations(allocations)
            @workspace_store.ensure_constraint!
            allocations.map do |allocation|
                next allocation if allocation.stored

                begin
                    actual = @workspace_store.persist!(
                        allocation.email,
                        allocation.db_login,
                    )
                    DatabaseIdentity::Allocation.new(
                        :email => allocation.email,
                        :db_login => actual,
                        :stored => true,
                        :stem => allocation.stem,
                    ).tap do |persisted|
                        @out.puts "[db-login-ok] #{persisted.email} -> #{persisted.db_login}"
                    end
                rescue StandardError => error
                    record_failure("db_login for #{allocation.email}", error)
                    nil
                end
            end.compact
        end

        def migrate_mysql(allocations, original_accounts)
            managed_native_accounts(original_accounts, allocations)
                .each do |account, owner|
                    begin
                        password = WorkspaceCredentials.password_for_email(
                            owner.email,
                            @mysql_password_salt,
                        )
                        @mysql.query(alter_mysql_user(
                            account['user'],
                            account['host'],
                            password,
                        ))
                        @out.puts "[mysql-auth-ok] #{account_label(account)}"
                    rescue StandardError => error
                        record_failure(
                            "MySQL authentication #{account_label(account)}",
                            redact(error, password),
                        )
                    end
                end

            native_root_accounts(original_accounts).each do |account|
                begin
                    @mysql.query(alter_mysql_user(
                        account['user'],
                        account['host'],
                        @mysql_root_password,
                    ))
                    @out.puts "[mysql-root-ok] #{account_label(account)}"
                rescue StandardError => error
                    record_failure(
                        "MySQL root #{account_label(account)}",
                        redact(error, @mysql_root_password),
                    )
                end
            end
        end

        def migrate_neo4j(allocations, existing_users, existing_databases)
            allocations.each do |allocation|
                next unless existing_users.include?(allocation.db_login)

                begin
                    password = WorkspaceCredentials.password_for_email(
                        allocation.email,
                        @neo4j_password_salt,
                    )
                    @neo4j_provisioner.migrate_existing(
                        allocation.db_login,
                        password,
                        :database_exists => existing_databases.include?(
                            allocation.db_login,
                        ),
                    )
                    @out.puts "[neo4j-ok] #{allocation.email} -> #{allocation.db_login}"
                rescue StandardError => error
                    record_failure("Neo4j #{allocation.email}", error)
                end
            end
        end

        def verify(allocations)
            remaining = @mysql.query(NATIVE_ACCOUNTS_SQL).to_a
            remaining_roots = remaining.select { |account| account['user'] == 'root' }
            remaining_managed = managed_native_accounts(remaining, allocations)
            remaining_roots.each do |account|
                record_failure(
                    "MySQL root #{account_label(account)}",
                    RuntimeError.new('still uses mysql_native_password'),
                )
            end
            remaining_managed.each do |account, owner|
                record_failure(
                    "MySQL authentication #{account_label(account)} for #{owner.email}",
                    RuntimeError.new('still uses mysql_native_password'),
                )
            end

            @out.puts
            @out.puts 'Migration result:'
            @out.puts "  Workspace database identities persisted: #{allocations.size - failed_user_count(allocations)}"
            @out.puts "  Root accounts still using mysql_native_password: #{remaining_roots.size}"
            @out.puts "  Failures: #{failures.size}"
            if remaining.empty?
                @out.puts 'No mysql_native_password accounts remain.'
            else
                @out.puts 'Accounts still using mysql_native_password:'
                remaining.each do |account|
                    kind = if account['user'] == 'root'
                        'root failure'
                    elsif workspace_owner(account, allocations)
                        'workspace failure'
                    else
                        'unmanaged/system'
                    end
                    @out.puts "  #{account_label(account)} (#{kind})"
                end
            end
        end

        def failed_user_count(allocations)
            allocations.count do |allocation|
                failures.any? { |failure| failure[:label].include?(allocation.email) }
            end
        end

        def native_root_accounts(accounts)
            accounts.select do |account|
                account['user'] == 'root' &&
                    account['plugin'] == 'mysql_native_password'
            end
        end

        def unmanaged_native_accounts(accounts, allocations)
            accounts.select do |account|
                account['plugin'] == 'mysql_native_password' &&
                    account['user'] != 'root' &&
                    workspace_owner(account, allocations).nil?
            end
        end

        def managed_native_accounts(accounts, allocations)
            accounts.filter_map do |account|
                next unless account['plugin'] == 'mysql_native_password'
                next if account['user'] == 'root'

                owner = workspace_owner(account, allocations)
                [account, owner] if owner
            end
        end

        def workspace_owner(account, allocations)
            desired = allocations.find do |allocation|
                allocation.db_login == account['user']
            end
            return desired if desired

            legacy = allocations.select do |allocation|
                DatabaseIdentity.legacy_login_for_email(allocation.email) ==
                    account['user']
            end
            legacy.one? ? legacy.first : nil
        end

        def alter_mysql_user(user, host, password)
            <<~SQL
                ALTER USER #{mysql_string(user)}@#{mysql_string(host)}
                IDENTIFIED WITH caching_sha2_password
                BY #{mysql_string(password)};
            SQL
        end

        def mysql_string(value)
            "'#{@mysql.escape(value.to_s)}'"
        end

        def account_label(account)
            "#{account['user'].inspect}@#{account['host'].inspect}"
        end

        def record_failure(label, error)
            @failures << { :label => label, :error => error }
            @err.puts "[failure] #{label} - #{error.class}: #{error.message}"
        end

        def redact(error, secret)
            secret = secret.to_s
            message = if secret.empty?
                error.message
            else
                error.message.gsub(secret, '[REDACTED]')
            end
            error.class.new(message)
        rescue StandardError
            RuntimeError.new('database operation failed')
        end
    end

    def self.run_cli(arguments)
        apply = arguments.delete('--apply')
        abort "Usage: ruby #{File.basename($PROGRAM_NAME)} [--apply]" unless arguments.empty?

        require 'mysql2'
        require 'neo4j_bolt'
        require_relative 'credentials'

        Neo4jBolt.bolt_host = 'neo4japp'
        Neo4jBolt.bolt_port = 7687
        neo4j = Class.new { include Neo4jBolt }.new
        mysql = Mysql2::Client.new(
            :host => 'mysql',
            :username => 'root',
            :password => MYSQL_ROOT_PASSWORD,
            :database => 'mysql',
            :encoding => 'utf8mb4',
        )

        runner = Runner.new(
            :mysql => mysql,
            :workspace_store => Neo4jWorkspaceStore.new(neo4j),
            :neo4j_state => Neo4jState.new(neo4j),
            :neo4j_provisioner => Neo4jProvisioner.new(NEO4J_ROOT_PASSWORD),
            :mysql_root_password => MYSQL_ROOT_PASSWORD,
            :mysql_password_salt => MYSQL_PASSWORD_SALT,
            :neo4j_password_salt => NEO4J_PASSWORD_SALT,
            :apply => apply,
        )
        runner.run ? 0 : 1
    rescue StandardError => error
        warn "Migration aborted: #{error.class}: #{error.message}"
        1
    end
end

exit MysqlAuthMigration.run_cli(ARGV) if $PROGRAM_NAME == __FILE__
