#!/usr/bin/env ruby

require_relative 'include/workspace_credentials'

module MysqlAuthMigration
    NATIVE_ACCOUNTS_SQL = <<~SQL.freeze
        SELECT user, host, plugin
        FROM mysql.user
        WHERE plugin = 'mysql_native_password';
    SQL

    ALTER_USER_SQL = <<~SQL.freeze
        ALTER USER %<user>s@%<host>s
        IDENTIFIED WITH caching_sha2_password
        BY %<password>s;
    SQL

    class WorkspaceUsers
        def initialize(neo4j)
            @neo4j = neo4j
        end

        def emails
            @neo4j.neo4j_query(
                'MATCH (u:User) RETURN u.email AS email'
            ).map { |row| row['email'] }
        end
    end

    class Runner
        attr_reader :failures

        def initialize(mysql:, emails:, root_password:, password_salt:,
            apply: false, out: $stdout, err: $stderr)
            @mysql = mysql
            @root_password = root_password
            @password_salt = password_salt
            @apply = apply
            @out = out
            @err = err
            @emails_by_login = emails_by_login(emails)
            @failures = []
        end

        def run
            accounts = native_accounts
            @out.puts "Found #{accounts.size} mysql_native_password account(s)."
            @out.puts

            planned = accounts.map { |account| classify(account) }
            planned.each { |entry| print_plan(entry) }
            print_plan_summary(planned)

            unless @apply
                @out.puts
                @out.puts 'Dry run only. Run with --apply to perform the migration.'
                return true
            end

            migrate(planned.select { |entry| migratable?(entry) })
            remaining = native_accounts
            print_apply_summary(planned, remaining)
            failures.empty?
        end

        private

        def emails_by_login(emails)
            emails.each_with_object(Hash.new { |hash, key| hash[key] = [] }) do |email, result|
                login = WorkspaceCredentials.mysql_login_for_email(email)
                result[login] << email unless result[login].include?(email)
            end
        end

        def native_accounts
            @mysql.query(NATIVE_ACCOUNTS_SQL).to_a.sort_by do |account|
                [account['user'] == 'root' ? 1 : 0, account['user'], account['host']]
            end
        end

        def classify(account)
            user = account.fetch('user')
            return account.merge(:kind => :root, :password => @root_password) if user == 'root'

            emails = @emails_by_login[user]
            case emails.size
            when 0
                account.merge(:kind => :unmanaged)
            when 1
                account.merge(
                    :kind => :workspace,
                    :email => emails.first,
                    :password => WorkspaceCredentials.password_for_email(
                        emails.first,
                        @password_salt,
                    ),
                )
            else
                account.merge(:kind => :ambiguous, :emails => emails)
            end
        end

        def account_label(entry)
            "#{entry.fetch('user').inspect}@#{entry.fetch('host').inspect}"
        end

        def migratable?(entry)
            [:workspace, :root].include?(entry[:kind])
        end

        def print_plan(entry)
            case entry[:kind]
            when :workspace
                @out.puts "[migrate] #{account_label(entry)} <- #{entry[:email]}"
            when :root
                @out.puts "[migrate] #{account_label(entry)} <- MYSQL_ROOT_PASSWORD"
            when :ambiguous
                @out.puts "[skip]    #{account_label(entry)} - ambiguous workspace users: #{entry[:emails].join(', ')}"
            when :unmanaged
                @out.puts "[skip]    #{account_label(entry)} - unmanaged/system account"
            end
        end

        def print_plan_summary(planned)
            @out.puts
            @out.puts "#{planned.count { |entry| migratable?(entry) }} account(s) can be migrated."
            @out.puts "#{planned.count { |entry| entry[:kind] == :ambiguous }} ambiguous workspace account(s)."
            @out.puts "#{planned.count { |entry| entry[:kind] == :unmanaged }} unmanaged/system account(s) remain."
        end

        def quoted(value)
            "'#{@mysql.escape(value.to_s)}'"
        end

        def alter_user_sql(entry)
            format(
                ALTER_USER_SQL,
                :user => quoted(entry.fetch('user')),
                :host => quoted(entry.fetch('host')),
                :password => quoted(entry.fetch(:password)),
            )
        end

        def migrate(entries)
            @out.puts
            entries.each do |entry|
                begin
                    @mysql.query(alter_user_sql(entry))
                    label = entry[:kind] == :root ? 'root' : 'workspace'
                    source = entry[:kind] == :workspace ? " <- #{entry[:email]}" : ''
                    @out.puts "[migrated #{label}] #{account_label(entry)}#{source}"
                rescue StandardError => error
                    @failures << entry.merge(:error => error)
                    @err.puts "[failure] #{account_label(entry)} - #{error.class}: #{error.message}"
                end
            end
        end

        def print_apply_summary(planned, remaining_accounts)
            remaining = remaining_accounts.map { |account| classify(account) }
            remaining.select { |entry| migratable?(entry) }.each do |entry|
                next if failed_account?(entry)

                error = RuntimeError.new(
                    'account still uses mysql_native_password'
                )
                @failures << entry.merge(:error => error)
                @err.puts "[failure] #{account_label(entry)} - #{error.message}"
            end

            successful = planned.select { |entry| migratable?(entry) }.reject do |entry|
                failed_account?(entry)
            end

            @out.puts
            @out.puts 'Migration result:'
            @out.puts "  Workspace-managed accounts migrated: #{successful.count { |entry| entry[:kind] == :workspace }}"
            @out.puts "  Root accounts migrated: #{successful.count { |entry| entry[:kind] == :root }}"
            @out.puts "  Ambiguous workspace accounts remaining: #{remaining.count { |entry| entry[:kind] == :ambiguous }}"
            @out.puts "  Unmanaged/system accounts remaining: #{remaining.count { |entry| entry[:kind] == :unmanaged }}"
            @out.puts "  Failures: #{failures.size}"

            if remaining.empty?
                @out.puts 'No mysql_native_password accounts remain.'
            else
                @out.puts
                @out.puts 'Accounts still using mysql_native_password:'
                remaining.each { |entry| @out.puts "  #{account_label(entry)} (#{entry[:kind]})" }
            end
        end

        def failed_account?(entry)
            @failures.any? do |failure|
                failure['user'] == entry['user'] && failure['host'] == entry['host']
            end
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
            :emails => WorkspaceUsers.new(neo4j).emails,
            :root_password => MYSQL_ROOT_PASSWORD,
            :password_salt => MYSQL_PASSWORD_SALT,
            :apply => apply,
        )
        runner.run ? 0 : 1
    rescue StandardError => error
        warn "Migration aborted: #{error.class}: #{error.message}"
        1
    end
end

exit MysqlAuthMigration.run_cli(ARGV) if $PROGRAM_NAME == __FILE__
