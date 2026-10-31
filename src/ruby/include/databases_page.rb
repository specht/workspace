# The databases of a Workspace: starting and resetting a person's MySQL and
# Neo4j database, one's own further MySQL databases (list, create, delete,
# one at a time per person), and the runners for statements as root that
# exams use too. Moved out of main.rb, which loads it.
class Main < Sinatra::Base
    post '/api/start_mysql' do
        assert(user_logged_in?)
        email = @session_user[:email]
        init_mysql(email)
        respond(:yay => 'sure')
    end
    post '/api/reset_mysql' do
        assert(user_logged_in?)
        email = @session_user[:email]
        reset_mysql(email)
        log_event "MySQL database reset by its owner: #{email}"
        respond(:yay => 'sure')
    end
    post '/api/start_neo4j' do
        assert(user_logged_in?)
        email = @session_user[:email]
        init_neo4j(email)
        user = neo4j_query_expect_one(<<~END_OF_QUERY, :email => email)['u']
            MATCH (u:User {email: $email})
            RETURN u;
        END_OF_QUERY
        respond(:yay => 'sure', :browser_url => "#{NEO4J_WEB_ROOT.sub('neo4j.', 'neo4j-' + user[:server_tag] + '.')}?dbms=bolt#{DEVELOPMENT ? '' : '+s'}://bolt.#{WEBSITE_HOST}#{DEVELOPMENT ? '' : ':443'}")
    end
    post '/api/reset_neo4j' do
        assert(user_logged_in?)
        email = @session_user[:email]
        reset_neo4j(email)
        log_event "Neo4j database reset by its owner: #{email}"
        respond(:yay => 'sure')
    end

    def run_mysql_statements(statements)
        Open3.popen2(MYSQL_ROOT_ENV, MYSQL_ROOT_CLIENT) do |stdin, _stdout, wait_thr|
            statements.each { |statement| stdin.puts statement }
            stdin.close
            raise 'MySQL statements failed' unless wait_thr.value.success?
        end
    end
    def run_neo4j_statements(statements)
        Open3.popen2(NEO4J_ROOT_ENV, NEO4J_ROOT_CLIENT) do |stdin, _stdout, wait_thr|
            statements.each { |statement| stdin.puts statement }
            stdin.close
            raise 'Neo4j statements failed' unless wait_thr.value.success?
        end
    end

    post '/api/get_my_mysql_databases' do
        assert(user_logged_in?)
        client = Mysql2::Client.new(
            host: 'mysql',
            username: 'root',
            password: MYSQL_ROOT_PASSWORD
        )

        result = {}
        databases = []
        databases << @session_user[:db_login]
        neo4j_query(<<~END_OF_STRING, {:email => @session_user[:email]}).each do |row|
            MATCH (u:User {email: $email})-[:HAS]->(d:Database {type: 'mysql'})
            RETURN d.name AS name;
        END_OF_STRING
            databases << row['name']
        end
        # Extra databases made before they had a login of their own get it
        # here, the first time the profile lists them.
        result[:logins] = {}
        result[:max_extra] = MYSQL_EXTRA_DATABASES_MAX
        databases.each do |database|
            next unless DatabaseProvisioning.extra_database?(database)

            Main.ensure_extra_database_login(client, @session_user[:email], database)
            result[:logins][database] = {
                :login => database,
                :password => Main.extra_database_password(@session_user[:email], database),
            }
        end
        databases.each do |database|
            result[:databases] ||= []
            result[:databases] << database
            result[:tables] ||= {}
            result[:tables][database] = {}
            result[:total] ||= {}
            result[:total][database] = {
                :rows => 0,
                :size => 0
            }
            client.query("SELECT table_name AS `table`, table_rows AS `rows`, data_length + index_length AS size FROM information_schema.tables WHERE table_schema = '#{database}';").each do |row|
                result[:tables][database][row['table']] = {
                    :rows => row['rows'],
                    :size => row['size'],
                }
                result[:total][database][:rows] += row['rows']
                result[:total][database][:size] += row['size']
            end
        end
        respond(:result => result)
    end
    MYSQL_EXTRA_DATABASES_MAX = 4
    post '/api/create_mysql_database' do
        assert(user_logged_in?)
        mysql_database_lock(@session_user[:email]).synchronize do
            count = neo4j_query_expect_one(<<~END_OF_STRING, {:email => @session_user[:email]})['count']
                MATCH (u:User {email: $email})-[:HAS]->(d:Database {type: 'mysql'})
                RETURN COUNT(d) AS count;
            END_OF_STRING
            assert(count < MYSQL_EXTRA_DATABASES_MAX)
            database_name = "db_#{RandomTag.generate(12)}"
            client = Mysql2::Client.new(
                host: 'mysql',
                username: 'root',
                password: MYSQL_ROOT_PASSWORD
            )
            login = @session_user[:db_login]
            client.query("CREATE DATABASE #{database_name};")
            client.query("GRANT ALL ON `#{database_name}`.* TO '#{login}'@'%';")
            Main.ensure_extra_database_login(client, @session_user[:email], database_name)
            client.query("FLUSH PRIVILEGES;")
            neo4j_query_expect_one(<<~END_OF_STRING, {:email => @session_user[:email], :database_name => database_name})
                MATCH (u:User {email: $email})
                CREATE (d:Database {type: 'mysql', name: $database_name})<-[:HAS]-(u)
                RETURN d;
            END_OF_STRING
        end
        respond(:yay => 'sure')
    end
    # The password of an extra database's own login
    # (DatabaseProvisioning.extra_database_login_statements): derived, like
    # the others, so nothing has to be stored.
    def self.extra_database_password(email, database)
        gen_password_for_email("#{email}/#{database}", MYSQL_PASSWORD_SALT)
    end
    def self.ensure_extra_database_login(client, email, database)
        return unless DatabaseProvisioning.extra_database?(database)

        DatabaseProvisioning.extra_database_login_statements(database, extra_database_password(email, database))
            .each { |statement| client.query(statement) }
    end
    # Only one's own databases: the main one (db_login, reset instead) and
    # the ones created in the profile. Anything else was dropped before,
    # for any logged-in user who knew a name.
    def self.own_mysql_database?(email, db_login, name)
        return false unless name.is_a?(String) && name.match?(/\A[A-Za-z0-9_]{1,64}\z/)
        return true if name == db_login

        !$neo4j.neo4j_query(<<~END_OF_STRING, {:email => email, :name => name}).first.nil?
            MATCH (u:User {email: $email})-[:HAS]->(d:Database {type: 'mysql', name: $name})
            RETURN d.name;
        END_OF_STRING
    end
    post '/api/delete_mysql_database' do
        assert(user_logged_in?)
        data = parse_request_data(:required_keys => [:database])
        mysql_database_lock(@session_user[:email]).synchronize do
            assert(Main.own_mysql_database?(@session_user[:email], @session_user[:db_login], data[:database]), 'not_your_database')
            client = Mysql2::Client.new(
                host: 'mysql',
                username: 'root',
                password: MYSQL_ROOT_PASSWORD
            )
            is_user_db = (data[:database] == @session_user[:db_login])
            client.query("DROP DATABASE IF EXISTS `#{data[:database]}`;")
            log_event "MySQL database #{data[:database]} deleted by its owner: #{@session_user[:email]}"
            if DatabaseProvisioning.extra_database?(data[:database])
                client.query(DatabaseProvisioning.extra_database_drop_login_statement(data[:database]))
            end
            neo4j_query(<<~END_OF_STRING, {:email => @session_user[:email], :database => data[:database]})
                MATCH (u:User {email: $email})-[:HAS]->(d:Database {type: 'mysql', name: $database})
                DETACH DELETE d;
            END_OF_STRING
            if is_user_db
                login = @session_user[:db_login]
                client.query("CREATE DATABASE `#{login}`;")
                client.query("GRANT ALL ON `#{login}`.* TO '#{login}'@'%';")
                client.query("FLUSH PRIVILEGES;")
            end
        end
    end

    def mysql_database_lock(email)
        @@mysql_database_locks_mutex.synchronize do
            @@mysql_database_locks[email] ||= Mutex.new
        end
    end
end
