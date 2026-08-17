require_relative 'database_identity'
require_relative 'atomic_file'

module DatabaseProvisioning
    NEO4J_ROLE_PATTERN = /\A[a-z][a-z0-9_]*\z/

    def self.mysql_statements(login, password)
        DatabaseIdentity.validate!(login)
        quoted_password = mysql_string(password)
        [
            "CREATE USER IF NOT EXISTS '#{login}'@'%' IDENTIFIED WITH caching_sha2_password BY #{quoted_password};",
            "ALTER USER '#{login}'@'%' IDENTIFIED WITH caching_sha2_password BY #{quoted_password};",
            "CREATE DATABASE IF NOT EXISTS `#{login}`;",
            "GRANT ALL ON `#{login}`.* TO '#{login}'@'%';",
            'FLUSH PRIVILEGES;',
        ]
    end

    def self.neo4j_statements(login, password)
        DatabaseIdentity.validate!(login)
        quoted_password = cypher_string(password)
        existing = neo4j_existing_identity_statements(login, password)
        [
            "CREATE USER `#{login}` IF NOT EXISTS SET PLAINTEXT PASSWORD #{quoted_password} CHANGE NOT REQUIRED;",
            "CREATE DATABASE `#{login}` IF NOT EXISTS;",
            *existing.drop(1),
        ]
    end

    def self.neo4j_existing_user_statements(login, password)
        DatabaseIdentity.validate!(login)
        quoted_password = cypher_string(password)
        [
            "ALTER USER `#{login}` SET PLAINTEXT PASSWORD #{quoted_password} CHANGE NOT REQUIRED;",
        ]
    end

    def self.neo4j_existing_identity_statements(login, password)
        role = neo4j_role_name(login)
        [
            *neo4j_existing_user_statements(login, password),
            "CREATE ROLE `#{role}` IF NOT EXISTS;",
            "GRANT ACCESS ON DATABASE `#{login}` TO `#{role}`;",
            "GRANT ALL ON GRAPH `#{login}` TO `#{role}`;",
            "GRANT CREATE NEW NODE LABEL ON DATABASE `#{login}` TO `#{role}`;",
            "GRANT CREATE NEW RELATIONSHIP TYPE ON DATABASE `#{login}` TO `#{role}`;",
            "GRANT CREATE NEW PROPERTY NAME ON DATABASE `#{login}` TO `#{role}`;",
            "GRANT CREATE CONSTRAINTS ON DATABASE `#{login}` TO `#{role}`;",
            "GRANT DROP CONSTRAINTS ON DATABASE `#{login}` TO `#{role}`;",
            "GRANT SHOW CONSTRAINTS ON DATABASE `#{login}` TO `#{role}`;",
            "GRANT CREATE INDEXES ON DATABASE `#{login}` TO `#{role}`;",
            "GRANT DROP INDEXES ON DATABASE `#{login}` TO `#{role}`;",
            "GRANT SHOW INDEXES ON DATABASE `#{login}` TO `#{role}`;",
            "GRANT ROLE `#{role}` TO `#{login}`;",
            "ALTER USER `#{login}` SET HOME DATABASE `#{login}`;",
        ]
    end

    def self.neo4j_role_name(login)
        DatabaseIdentity.validate!(login)
        return login if login.match?(NEO4J_ROLE_PATTERN)

        encoded = login.gsub('-', '_dash_').gsub('.', '_dot_')
        "workspace_#{encoded}"
    end

    def self.my_cnf(login, password)
        DatabaseIdentity.validate!(login)
        <<~INI
            [client]
            user = #{login}
            password = #{password}
            host = mysql
            database = #{login}
            port = 3306
        INI
    end

    def self.sync_my_cnf(path, login, password)
        desired = my_cnf(login, password)
        return false if File.exist?(path) && File.read(path) == desired

        AtomicFile.write(path, desired)
        true
    end

    def self.workspace_environment(login, mysql_password, neo4j_password)
        DatabaseIdentity.validate!(login)
        {
            'MYSQL_HOST' => 'mysql',
            'MYSQL_USER' => login,
            'MYSQL_PASSWORD' => mysql_password,
            'MYSQL_DATABASE' => login,
            'NEO4J_URI' => 'neo4j://neo4j:7687',
            'NEO4J_USERNAME' => login,
            'NEO4J_PASSWORD' => neo4j_password,
            'NEO4J_DATABASE' => login,
        }
    end

    def self.mysql_string(value)
        escaped = value.to_s.gsub('\\') { '\\\\' }.gsub("'") { "\\'" }
        "'#{escaped}'"
    end

    def self.cypher_string(value)
        escaped = value.to_s.gsub('\\') { '\\\\' }.gsub("'") { "\\'" }
        "'#{escaped}'"
    end
end
