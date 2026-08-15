require_relative 'database_identity'
require_relative 'atomic_file'

module DatabaseProvisioning
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
            existing.first,
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
        [
            *neo4j_existing_user_statements(login, password),
            "CREATE ROLE `#{login}` IF NOT EXISTS;",
            "GRANT ACCESS ON DATABASE `#{login}` TO `#{login}`;",
            "GRANT ALL ON GRAPH `#{login}` TO `#{login}`;",
            "GRANT CREATE NEW NODE LABEL ON DATABASE `#{login}` TO `#{login}`;",
            "GRANT CREATE NEW RELATIONSHIP TYPE ON DATABASE `#{login}` TO `#{login}`;",
            "GRANT CREATE NEW PROPERTY NAME ON DATABASE `#{login}` TO `#{login}`;",
            "GRANT CREATE CONSTRAINTS ON DATABASE `#{login}` TO `#{login}`;",
            "GRANT DROP CONSTRAINTS ON DATABASE `#{login}` TO `#{login}`;",
            "GRANT SHOW CONSTRAINTS ON DATABASE `#{login}` TO `#{login}`;",
            "GRANT CREATE INDEXES ON DATABASE `#{login}` TO `#{login}`;",
            "GRANT DROP INDEXES ON DATABASE `#{login}` TO `#{login}`;",
            "GRANT SHOW INDEXES ON DATABASE `#{login}` TO `#{login}`;",
            "GRANT ROLE `#{login}` TO `#{login}`;",
            "ALTER USER `#{login}` SET HOME DATABASE `#{login}`;",
        ]
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
