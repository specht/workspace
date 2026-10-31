require 'minitest/autorun'
require_relative '../include/command_redaction'

# What a failed command may show of itself in the log (CommandRedaction).
class CommandRedactionTest < Minitest::Test
    def r(command)
        CommandRedaction.redact(command)
    end

    def test_password_options
        assert_equal 'docker exec -i x mysql --user=root --password=***', r('docker exec -i x mysql --user=root --password=geheim')
        assert_equal 'mysql --password *** -e x', r('mysql --password geheim -e x')
        assert_equal 'tool --token=*** --api-key ***', r("tool --token=abc --api-key 'x y'")
    end

    def test_mysql_and_cypher_shell_short_forms
        assert_equal 'mysqldump --user=root -p*** db', r('mysqldump --user=root -pgeheim db')
        assert_equal 'bin/cypher-shell -u neo4j -p *** "RETURN 1"', r('bin/cypher-shell -u neo4j -p geheim "RETURN 1"')
    end

    def test_assignments_and_urls
        assert_equal 'MYSQL_PASSWORD=*** docker run -e NEO4J_PASSWORD=*** img', r('MYSQL_PASSWORD=a1 docker run -e NEO4J_PASSWORD=b2 img')
        assert_equal 'git clone https://student:***@git.example.org/r.git', r('git clone https://student:s3cr3t@git.example.org/r.git')
    end

    # what is no secret stays as it is
    def test_harmless_commands_stay
        [
            'mkdir -p /workspace/a',
            'nc -l -p 1234',
            'docker exec -i -e MYSQL_PWD workspace_mysql_1 mysql --user=root',
            'docker run -e MYSQL_PASSWORD -e NEO4J_PASSWORD --name hs_code_x img',
            'git clone https://git.example.org/r.git',
            'docker kill -s HUP workspace_nginx_1',
        ].each { |c| assert_equal c, r(c) }
    end
end
