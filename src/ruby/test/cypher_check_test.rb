require 'minitest/autorun'
require_relative '../include/cypher_check'

class CypherCheckTest < Minitest::Test
    def ok(text)
        query, refusal = CypherCheck.single_read(text)
        assert_nil refusal, "#{text}: #{refusal}"
        query
    end

    def refused(text)
        query, refusal = CypherCheck.single_read(text)
        assert_nil query, "#{text} should be refused"
        refusal
    end

    # Reading queries pass, a trailing ; is removed.
    def test_reading_queries
        assert_equal 'MATCH (m:Movie) RETURN m.title', ok("MATCH (m:Movie) RETURN m.title;\n")
        ok("MATCH (p:Person)-[:DIRECTOR]->(m:Movie)\nWHERE p.name = 'Christopher Nolan'\nRETURN m.title, m.year ORDER BY m.year")
        ok("MATCH (m:Movie) WITH m ORDER BY m.runtime DESC LIMIT 3 RETURN m.title")
        # words inside strings, comments and `quoted names` do not count
        ok("MATCH (m:Movie {title: 'Set It Up'}) RETURN m.title // CREATE nothing")
        ok("MATCH (m:Movie) RETURN m.`set` AS x /* DELETE */")
        ok('MATCH (m:Movie) WHERE m.title CONTAINS "Merge" RETURN m.title')
    end

    # Rule 1: anything that writes or reaches beyond the database is
    # refused before it is sent (the READ transaction is the second lock).
    def test_writing_and_beyond_is_refused
        ['CREATE (:Movie {title: "x"}) RETURN 1',
         'MATCH (m:Movie) SET m.title = "x" RETURN m',
         'MATCH (m:Movie) DETACH DELETE m RETURN count(*)',
         'MERGE (g:Genre {name: "x"}) RETURN g',
         'MATCH (m) REMOVE m.title RETURN m',
         "LOAD CSV FROM 'https://example.org/x.csv' AS row RETURN row",
         'CALL db.labels() YIELD label RETURN label',
         'MATCH (m) CALL { WITH m RETURN m.title AS t } RETURN t',
         'USE system SHOW USERS',
         'match (m) set m.x = 1 return m',
         'MATCH (m) FOREACH (x IN [1] | SET m.x = x) RETURN m'].each do |q|
            assert_match(/verändern|greifen/, refused(q), q)
        end
    end

    def test_shape
        assert_match(/zuerst eine Abfrage/, refused("  \n"))
        assert_match(/ohne `;`/, refused('MATCH (m) RETURN m; MATCH (n) RETURN n'))
        assert_match(/RETURN/, refused('MATCH (m:Movie)'))
        assert_match(/Anführungszeichen/, refused("MATCH (m {title: 'open}) RETURN m"))
    end

    def test_values
        assert_equal '[1, "a", null]', CypherCheck.plain_value([1, 'a', nil])
        assert_equal '{title: "Up", year: 2009}', CypherCheck.plain_value({ 'title' => 'Up', 'year' => 2009 })
        assert_equal 42, CypherCheck.plain_value(42)
    end

    # The comparison is SqlCheck's: other column names and order count the
    # same, order only with sorted_by.
    def test_check_compares_results
        rows = { 'REF' => [['Inception', 2010], ['Interstellar', 2014]] }
        runner = lambda do |q|
            if q.start_with?('REF')
                { :columns => %w[title year], :rows => rows['REF'], :truncated => false }
            else
                { :columns => %w[jahr t], :rows => [[2014, 'Interstellar'], [2010, 'Inception']], :truncated => false }
            end
        end
        status, = CypherCheck.check({}, { 'expect_query' => 'REF' }, 'MATCH (m) RETURN m.year, m.title', runner)
        assert_equal :correct, status
        status, message = CypherCheck.check({}, { 'expect_query' => 'REF', 'sorted_by' => '2' }, 'MATCH (m) RETURN m.year, m.title', runner)
        assert_equal :wrong, status
        assert message
    end

    def test_error_messages
        message = CypherCheck.error_message({ :code => 'Neo.ClientError.Statement.SyntaxError', :message => "Invalid input 'RETRUN'" })
        assert_includes message, 'versteht die Abfrage nicht'
        assert_includes message, 'RETRUN'
        assert_includes CypherCheck.error_message({ :code => 'x', :message => 'Writing in read access mode not allowed' }), 'nur Abfragen erlaubt, die lesen'
    end

    # A typical mistake is recognised by its rows, also when the task asks
    # for an order (sorted_by) that the typical wrong query does not have.
    def test_feedback_with_sorted_by
        results = {
            'REF' => [['B', 2011], ['A', 2010]],
            'WRONG' => [['B', 2011]],
        }
        runner = lambda do |q|
            key = q.start_with?('REF') ? 'REF' : 'WRONG'
            { :columns => %w[t y], :rows => results[key], :truncated => false }
        end
        task = { 'feedback' => [{ 'when' => { 'same_result_as' => 'WRONG' }, 'say' => 'Mit `>` fehlt 2010.' }] }
        status, message = CypherCheck.check(task, { 'expect_query' => 'REF', 'sorted_by' => '2 DESC' },
            'MATCH (m) WHERE m.y > 2010 RETURN m.t, m.y ORDER BY m.y DESC', runner)
        assert_equal :wrong, status
        assert_equal 'Mit `>` fehlt 2010.', message
    end
end
