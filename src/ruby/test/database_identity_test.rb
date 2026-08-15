require 'minitest/autorun'
require 'thread'
require_relative '../include/database_identity'

class FakeIdentityNeo4j
    attr_reader :constraint_calls

    def initialize(emails)
        @users = if emails.is_a?(Hash)
            emails.dup
        else
            emails.to_h { |email| [email, nil] }
        end
        @mutex = Mutex.new
        @constraint_calls = 0
    end

    def neo4j_query(query, parameters = {})
        if query.include?('CREATE CONSTRAINT')
            @constraint_calls += 1
            return []
        end
        if query.include?('SET u.db_login')
            return @mutex.synchronize do
                candidate = parameters[:db_login]
                raise 'ConstraintValidationFailed' if @users.value?(candidate)

                email = parameters[:email]
                if @users[email].nil?
                    @users[email] = candidate
                    [{ 'db_login' => candidate }]
                else
                    []
                end
            end
        end
        if query.include?('RETURN u.email AS email, u.db_login AS db_login')
            return @mutex.synchronize do
                @users.map do |email, db_login|
                    { 'email' => email, 'db_login' => db_login }
                end
            end
        end
        if query.include?('{db_login: $db_login}')
            owner = @users.key(parameters[:db_login])
            return owner ? [{ 'email' => owner }] : []
        end

        login = @mutex.synchronize { @users[parameters[:email]] }
        [{ 'db_login' => login }]
    end
end

class DatabaseIdentityTest < Minitest::Test
    def logins(users, occupied: [])
        DatabaseIdentity.plan(users, :occupied => occupied)
            .to_h { |allocation| [allocation.email, allocation.db_login] }
    end

    def test_unique_stem_keeps_plain_readable_login
        assert_equal 'micha.specht', logins([
            { :email => 'Micha.Specht@gmail.com' },
        ])['Micha.Specht@gmail.com']
    end

    def test_valid_readable_stems_are_preserved
        %w[123 ada ada-lovelace ada.lovelace].each do |stem|
            assert_equal stem, logins([
                { :email => "#{stem}@example.com" },
            ])["#{stem}@example.com"]
        end
    end

    def test_one_and_two_character_stems_are_extended_to_minimum_length
        result = logins([
            { :email => 'a@example.com' },
            { :email => 'ab@example.com' },
            { :email => '1@example.com' },
            { :email => '12@example.com' },
        ])

        assert_equal 'a-user', result['a@example.com']
        assert_equal 'ab-user', result['ab@example.com']
        assert_equal '1-user', result['1@example.com']
        assert_equal '12-user', result['12@example.com']
        assert result.values.all? { |login| login.length.between?(3, 32) }
    end

    def test_two_identical_stems_get_one_plain_and_one_suffix
        result = logins([
            { :email => 'specht@ovgu.de' },
            { :email => 'specht@gymnasiumsteglitz.de' },
        ])

        assert_equal 'specht', result['specht@gymnasiumsteglitz.de']
        assert_match(/\Aspecht-[a-z0-9]{4}\z/, result['specht@ovgu.de'])
    end

    def test_three_way_collision_is_unique
        result = logins(%w[
            specht@third.de
            specht@ovgu.de
            specht@gymnasiumsteglitz.de
        ].map { |email| { :email => email } })

        assert_equal 3, result.values.uniq.size
        assert_equal 1, result.values.count('specht')
    end

    def test_plan_is_independent_of_input_order
        users = %w[
            specht@third.de
            specht@ovgu.de
            specht@gymnasiumsteglitz.de
        ].map { |email| { :email => email } }

        assert_equal logins(users), logins(users.reverse)
        assert_equal logins(users), logins(users.rotate)
    end

    def test_persisted_login_is_never_reallocated
        users = [
            { :email => 'first@example.com', :db_login => 'historic-login' },
            { :email => 'historic-login@example.com' },
        ]
        result = logins(users)

        assert_equal 'historic-login', result['first@example.com']
        refute_equal 'historic-login', result['historic-login@example.com']
    end

    def test_unusual_local_parts_are_sanitized
        assert_equal 'micha-tag-thing',
            DatabaseIdentity.stem_for_email('Mïcha+Tag_Thing@example.com')
        assert_equal '123',
            DatabaseIdentity.stem_for_email('123@example.com')
        assert_equal 'user',
            DatabaseIdentity.stem_for_email('!!!@example.com')
        assert_equal 'user-systemadmin',
            DatabaseIdentity.stem_for_email('systemadmin@example.com')
    end

    def test_sanitization_never_leaves_a_trailing_dot_or_dash
        dot_at_limit = "#{'d' * 31}.tail@example.com"
        dash_at_limit = "#{'h' * 31}-tail@example.com"
        result = logins([
            { :email => 'readable.@example.com' },
            { :email => 'another-@example.com' },
            { :email => 'ab.-@example.com' },
            { :email => dot_at_limit },
            { :email => dash_at_limit },
        ])

        assert_equal 'readable', result['readable.@example.com']
        assert_equal 'another', result['another-@example.com']
        assert_equal 'ab-user', result['ab.-@example.com']
        assert_equal 'd' * 31, result[dot_at_limit]
        assert_equal 'h' * 31, result[dash_at_limit]
        result.each_value { |login| refute_match(/[.-]\z/, login) }
    end

    def test_validation_enforces_shared_mysql_and_neo4j_bounds
        %w[a ab].each do |login|
            assert_raises(DatabaseIdentity::AllocationError) do
                DatabaseIdentity.validate!(login)
            end
        end
        assert_raises(DatabaseIdentity::AllocationError) do
            DatabaseIdentity.validate!('a' * 33)
        end
        assert DatabaseIdentity.validate!('abc')
        assert DatabaseIdentity.validate!('a' * 32)
    end

    def test_validation_rejects_trailing_punctuation_and_neo4j_reserved_names
        %w[abc. abc- system systemdb _internal neo4j].each do |login|
            assert_raises(DatabaseIdentity::AllocationError) do
                DatabaseIdentity.validate!(login)
            end
        end
    end

    def test_infrastructure_names_are_never_allocated
        result = logins([
            { :email => 'root@example.com' },
            { :email => 'neo4j@example.com' },
        ])

        refute_equal 'root', result['root@example.com']
        refute_equal 'neo4j', result['neo4j@example.com']
        assert result.values.all? { |login| DatabaseIdentity.validate!(login) }
    end

    def test_login_respects_mysql_maximum_length
        email = "#{'a' * 80}@example.com"
        candidates = DatabaseIdentity.candidates_for_email(email).take(12)

        assert candidates.all? { |candidate| candidate.length <= 32 }
        assert candidates.all? { |candidate| DatabaseIdentity.validate!(candidate) }
    end

    def test_suffix_is_extended_when_short_candidate_is_occupied
        email = 'specht@ovgu.de'
        candidates = DatabaseIdentity.candidates_for_email(email).take(3)
        result = logins([{ :email => email }], :occupied => candidates.take(2))

        assert_equal candidates[2], result[email]
        assert_equal 5, result[email].split('-').last.length
    end

    def test_uniqueness_constraint_and_concurrent_allocation
        emails = ['same@one.example', 'same@two.example']
        neo4j = FakeIdentityNeo4j.new(emails)
        allocator = DatabaseIdentity::Neo4jAllocator.new(neo4j)
        allocator.ensure_constraint!
        ready = Queue.new
        start = Queue.new
        threads = emails.map do |email|
            Thread.new do
                ready << true
                start.pop
                allocator.allocate!(email)
            end
        end
        threads.size.times { ready.pop }
        threads.size.times { start << true }
        assigned = threads.map(&:value)

        assert_equal 1, neo4j.constraint_calls
        assert_equal assigned.size, assigned.uniq.size
        assert_includes assigned, 'same'
        expected = DatabaseIdentity.plan(
            emails.map { |email| { :email => email } }
        ).to_h { |entry| [entry.email, entry.db_login] }
        assert_equal expected.values.sort, assigned.sort
    end

    def test_allocator_returns_persisted_login_without_changing_it
        neo4j = FakeIdentityNeo4j.new(
            'student@example.com' => 'historic-login'
        )
        allocator = DatabaseIdentity::Neo4jAllocator.new(neo4j)

        assert_equal 'historic-login', allocator.allocate!('student@example.com')
    end
end
