require 'digest'
require 'set'

module DatabaseIdentity
    MAX_LOGIN_LENGTH = 32
    MIN_LOGIN_LENGTH = 3
    INITIAL_SUFFIX_LENGTH = 4
    SUFFIX_ALPHABET = '23456789bcdfghjkmnpqrstvwxyz'.freeze
    # MySQL 8.4 limits account names to 32 characters. Neo4j database names
    # supply the remaining restrictions used here: 3+ lowercase ASCII
    # alphanumeric/dot/dash characters, no trailing punctuation, and no
    # internal "system" prefix.
    LOGIN_PATTERN = /\A[a-z0-9][a-z0-9.-]*[a-z0-9]\z/
    NEO4J_RESERVED_PREFIXES = %w[system].freeze
    RESERVED_LOGINS = Set.new(%w[
        healthchecker
        information_schema
        mysql
        mysql.infoschema
        mysql.session
        mysql.sys
        neo4j
        performance_schema
        root
        sys
        system
    ]).freeze
    DB_LOGIN_CONSTRAINT = <<~CYPHER.freeze
        CREATE CONSTRAINT user_db_login IF NOT EXISTS
        FOR (u:User) REQUIRE u.db_login IS UNIQUE
    CYPHER

    Allocation = Struct.new(
        :email,
        :db_login,
        :stored,
        :stem,
        keyword_init: true,
    )

    class AllocationError < StandardError; end

    def self.normalized_email(email)
        email.to_s.strip.downcase
    end

    # This is migration-only knowledge of the identity used before db_login.
    def self.legacy_login_for_email(email)
        email.split('@').first.downcase
    end

    def self.stem_for_email(email)
        local = normalized_email(email).split('@', 2).first.to_s
        stem = local.unicode_normalize(:nfkd)
            .encode('ASCII', :invalid => :replace, :undef => :replace, :replace => '')
            .gsub(/[^a-z0-9.]+/, '-')
            .gsub(/[.-]{2,}/, '-')
            .gsub(/\A[.-]+|[.-]+\z/, '')
        stem = 'user' if stem.empty?
        stem = "user-#{stem}" if stem.start_with?('system')
        stem = "#{stem}-user" if stem.length < MIN_LOGIN_LENGTH
        trim(stem, MAX_LOGIN_LENGTH)
    end

    def self.candidates_for_email(email)
        Enumerator.new do |candidates|
            stem = stem_for_email(email)
            candidates << stem
            digest = compact_digest(normalized_email(email))
            maximum_suffix_length = MAX_LOGIN_LENGTH - MIN_LOGIN_LENGTH - 1
            (INITIAL_SUFFIX_LENGTH..maximum_suffix_length).each do |length|
                base = trim(stem, MAX_LOGIN_LENGTH - length - 1)
                candidates << "#{base}-#{digest[0, length]}"
            end
        end
    end

    def self.ephemeral_login(db_login, discriminator)
        validate!(db_login)
        digest = compact_digest(discriminator.to_s)[0, 8]
        base = trim(db_login, MAX_LOGIN_LENGTH - digest.length - 3)
        "#{base}-t-#{digest}"
    end

    def self.plan(users, occupied: [])
        records = users.map do |user|
            {
                :email => value(user, :email),
                :db_login => value(user, :db_login),
            }
        end
        used = RESERVED_LOGINS | Set.new(occupied.compact)
        persisted = records.select { |user| present?(user[:db_login]) }
        persisted.each do |user|
            validate!(user[:db_login])
            unless used.add?(user[:db_login])
                raise AllocationError,
                    "database login is already assigned: #{user[:db_login]}"
            end
        end

        allocations = persisted.map do |user|
            Allocation.new(
                :email => user[:email],
                :db_login => user[:db_login],
                :stored => true,
                :stem => stem_for_email(user[:email]),
            )
        end

        records.reject { |user| present?(user[:db_login]) }
            .sort_by { |user| normalized_email(user[:email]) }
            .each do |user|
                candidate = candidates_for_email(user[:email]).find do |login|
                    valid?(login) && !used.include?(login)
                end
                raise AllocationError, "cannot allocate database login for #{user[:email]}" unless candidate

                validate!(candidate)
                used << candidate
                allocations << Allocation.new(
                    :email => user[:email],
                    :db_login => candidate,
                    :stored => false,
                    :stem => stem_for_email(user[:email]),
                )
            end

        allocations.sort_by { |allocation| normalized_email(allocation.email) }
    end

    def self.valid?(login)
        login.is_a?(String) &&
            login.length.between?(MIN_LOGIN_LENGTH, MAX_LOGIN_LENGTH) &&
            login.match?(LOGIN_PATTERN) &&
            NEO4J_RESERVED_PREFIXES.none? { |prefix| login.start_with?(prefix) } &&
            !RESERVED_LOGINS.include?(login)
    end

    def self.validate!(login)
        raise AllocationError, "invalid database login: #{login.inspect}" unless valid?(login)

        login
    end

    def self.trim(value, length)
        value[0, length].gsub(/[.-]+\z/, '')
    end
    private_class_method :trim

    def self.compact_digest(value)
        number = Digest::SHA256.hexdigest(value).to_i(16)
        encoded = +''
        while number.positive?
            encoded << SUFFIX_ALPHABET[number % SUFFIX_ALPHABET.length]
            number /= SUFFIX_ALPHABET.length
        end
        encoded.reverse
    end
    private_class_method :compact_digest

    def self.value(object, key)
        object[key] || object[key.to_s]
    end
    private_class_method :value

    def self.present?(value)
        value.is_a?(String) && !value.empty?
    end
    private_class_method :present?

    class Neo4jAllocator
        def initialize(neo4j)
            @neo4j = neo4j
        end

        def ensure_constraint!
            @neo4j.neo4j_query(DB_LOGIN_CONSTRAINT)
        end

        def fetch(email)
            row = @neo4j.neo4j_query(<<~CYPHER, :email => email).first
                MATCH (u:User {email: $email})
                RETURN u.db_login AS db_login
            CYPHER
            row && row['db_login']
        end

        def allocate!(email)
            10.times do
                stored = fetch(email)
                return DatabaseIdentity.validate!(stored) if stored

                allocation = DatabaseIdentity.plan(all_users).find do |entry|
                    DatabaseIdentity.normalized_email(entry.email) ==
                        DatabaseIdentity.normalized_email(email)
                end
                raise AllocationError, "workspace user does not exist: #{email}" unless allocation

                claimed = claim(email, allocation.db_login)
                return claimed if claimed
            end
            raise AllocationError, "cannot allocate database login for #{email}"
        end

        def claim!(email, candidate)
            DatabaseIdentity.validate!(candidate)
            stored = fetch(email)
            return DatabaseIdentity.validate!(stored) if stored

            claimed = claim(email, candidate)
            return claimed if claimed

            allocate!(email)
        end

        private

        def all_users
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

        def claim(email, candidate)
            parameters = {
                :email => email,
                :db_login => candidate,
            }
            rows = @neo4j.neo4j_query(<<~CYPHER, parameters).to_a
                MATCH (u:User {email: $email})
                WHERE u.db_login IS NULL
                SET u.db_login = $db_login
                RETURN u.db_login AS db_login
            CYPHER
            return rows.first['db_login'] unless rows.empty?

            stored = fetch(email)
            return DatabaseIdentity.validate!(stored) if stored

            nil
        rescue StandardError => error
            stored = fetch(email)
            return DatabaseIdentity.validate!(stored) if stored

            occupied = @neo4j.neo4j_query(<<~CYPHER, :db_login => candidate).to_a
                MATCH (u:User {db_login: $db_login})
                RETURN u.email AS email
            CYPHER
            return nil unless occupied.empty?

            raise error
        end
    end
end
