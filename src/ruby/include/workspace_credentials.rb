require 'digest'

module WorkspaceCredentials
    PASSWORD_CHARACTERS = 'BCDFGHJKMNPQRSTVWXYZ23456789'.chars.freeze

    def self.password_for_email(email, salt)
        sha2 = Digest::SHA256.new
        sha2 << salt
        sha2 << email
        random = Random.new(sha2.hexdigest.to_i(16))
        password = ''
        8.times do
            character = PASSWORD_CHARACTERS.sample(:random => random).dup
            character.downcase! if random.rand(2) == 1
            password += character
        end
        password += '-'
        4.times do
            character = PASSWORD_CHARACTERS.sample(:random => random).dup
            character.downcase! if random.rand(2) == 1
            password += character
        end
        password
    end
end
