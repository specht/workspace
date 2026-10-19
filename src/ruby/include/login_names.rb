require 'set'

# Names made from a person's e-mail address: the Linux user in a Workspace
# and the MySQL and Neo4j login (DatabaseIdentity). Letters a person
# recognises, in ASCII: »jürgen.müller« becomes »juergen.mueller«, not
# »j-rgen.m-ller« or »jurgen.muller«.
module LoginNames
    # German first (ä → ae, as one writes without umlauts), then letters that
    # have no base letter to decompose into; everything else is decomposed
    # (é → e, ñ → n) and what is left over dropped.
    SPECIAL = {
        'ä' => 'ae', 'ö' => 'oe', 'ü' => 'ue', 'ß' => 'ss',
        'æ' => 'ae', 'œ' => 'oe', 'ø' => 'o', 'ł' => 'l', 'đ' => 'd', 'ð' => 'd', 'þ' => 'th', 'ı' => 'i',
    }.freeze
    SPECIAL_PATTERN = Regexp.union(SPECIAL.keys)

    def self.ascii(text)
        text.to_s.unicode_normalize(:nfc).downcase
            .gsub(SPECIAL_PATTERN) { |c| SPECIAL[c] }
            .unicode_normalize(:nfkd)
            .encode('ASCII', :invalid => :replace, :undef => :replace, :replace => '')
    end

    # The Linux user in the Workspace, from the part before the @: a–z, 0–9,
    # ».«, »_«, »-«, starting with a letter or »_«, at most 32 characters.
    # It need not be unique (every Workspace is a container of its own); the
    # container renames one that collides with a system account (root@ …,
    # docker/code/init-code-server-run).
    def self.linux_login(local_part)
        login = ascii(local_part).gsub(/[^a-z0-9._-]+/, '-').gsub(/\A[.-]+|[.-]+\z/, '')
        login = 'student' if login.empty?
        login = "user-#{login}" unless login.match?(/\A[a-z_]/)
        login[0, 32].sub(/[.-]+\z/, '')
    end
end
