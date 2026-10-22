require 'cgi'
require 'json'

# The class repository of a sandbox page, set per group by its teachers
# (docs/teacher-access.md, »Klassen-Repository«; docs/aufgaben.md).
#
# A page that clones one says so in its meta block,
#
#   class_repo: group
#
# and uses ${class_repo} (the address), ${git_host} (its server),
# ${git_web} (https://server/), ${git_ssh} (user@server for ssh -T) and
# ${git_server_hint} in its text. What
# only makes sense with a class repository, or only without one, stands
# between <!-- if: class_repo --> or <!-- if: !class_repo --> and
# <!-- /if --> (ContentParts.filter_conditions). The page is rendered in
# both variants when the content is loaded; each request picks one and
# fills in the variables (Main#page_html).
#
# Stored as JSON { slug => address } in GroupSetting.class_repos, next to
# the group's programming language: renaming the group takes it along,
# deleting the group deletes it. Plain Ruby, tested on its own.
module ClassRepos
    # The meta value that switches a page to per-group class repositories.
    META_VALUE = 'group'
    # The variants of a page (nil: with a class repository over SSH, the
    # page as teachers see it) and the values of the conditions in each
    # (<!-- if: class_repo -->, <!-- if: class_repo_https --> …).
    WITHOUT = 'without_class_repo'
    HTTPS = 'class_repo_https'
    CONDITIONS = %w[class_repo class_repo_ssh class_repo_https].freeze
    CONDITION_VALUES = {
        nil => { 'class_repo' => true, 'class_repo_ssh' => true, 'class_repo_https' => false },
        HTTPS => { 'class_repo' => true, 'class_repo_ssh' => false, 'class_repo_https' => true },
        WITHOUT => { 'class_repo' => false, 'class_repo_ssh' => false, 'class_repo_https' => false },
    }.freeze
    MAX_LENGTH = 300

    LABEL = /[A-Za-z0-9](?:[A-Za-z0-9-]{0,61}[A-Za-z0-9])?/
    HOST = /#{LABEL}(?:\.#{LABEL})*/
    SEGMENT = /[A-Za-z0-9_~][A-Za-z0-9._~-]*/
    REPO_PATH = %r{#{SEGMENT}(?:/#{SEGMENT})*}
    USER = /[A-Za-z0-9_][A-Za-z0-9._-]*/
    # git@host:path, ssh://[user@]host[:port]/path, https://host[:port]/path;
    # nothing else, so the address is safe in a copied shell command.
    FORMS = [
        /\A(?<user>#{USER})@(?<host>#{HOST}):(?<path>#{REPO_PATH})\z/,
        %r{\Assh://(?:(?<user>#{USER})@)?(?<host>#{HOST})(?::(?<port>[0-9]{1,5}))?/(?<path>#{REPO_PATH})\z},
        %r{\Ahttps://(?<host>#{HOST})(?::(?<port>[0-9]{1,5}))?/(?<path>#{REPO_PATH})\z},
    ].freeze

    # Placeholders for those who see no address: teachers, administrators
    # and visitors, and the Handbuch page for people without one.
    EXAMPLE_HOST = 'git.example.org'
    PLACEHOLDERS = {
        'class_repo' => '[Adresse des Klassen-Repositorys]',
        'git_host' => EXAMPLE_HOST,
        'git_web' => "https://#{EXAMPLE_HOST}/",
        'git_ssh' => "git@#{EXAMPLE_HOST}",
        'git_server_hint' => " (das ist ein Beispiel: Welchen Server deine Klasse benutzt, sagt dir deine Lehrkraft)",
    }.freeze
    # The variables filled in per request. In the rendered page they are
    # tokens of letters only, which Markdown, the highlighter and Nokogiri
    # leave in one piece.
    TOKENS = {
        'class_repo' => 'HSVARCLASSREPOX',
        'git_host' => 'HSVARGITHOSTX',
        'git_web' => 'HSVARGITWEBX',
        'git_ssh' => 'HSVARGITSSHX',
        'git_server_hint' => 'HSVARGITHINTX',
    }.freeze
    VARIABLES = TOKENS.keys.freeze
    TOKEN = /#{TOKENS.values.join('|')}/

    # The address without surrounding spaces if it is one of FORMS, else nil.
    def self.normalize(address)
        text = address.to_s.strip
        return nil if text.empty? || text.length > MAX_LENGTH
        return nil if text.split('/').any? { |segment| segment == '..' || segment == '.' }

        m = FORMS.lazy.map { |form| form.match(text) }.find(&:itself)
        return nil unless m
        return nil if m.names.include?('port') && m[:port] && !(1..65535).cover?(m[:port].to_i)

        text
    end

    def self.valid?(address)
        !normalize(address).nil?
    end

    # { user:, host:, port: } of a valid address (user and port nil where
    # it names none), else nil.
    def self.parts(address)
        text = normalize(address)
        return nil unless text

        FORMS.each do |form|
            m = form.match(text)
            next unless m

            names = m.names
            return { :user => names.include?('user') ? m[:user] : nil, :host => m[:host].downcase,
                     :port => names.include?('port') ? m[:port] : nil }
        end
        nil
    end

    def self.host(address)
        (parts(address) || {})[:host]
    end

    # The variants a page with these conditions is rendered in: nil always,
    # WITHOUT if it has any, HTTPS if it tells SSH and HTTPS apart.
    def self.variants(conditions)
        return [nil] if conditions.empty?

        conditions.any? { |c| c != 'class_repo' } ? [nil, HTTPS, WITHOUT] : [nil, WITHOUT]
    end

    # The variant for an address: WITHOUT for none, HTTPS for an https://
    # address, nil (SSH) for the others.
    def self.variant(address)
        text = normalize(address)
        return WITHOUT unless text

        text.start_with?('https://') ? HTTPS : nil
    end

    # The values of the variables for an address; the placeholders for nil.
    # git_ssh is what `ssh -T` needs to greet the server: the address's user
    # (git if it names none, as for https), and -p for another port of an
    # ssh:// address; git_web keeps the port of an https:// address.
    def self.values(address)
        p = address && parts(address)
        return PLACEHOLDERS.dup unless p

        text = normalize(address)
        ssh_port = text.start_with?('ssh://') && p[:port] ? "-p #{p[:port]} " : ''
        web_port = text.start_with?('https://') && p[:port] ? ":#{p[:port]}" : ''
        { 'class_repo' => text, 'git_host' => p[:host], 'git_web' => "https://#{p[:host]}#{web_port}/",
          'git_ssh' => "#{ssh_port}#{p[:user] || 'git'}@#{p[:host]}", 'git_server_hint' => '' }
    end

    # Is the page one with a class repository per group?
    def self.page?(meta)
        meta.is_a?(Hash) && meta['class_repo'].to_s.strip == META_VALUE
    end

    # GroupSetting.class_repos (JSON) as { slug => address }, only valid
    # entries; {} for nil or anything unreadable.
    def self.parse(json)
        data = json.to_s.empty? ? {} : JSON.parse(json.to_s)
        return {} unless data.is_a?(Hash)

        data.each_with_object({}) do |(slug, address), out|
            text = normalize(address)
            out[slug.to_s] = text if text && slug.to_s =~ /\A[a-z0-9][a-z0-9-]*\z/
        end
    rescue JSON::ParserError
        {}
    end

    # The map with one entry set (address) or removed (nil or empty), as
    # JSON for GroupSetting.class_repos; nil when nothing is left.
    def self.update(json, slug, address)
        map = parse(json)
        text = normalize(address)
        text ? map[slug] = text : map.delete(slug)
        map.empty? ? nil : JSON.generate(map.sort.to_h)
    end

    # The address a person sees on a page: that of the first of their groups
    # (in the order given) with one for this page; nil if none has one.
    # settings: { group => { slug => address } }
    def self.for_page(slug, groups, settings)
        groups.each do |group|
            address = (settings[group] || {})[slug]
            return address if address
        end
        nil
    end

    # For the Handbuch page: the first address of the first group that has
    # any, so its server is the one of the person's class.
    def self.first_of(groups, settings)
        groups.each do |group|
            map = settings[group] || {}
            return map[map.keys.min] unless map.empty?
        end
        nil
    end

    # How a person sees a page: { variant: nil (with a class repository over
    # SSH), HTTPS or WITHOUT, address: the address or nil (placeholders) }.
    # placeholders: teachers, administrators and visitors, who see the page
    # with a class repository and the placeholders. Students see the
    # address of the first of their groups that has one for this page, and
    # without one the variant without; on other pages (the Handbuch) the
    # first address of their groups, if any.
    def self.view(slug, per_group:, placeholders:, groups:, settings:)
        return { :variant => nil, :address => nil } if placeholders
        return { :variant => nil, :address => first_of(groups, settings) } unless per_group

        address = for_page(slug, groups, settings)
        { :variant => variant(address), :address => address }
    end

    # The tasks of a page (content[:tasks]) as they are in a variant: its
    # own :on_page instead of the page's. nil: the page as it is.
    def self.tasks(content, variant)
        own = variant && (content[:variants] || {})[variant]
        own ? content[:tasks].merge(:on_page => own[:on_page]) : content[:tasks]
    end

    # Fills the tokens of a rendered page with the values (HTML-escaped; the
    # hint is text too).
    def self.fill(html, values)
        html.gsub(TOKEN) do |token|
            name = TOKENS.key(token)
            CGI.escapeHTML(values[name].to_s)
        end
    end

    # The tokens with the placeholders, for text that is not shown per
    # person (the Handbuch search).
    def self.neutral(text)
        text.is_a?(String) ? text.gsub(TOKEN) { |token| PLACEHOLDERS[TOKENS.key(token)] } : text
    end
end
