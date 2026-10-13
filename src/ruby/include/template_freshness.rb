require 'net/http'
require 'uri'

# Is a student's clone of a template (sandbox, Anaglyph viewer, BIF, …) out of
# date? (docs/aufgaben.md, »Neue Version einer Vorlage«)
#
# The page's meta block names the template (template:, template_name:). The
# server asks the template's own host which commit is current: an HTTPS
# request to <url>/info/refs, like `git ls-remote`, for a public address
# from the page, never for a student's repository. Then a fixed script looks
# in the student's running Workspace, as the student, whether their clone
# already contains that commit. It reads only: nothing is fetched, pulled or
# changed. The student gets a hint to run `git pull` themselves.
module TemplateFreshness
    FOLDER = /\A[A-Za-z0-9][A-Za-z0-9._-]*\z/
    SHA = /\A\h{40}\z/

    # From a page's meta block: { url:, folder:, name: } or nil.
    def self.from_meta(meta)
        url = meta['template']
        return nil unless url.is_a?(String) && url.strip =~ %r{\Ahttps://[^\s/]+/\S+\z}

        url = url.strip
        folder = File.basename(URI.parse(url).path.to_s).sub(/\.git\z/, '')
        return nil unless folder =~ FOLDER && !folder.include?('..')

        name = meta['template_name'].is_a?(String) && !meta['template_name'].strip.empty? ? meta['template_name'].strip : nil
        { :url => url, :folder => folder, :name => name }
    rescue URI::InvalidURIError
        nil
    end

    # host/path, lower case, without scheme, user, .git and trailing slash,
    # so that https://github.com/specht/anaglyph.git and
    # git@github.com:specht/anaglyph are the same repository.
    def self.normalize(url)
        s = url.to_s.strip.downcase
        s = s.sub(%r{\A[a-z+]+://}, '')
        s = s.sub(/\A[^@\/]+@/, '')
        s = s.sub(/\A([^\/:]+):(?!\d+\/)/, '\1/')
        s = s.sub(%r{\A([^/]+):\d+/}, '\1/')
        s.sub(%r{/+\z}, '').sub(/\.git\z/, '').sub(%r{/+\z}, '')
    end

    def self.same_repo?(a, b)
        !a.to_s.strip.empty? && normalize(a) == normalize(b)
    end

    # --- the template's current commit --------------------------------------

    # The commit HEAD points to, from a smart-HTTP ref advertisement
    # (<url>/info/refs?service=git-upload-pack), or nil.
    def self.head_from_advertisement(body)
        data = body.to_s.b
        pos = 0
        first = nil
        while pos + 4 <= data.size
            len = data[pos, 4].to_i(16)
            if len < 4
                pos += 4
                next
            end
            line = data[pos + 4, len - 4].to_s
            pos += len
            next if line.start_with?('#')

            ref = line.split("\0", 2).first.to_s.strip
            sha, name = ref.split(' ', 2)
            next unless sha =~ SHA

            return sha.downcase if name == 'HEAD'

            first ||= sha.downcase
        end
        first
    end

    # Asks the template's host. Returns the commit or nil (unreachable,
    # private, not a git repository).
    def self.fetch_head(url, timeout: 5)
        uri = URI.parse("#{url.sub(%r{/+\z}, '')}/info/refs?service=git-upload-pack")
        3.times do
            return nil unless uri.scheme == 'https'

            response = Net::HTTP.start(uri.host, uri.port, :use_ssl => true,
                    :open_timeout => timeout, :read_timeout => timeout) do |http|
                request = Net::HTTP::Get.new(uri)
                request['User-Agent'] = 'git/2.43.0 (Hackschule Workspace)'
                http.request(request)
            end
            case response
            when Net::HTTPRedirection
                uri = URI.join(uri.to_s, response['location'].to_s)
            when Net::HTTPSuccess
                return nil if response.body.to_s.bytesize > 4_000_000

                return head_from_advertisement(response.body)
            else
                return nil
            end
        end
        nil
    rescue StandardError
        nil
    end

    # Remembers the answer per template for an hour, so that page views do
    # not each ask the host; nothing asks in the background. A teacher can
    # ask again at once (refresh). Failures are remembered for a shorter time.
    class LatestCache
        def initialize(ttl: 3600, failure_ttl: 120, clock: -> { Time.now.to_i }, fetcher: TemplateFreshness.method(:fetch_head))
            @ttl = ttl
            @failure_ttl = failure_ttl
            @clock = clock
            @fetcher = fetcher
            @entries = {}
            @lock = Mutex.new
        end

        def get(url)
            now = @clock.call
            @lock.synchronize do
                entry = @entries[url]
                return entry[:sha] if entry && now < entry[:until]
            end
            sha = @fetcher.call(url)
            @lock.synchronize do
                @entries[url] = { :sha => sha, :until => now + (sha ? @ttl : @failure_ttl) }
            end
            sha
        end

        # Asks the host again, whatever is remembered: a teacher's »Neueste
        # Version neu abfragen«. Page views after it see the new answer.
        def refresh(url)
            sha = @fetcher.call(url)
            now = @clock.call
            @lock.synchronize do
                @entries[url] = { :sha => sha, :until => now + (sha ? @ttl : @failure_ttl) }
            end
            sha
        end
    end

    # --- the student's clone --------------------------------------------------

    # Runs in the student's container as the student: sh -c SCRIPT sh <dir> <sha>.
    # Read-only git plumbing; no lock files, no lazy fetch from a promisor.
    SCRIPT = <<~'SH'
        export GIT_OPTIONAL_LOCKS=0 GIT_NO_LAZY_FETCH=1 GIT_TERMINAL_PROMPT=0
        cd "$1" 2>/dev/null || { echo "STATE absent"; exit 0; }
        top=$(git rev-parse --show-toplevel 2>/dev/null)
        [ "$top" = "$1" ] || { echo "STATE norepo"; exit 0; }
        printf 'ORIGIN %s\n' "$(git config --get remote.origin.url 2>/dev/null)"
        if git cat-file -e "$2^{commit}" 2>/dev/null; then
            if git merge-base --is-ancestor "$2" HEAD 2>/dev/null; then echo "STATE current"; else echo "STATE fetched"; fi
        else
            echo "STATE behind"
        fi
    SH

    def self.parse(output)
        return nil if output.nil?

        state = output[/^STATE (\w+)$/, 1]
        return nil unless %w[absent norepo current fetched behind].include?(state)

        { :state => state.to_sym, :origin => output[/^ORIGIN (.*)$/, 1].to_s.strip }
    end

    # What to tell: one of
    #   :outdated     the clone of the template lacks its current commit
    #   :current      it has it
    #   :absent       there is no clone in /workspace/<folder>
    #   :other        /workspace/<folder> is not a clone of this template
    #   :unavailable  Workspace not running, or the host did not answer
    def self.status(template, latest_sha, parsed)
        return :unavailable if latest_sha.nil? || parsed.nil?
        return :absent if parsed[:state] == :absent
        return :other if parsed[:state] == :norepo || !same_repo?(parsed[:origin], template[:url])

        parsed[:state] == :current ? :current : :outdated
    end

    def self.what(template)
        template[:name] || "von `#{template[:folder]}`"
    end

    # Markdown for the student (rule 5: the command as it is).
    def self.notice(template)
        "**Es gibt eine neue Version #{what(template)}!** Du solltest `git pull` ausführen:\n\n" \
            "```bash\ncd /workspace/#{template[:folder]}\ngit pull\n```\n\n" \
            'Der Workspace hat nur nachgesehen und nichts verändert. Was tun, wenn `git pull` abbricht, ' \
            'steht im [Handbuch](/reference/workspace#neue-version).'
    end
end
