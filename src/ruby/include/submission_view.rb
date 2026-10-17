require 'openssl'
require 'rubygems/package'
require 'zlib'

# Viewing a hand-in in the browser (class page): the files of a stored
# submission (SubmissionStore, a .tar.gz) are served from a host of their
# own, abgaben.<host>, under a signed address that is valid for a while.
#
# What a student wrote runs there, never on the site itself: each answer
# carries »Content-Security-Policy: sandbox«, so the page has an opaque
# origin. It gets no cookies of the site or of any Workspace subdomain (the
# server-sid cookie is shared with them), and the address itself is the
# only permission, so scripts, modules and fetch work with
# Access-Control-Allow-Origin: *. Nothing in here executes or decodes the
# files; they are handed to the browser as they are.
module SubmissionView
    LIFETIME = 12 * 3600
    MAX_TEXT = 512 * 1024
    TOKEN = /\A([0-9a-f]{32})\.(\d{1,12})\.([0-9a-f]{32})\z/

    TYPES = {
        'html' => 'text/html; charset=utf-8', 'htm' => 'text/html; charset=utf-8',
        'css' => 'text/css; charset=utf-8', 'js' => 'text/javascript; charset=utf-8',
        'mjs' => 'text/javascript; charset=utf-8', 'json' => 'application/json; charset=utf-8',
        'md' => 'text/plain; charset=utf-8', 'txt' => 'text/plain; charset=utf-8',
        'ini' => 'text/plain; charset=utf-8', 'csv' => 'text/plain; charset=utf-8',
        'svg' => 'image/svg+xml', 'png' => 'image/png', 'jpg' => 'image/jpeg', 'jpeg' => 'image/jpeg',
        'gif' => 'image/gif', 'webp' => 'image/webp', 'ico' => 'image/x-icon',
        'mp3' => 'audio/mpeg', 'ogg' => 'audio/ogg', 'wav' => 'audio/wav',
        'mp4' => 'video/mp4', 'webm' => 'video/webm',
        'woff' => 'font/woff', 'woff2' => 'font/woff2', 'ttf' => 'font/ttf', 'otf' => 'font/otf',
        'pdf' => 'application/pdf', 'wasm' => 'application/wasm',
    }.freeze
    # Source files of the activities, shown as text
    TEXT = %w[html htm css scss js mjs ts tsx jsx json md txt ini csv svg py rb c h cpp hpp java sql er tex bib
        yaml yml toml xml sh lua svelte vue go rs cs pas pp f90 asm bas cob st lisp dart kt php r cfg conf mk
        gitignore].freeze
    # ... and files that are text by their name
    TEXT_NAMES = %w[Makefile Dockerfile LICENSE README Gemfile Rakefile].freeze

    SANDBOX = 'sandbox allow-scripts allow-forms allow-modals allow-popups allow-downloads'

    # Pages that keep something in localStorage (BIF remembers its panel
    # width) or in a cookie would fail with an opaque origin; while they run
    # in the viewer they get stores in memory instead. The file itself stays
    # as it is.
    STORAGE_SHIM = "<script>(function(){function m(){var d={};return{getItem:function(k){return Object.prototype.hasOwnProperty.call(d,k)?d[k]:null},setItem:function(k,v){d[k]=String(v)},removeItem:function(k){delete d[k]},clear:function(){d={}},key:function(i){var k=Object.keys(d);return i<k.length?k[i]:null},get length(){return Object.keys(d).length}}}['localStorage','sessionStorage'].forEach(function(n){try{window[n].length}catch(e){try{Object.defineProperty(window,n,{value:m(),configurable:true})}catch(f){}}});try{document.cookie}catch(e){var c={};try{Object.defineProperty(document,'cookie',{configurable:true,get:function(){return Object.keys(c).map(function(k){return k+'='+c[k]}).join('; ')},set:function(v){var p=String(v).split(';')[0],i=p.indexOf('=');if(i>0)c[p.slice(0,i).trim()]=p.slice(i+1).trim()}})}catch(f){}}})();</script>"

    def self.sign(payload, secret)
        OpenSSL::HMAC.hexdigest('SHA256', secret, payload)[0, 32]
    end

    # Address part for one submission (TaskStore.state_key), valid until expires.
    def self.token(state_key, secret:, now: Time.now.to_i)
        expires = now + LIFETIME
        "#{state_key}.#{expires}.#{sign("#{state_key}.#{expires}", secret)}"
    end

    # The state key, if the token is genuine and not expired.
    def self.verify(token, secret:, now: Time.now.to_i)
        m = TOKEN.match(token.to_s)
        return nil if m.nil? || m[2].to_i < now
        return nil unless OpenSSL.fixed_length_secure_compare(sign("#{m[1]}.#{m[2]}", secret), m[3])

        m[1]
    end

    # A path inside the submission, or nil if it tries to leave it.
    def self.clean_path(path)
        parts = path.to_s.split('/').reject { |p| p.empty? || p == '.' }
        return nil if parts.empty? || parts.any? { |p| p == '..' || p.include?("\0") }

        parts.join('/')
    end

    # [{ path:, size: }] of the files in a stored submission.
    def self.files(archive)
        list = []
        each_entry(archive) { |entry| list << { :path => entry.full_name, :size => entry.header.size } }
        list
    end

    # Bytes of one file (at most max), or nil.
    def self.read(archive, path, max: 20 * 1024 * 1024)
        found = nil
        each_entry(archive) do |entry|
            next unless entry.full_name == path
            return nil if entry.header.size > max

            found = entry.read.to_s
            break
        end
        found
    end

    def self.each_entry(archive)
        File.open(archive, 'rb') do |f|
            Zlib::GzipReader.wrap(f) do |gz|
                Gem::Package::TarReader.new(gz) do |tar|
                    tar.each { |entry| yield entry if entry.file? }
                end
            end
        end
    end

    # A folder asked for as such: its index.html.
    def self.resolve(files, path)
        names = files.map { |f| f[:path] }
        return path if names.include?(path)

        index = path.empty? ? 'index.html' : "#{path}/index.html"
        names.include?(index) ? index : nil
    end

    def self.extension(path)
        File.extname(path.to_s).delete_prefix('.').downcase
    end

    def self.content_type(path)
        TYPES.fetch(extension(path)) { text?(path) ? 'text/plain; charset=utf-8' : 'application/octet-stream' }
    end

    def self.text?(path)
        TEXT.include?(extension(path)) || TEXT_NAMES.include?(File.basename(path.to_s))
    end

    def self.pdf?(path)
        extension(path) == 'pdf'
    end

    def self.html?(path)
        %w[html htm].include?(extension(path))
    end

    # The page as it runs in the viewer: the storage shim first, then the file.
    def self.for_viewer(path, bytes)
        return bytes unless html?(path)

        STORAGE_SHIM.b + bytes.b
    end

    def self.headers(path, frame_ancestors)
        # A PDF only shows outside a sandbox: Chrome refuses to show one in a
        # sandboxed frame. As application/pdf with nosniff it is never taken
        # for a page, its own scripts run in the PDF viewer, and this host
        # has no cookies of the site.
        policy = pdf?(path) ? "frame-ancestors #{frame_ancestors}" : "#{SANDBOX}; frame-ancestors #{frame_ancestors}"
        {
            'Content-Type' => content_type(path),
            'Content-Security-Policy' => policy,
            'Access-Control-Allow-Origin' => '*',
            'X-Content-Type-Options' => 'nosniff',
            'Referrer-Policy' => 'no-referrer',
            'Cache-Control' => 'private, max-age=600',
        }
    end
end
