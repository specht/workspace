require 'base64'
require 'json'
require 'net/http'

# The backend's side of the office service (src/office/server.py,
# docs/plan-textverarbeitung.md): sends a document there and gets back what
# LibreOffice read in it and its pages as pictures. The service runs on a
# network of its own with the backend only; OFFICE_URL names it
# (http://office:8000 in the containers, set to something else for a test).
module OfficeClient
    URL = ENV['OFFICE_URL'] || 'http://office:8000'
    MAX_BYTES = 10 * 1024 * 1024
    # pdf: a document exported as PDF (a check with accept: pdf), read by
    # poppler, not LibreOffice
    FORMATS = %w[docx odt doc rtf pdf xlsx ods xls].freeze
    # what the service answers when it cannot read a document, for the card
    ERRORS = {
        'format' => 'Diese Datei kann ich nicht lesen. Speichere ein Textdokument als .odt oder .docx, eine Tabelle als .ods oder .xlsx.',
        'not_text' => 'Das ist kein Textdokument. Speichere es als .odt (LibreOffice) oder .docx (Word).',
        'timeout' => 'Das Lesen hat zu lange gedauert. Ist die Datei sehr groß? Versuch es noch einmal.',
        'failed' => 'Diese Datei konnte ich nicht lesen. Öffne sie noch einmal, speichere sie als .docx und lade sie neu hoch.',
        'too_big' => 'Die Datei ist größer als 10 MB. Sind große Bilder darin?',
        'empty' => 'Die Datei ist leer.',
        'pdf' => 'Diese PDF-Datei konnte ich nicht lesen. Exportiere sie noch einmal aus deinem Programm.',
    }.freeze

    class Unavailable < StandardError; end

    # bytes and the file's name (for its format) to the service; returns the
    # parsed answer, a Hash with 'paragraphs', 'pages' …, or with 'error'.
    # Raises Unavailable when the service cannot be reached.
    # probes: for a spreadsheet, cells to change and read after recalculating
    # (DocumentCheck.probes), sent along as base64 JSON
    def self.analyze(bytes, filename, probes: nil)
        ext = File.extname(filename.to_s).delete('.').downcase
        return { 'error' => 'format' } unless FORMATS.include?(ext)
        return { 'error' => 'too_big' } if bytes.bytesize > MAX_BYTES
        return { 'error' => 'empty' } if bytes.empty?

        uri = URI.join(URL, '/analyze')
        http = Net::HTTP.new(uri.host, uri.port)
        http.open_timeout = 5
        http.read_timeout = 90
        request = Net::HTTP::Post.new(uri)
        request['X-Filename'] = "upload.#{ext}"
        request['X-Probes'] = Base64.strict_encode64(JSON.generate(probes)) if probes && !probes.empty?
        request['Content-Type'] = 'application/octet-stream'
        request.body = bytes
        response = http.request(request)
        raise Unavailable, "office service: HTTP #{response.code}" unless %w[200 413 400].include?(response.code)
        JSON.parse(response.body)
    rescue SystemCallError, SocketError, Net::OpenTimeout, Net::ReadTimeout, IOError => e
        raise Unavailable, "office service: #{e.class}: #{e.message}"
    end

    @cache = {}
    @cache_lock = Mutex.new

    # A file of the site (a task's starting document), read once per
    # version of the file.
    def self.analyze_file(path)
        key = [path, File.mtime(path).to_i, File.size(path)]
        @cache_lock.synchronize { return @cache[key] if @cache.key?(key) }
        result = analyze(File.binread(path), File.basename(path))
        result = result.reject { |k, _| k == 'pages' }
        @cache_lock.synchronize { @cache[key] = result }
        result
    end
end
