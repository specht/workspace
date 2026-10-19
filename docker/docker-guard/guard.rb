# The Docker guard: the only process with the real Docker socket. The
# backend talks to it as if it were Docker (DOCKER_HOST points at its
# socket); every request is checked with DockerPolicy
# (src/ruby/include/docker_policy.rb) and either passed on or refused with a
# 403. Standard library only.
#
# One request per connection: the guard reads the request, decides, and
# forwards it with »Connection: close«, so that Docker closes after its
# answer; the CLI opens a new connection for the next one. An upgraded
# connection (attach, exec with stdin) is relayed both ways until it ends.
require 'socket'
require 'json'
require 'uri'
require_relative '/policy/docker_policy'

DOCKER_SOCKET = ENV['GUARD_DOCKER_SOCKET'] || '/var/run/docker.sock'
LISTEN = ENV['GUARD_LISTEN'] || '/run/docker-guard/docker.sock'
CONFIG = DockerPolicy.config_from_env
$stdout.sync = true

def read_head(io)
    head = +''
    until head.end_with?("\r\n\r\n")
        chunk = io.read(1)
        return nil if chunk.nil?

        head << chunk
        return nil if head.size > 64 * 1024
    end
    lines = head.split("\r\n")
    method, target, version = lines.shift.to_s.split(' ', 3)
    headers = lines.map { |l| l.split(':', 2).map(&:strip) }.reject { |k, _| k.to_s.empty? }
    [method, target, version, headers]
end

def header(headers, name)
    (headers.find { |k, _| k.casecmp?(name) } || [])[1]
end

def read_body(io, headers)
    if (length = header(headers, 'Content-Length'))
        return io.read(length.to_i).to_s
    end
    return '' unless header(headers, 'Transfer-Encoding').to_s.downcase.include?('chunked')

    body = +''
    loop do
        size = io.gets("\r\n").to_s.strip.to_i(16)
        if size.zero?
            io.gets("\r\n")
            break
        end
        body << io.read(size).to_s
        io.read(2)
    end
    body
end

# The container's name, from Docker itself (an ID or a name may come in).
def container_name(id)
    sock = UNIXSocket.new(DOCKER_SOCKET)
    sock.write("GET /containers/#{URI.encode_www_form_component(id)}/json HTTP/1.1\r\nHost: docker\r\nConnection: close\r\n\r\n")
    response = sock.read
    sock.close
    status = response[/\AHTTP\/1\.\d (\d+)/, 1].to_i
    return nil unless status == 200

    body = response.split("\r\n\r\n", 2)[1].to_s
    body = dechunk(body) if response.split("\r\n\r\n", 2)[0].downcase.include?('transfer-encoding: chunked')
    JSON.parse(body)['Name'].to_s.delete_prefix('/')
rescue StandardError
    nil
end

def dechunk(text)
    out = +''
    rest = text
    loop do
        line, rest = rest.split("\r\n", 2)
        size = line.to_s.strip.to_i(16)
        break if size.zero? || rest.nil?

        out << rest[0, size]
        rest = rest[(size + 2)..].to_s
    end
    out
end

def refuse(client, reason)
    body = JSON.generate({ 'message' => "docker-guard: #{reason}" })
    client.write("HTTP/1.1 403 Forbidden\r\nContent-Type: application/json\r\nContent-Length: #{body.bytesize}\r\nConnection: close\r\n\r\n#{body}")
end

def relay(from, to)
    IO.copy_stream(from, to)
rescue StandardError
    nil
ensure
    to.close_write rescue nil
end

def handle(client)
    method, target, version, headers = read_head(client)
    return if method.nil?

    body = read_body(client, headers)
    path, query = target.to_s.split('?', 2)
    kind, id = DockerPolicy.classify(method, path)
    name = id ? container_name(id) : nil
    allowed, reason = DockerPolicy.decide(method, path, query.to_s, body, CONFIG, :name => name)
    unless allowed
        puts "refused: #{method} #{DockerPolicy.plain_path(path)}#{name ? " (#{name})" : ''}: #{reason}"
        return refuse(client, reason)
    end

    upgrade = header(headers, 'Connection').to_s.downcase.include?('upgrade')
    upstream = UNIXSocket.new(DOCKER_SOCKET)
    kept = headers.reject { |k, _| %w[connection content-length transfer-encoding].include?(k.downcase) }
    kept << ['Connection', upgrade ? header(headers, 'Connection') : 'close']
    kept << ['Content-Length', body.bytesize.to_s] unless body.empty? && %w[GET HEAD].include?(method)
    upstream.write("#{method} #{target} #{version}\r\n" + kept.map { |k, v| "#{k}: #{v}\r\n" }.join + "\r\n" + body)
    # Both directions: the answer back, and for an upgraded connection the
    # client's input onward (exec -i, attach).
    back = Thread.new { relay(upstream, client) }
    forward = Thread.new { relay(client, upstream) } if upgrade
    back.join
    forward&.kill
    upstream.close rescue nil
rescue StandardError => e
    puts "error: #{e.class}: #{e.message}"
ensure
    client.close rescue nil
end

File.unlink(LISTEN) if File.exist?(LISTEN)
server = UNIXServer.new(LISTEN)
File.chmod(0o660, LISTEN)
puts "docker-guard listening on #{LISTEN} (images #{CONFIG.images.join(', ')}, folders under #{CONFIG.bind_prefix})"
loop do
    client = server.accept
    Thread.new(client) { |c| handle(c) }
end
