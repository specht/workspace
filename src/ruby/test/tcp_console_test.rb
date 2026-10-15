require 'minitest/autorun'
require_relative '../include/tcp_console'

class TcpConsoleTest < Minitest::Test
    def test_hosts
        %w[info.cern.ch localhost example.com. 93.184.216.34 ::1 2001:db8::1 a-b.c].each do |host|
            assert TcpConsole.valid_host?(host), host
        end
        ['', 'a b', '-x.de', 'x-.de', 'a;rm', '$(id)', "a\nb", 'a' * 254, 'host:80', nil].each do |host|
            refute TcpConsole.valid_host?(host), host.inspect
        end
    end

    def test_ports
        assert_equal 80, TcpConsole.port('80')
        assert_equal 65535, TcpConsole.port(65535)
        [nil, '', '0', '65536', '8o', '-1', '1.5'].each { |p| assert_nil TcpConsole.port(p), p.inspect }
    end

    def test_command_runs_the_relay_as_given_user_in_the_workspace
        argv = TcpConsole.command('abc123', 'info.cern.ch', 80, true, :uid => '65534:65534')
        assert_equal ['docker', 'exec', '-i', '-u', '65534:65534'], argv.first(5)
        assert_includes argv, 'hs_code_abc123'
        assert_equal ['info.cern.ch', '80', '1', TcpConsole::IDLE_SECONDS.to_s, TcpConsole::MAX_BYTES.to_s], argv.last(5)
    end

    def test_decoder_keeps_split_characters
        decoder = TcpConsole::Decoder.new
        bytes = 'Grüße €'.b
        first = decoder.call(bytes.byteslice(0, 3))      # G r and half an ü
        rest = decoder.call(bytes.byteslice(3..))
        assert_equal 'Gr', first
        assert_equal 'üße €', rest
        assert_equal "a�b", TcpConsole::Decoder.new.call("a\xFFb".b)
        assert rest.valid_encoding?
    end

    def test_relay_against_a_local_server
        require 'socket'
        server = TCPServer.new('127.0.0.1', 0)
        port = server.addr[1]
        Thread.new do
            client = server.accept
            line = client.gets
            client.write("echo: #{line}")
            client.close
        end
        events = Queue.new
        session = TcpConsole::Session.new(['python3', '-I', '-c', TcpConsole::RELAY, '127.0.0.1', port.to_s, '0', '5', '1000']) { |e| events << e }
        assert_equal '127.0.0.1', events.pop[:connected]
        session.send_line("Hallo\r\nnoch was")
        seen = []
        seen << events.pop until seen.last && seen.last[:ended]
        assert_equal "echo: Hallonoch was\r\n", seen.select { |e| e[:data] }.map { |e| e[:data] }.join
        assert_includes seen, { :closed => 'server' }
    ensure
        server&.close
    end
end
