# Nachrichten über eine WebSocket-Verbindung lesen und schicken.
#
# Nach dem Handshake reden Browser und Server nicht mehr in Textzeilen,
# sondern in »Frames«: Jede Nachricht hat einen kleinen Header aus Bytes, der
# sagt, was für eine Nachricht es ist und wie lang sie ist. Nachrichten vom
# Browser sind außerdem mit vier zufälligen Bytes »maskiert« (XOR), damit
# alte Proxys im Netz sie nicht für HTTP halten.

module WebSocket
  # Liest eine Textnachricht. Gibt nil zurück, wenn die Verbindung zu ist.
  def self.read_text(client)
    loop do
      head = client.read(2)
      return nil if head.nil? || head.bytesize < 2
      byte1, byte2 = head.bytes
      opcode = byte1 & 0x0f          # 1: Text, 8: Schluss, 9: Ping
      masked = (byte2 & 0x80) != 0
      length = byte2 & 0x7f
      if length == 126               # Länge steht in den nächsten 2 Bytes
        length = client.read(2).unpack1('n')
      elsif length == 127            # … oder in den nächsten 8 Bytes
        length = client.read(8).unpack1('Q>')
      end
      mask = masked ? client.read(4).bytes : [0, 0, 0, 0]
      data = client.read(length).to_s.bytes
      data = data.each_with_index.map { |b, i| b ^ mask[i % 4] }.pack('C*')

      case opcode
      when 1 then return data.force_encoding('UTF-8')
      when 8 then return nil                      # der Browser verabschiedet sich
      when 9 then write_frame(client, 10, data)   # auf Ping mit Pong antworten
      end
    end
  rescue IOError, SystemCallError
    nil
  end

  # Schickt eine Textnachricht
  def self.write_text(client, text)
    write_frame(client, 1, text.b)
  end

  def self.write_frame(client, opcode, data)
    head = [0x80 | opcode].pack('C')  # 0x80: das ist die ganze Nachricht
    if data.bytesize < 126
      head << [data.bytesize].pack('C')
    elsif data.bytesize < 65536
      head << [126, data.bytesize].pack('Cn')
    else
      head << [127, data.bytesize].pack('CQ>')
    end
    client.write(head + data)
  end
end
