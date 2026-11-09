# The Telefonspiel in Ruby: say, keys, music, hangup.
#
# Every method writes one command as a line of JSON and waits for the
# answer, so a game is an ordinary program that runs from top to bottom.
# puts still works: lines that are not JSON are ordinary output. Ruby's
# own sleep works as well: the music keeps playing, keys pressed meanwhile
# are kept.

require 'json'

$stdout.sync = true

module Telefon
    def self.command(data)
        $stdout.write(JSON.generate(data) + "\n")
        line = $stdin.gets
        exit 0 if line.nil?           # the caller hung up
        JSON.parse(line)
    end
end

# speak the text; returns when it has been said
def say(text)
    Telefon.command('say' => text.to_s)
    nil
end

# wait for up to count keys (# ends early); "" after timeout seconds
def keys(count = 1, timeout: 10)
    Telefon.command('keys' => count.to_i, 'timeout' => (timeout * 1000).to_i)['keys'].to_s
end

# background music (a YouTube link or a file in the game's folder), looped,
# quieter while speaking; music(nil) stops it
def music(source, volume: 1.0)
    Telefon.command('music' => source, 'volume' => volume)
    nil
end

# end the call
def hangup
    Telefon.command('hangup' => true)
    exit 0
end
