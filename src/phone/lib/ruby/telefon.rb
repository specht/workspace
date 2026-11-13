# The Telefonspiel in Ruby: say, play, keys, music, hangup, transfer, call, answer.
#
# Every method writes one command as a line of JSON and waits for the
# answer, so a game is an ordinary program that runs from top to bottom.
# puts still works: lines that are not JSON are ordinary output. sleep
# goes through the phone too, as in Python and JavaScript: the music keeps
# playing, keys pressed meanwhile are kept, and the checks need not wait.

require 'json'

$stdout.sync = true

module Telefon
    def self.command(data)
        $stdout.write(JSON.generate(data) + "\n")
        line = $stdin.gets
        exit 0 if line.nil?           # the caller hung up
        answer = JSON.parse(line)
        # a wrong argument (a time that is none): an error at the game's own line
        raise ArgumentError, answer['error'], caller.reject { |l| l.start_with?(__FILE__) } if answer['error']
        answer
    end
end

# the game's name and a line about it: the phone shows the name, and the
# Telefonbuch lists both when the game is published
def title(text)
    Telefon.command('title' => text.to_s)
    nil
end

def subtitle(text)
    Telefon.command('subtitle' => text.to_s)
    nil
end

# speak the text; returns when it has been said. interrupt: true lets a key
# stop it; the key is kept for the next keys. mood: Thorsten in a mood:
# neutral, amused, angry, disgusted, drunk, sleepy, surprised or whisper.
def say(text, interrupt: false, mood: nil)
    command = { 'say' => text.to_s }
    command['interrupt'] = true if interrupt
    command['mood'] = mood.to_s if mood
    Telefon.command(command)
    nil
end

# play a recording of the game's folder (an MP3) in place of the voice;
# returns when it has been heard. interrupt: true lets a key stop it.
def play(file, interrupt: false)
    command = { 'play' => file.to_s }
    command['interrupt'] = true if interrupt
    Telefon.command(command)
    nil
end

# wait for up to count keys (# ends early); "" after timeout seconds
def keys(count = 1, timeout: 10)
    Telefon.command('keys' => count.to_i, 'timeout' => (timeout * 1000).to_i)['keys'].to_s
end

# background music (a YouTube link or a file in the game's folder), looped,
# quieter while speaking; music(nil) stops it. start and stop in seconds
# (75) or as '1:15': the piece from start to stop, looped; a YouTube link
# with t= starts there.
def music(source, volume: 1.0, start: nil, stop: nil)
    Telefon.command('music' => source, 'volume' => volume, 'start' => start, 'stop' => stop)
    nil
end

# wait; the music keeps playing, keys pressed meanwhile are kept
def sleep(seconds)
    Telefon.command('sleep' => (seconds * 1000).to_i)
    seconds.round
end

# end the call
def hangup
    Telefon.command('hangup' => true)
    exit 0
end

# forget the keys pressed so far (also digits dialled after this game's
# number that keys has not read yet)
def clear_keys
    Telefon.command('clear_keys' => true)
    nil
end

# forward the call to the published game with this number (letters as on
# the keys: 'DICE'), extension: digits dialled after it; the call goes on
# there as if the caller had dialled it, and this game ends
def transfer(number, extension = '')
    Telefon.command('transfer' => number.to_s, 'extension' => extension.to_s)
    exit 0
end

# call the game with this number and wait on hold: the caller hears it
# dialled (with the extension after it) and is with that game until it
# answers with answer; returns its digits, "" when it ended without
def call(number, extension = '')
    Telefon.command('call' => number.to_s, 'extension' => extension.to_s)['keys'].to_s
end

# answer the game that called this one: the digits as key tones, then #,
# and this game ends (dialled by a person, the call ends)
def answer(digits = '')
    Telefon.command('answer' => digits.to_s)
    exit 0
end
