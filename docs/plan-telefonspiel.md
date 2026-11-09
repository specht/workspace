# Telefonspiel: a game behind a real phone number

Students program a game that answers the phone: someone dials a Berlin
number, a menu offers the published games, and the game talks, plays music
and reacts to the keys of the phone. As of 9 November 2026: the call path
runs end to end in a test network (Asterisk, the runner, a test telephone);
the trunk, publishing and the Workspace side are open (»Steps«).

## What the old Hackschule had

The old Hackschule had a Telefonspiel ([video](https://www.youtube.com/watch?v=pOFEFaOmCDA);
code in the old repository: `src/ivr/`, `src/tts_helper/`, the task
`08-telefonspiel.txt`), and classes loved it:

- a Python class `Game(AnswerPhone)` with `say(text)`, `sleep(ms)`,
  `dtmf(n)` (up to n keys), `bg_play(url)` (music, quieter under speech)
  and `set_title(name)`;
- tried in the browser first (audio there, keys typed), then published
  with a code; on the phone a menu read out all published games
  (»Drücke die 3 für: …«, 0 for a random one);
- students could record their sentences with their own voice instead of
  the computer voice (»Texte einsprechen«);
- a teacher page listed the published games.

Telephony was sipgate.io: a webhook per call event, answered with XML
(Play a file, then Gather keys). The voice was Coqui TTS (Thorsten), the
music came from YouTube via yt-dlp, mixed with ffmpeg and sox.

Why it was flaky:

- speech synthesis, the YouTube download and the mixing happened during
  the call, inside the webhook's timeout; when they took longer, the call
  dropped;
- `hangup()` ended in `while True: pass`: every finished game kept a
  processor busy until it was killed, and slowed the other calls.

## Decided

- **Route B:** a SIP trunk from a provider and our own phone server
  (Asterisk) as a service beside the Workspace, not webhooks. Speech and
  keys in real time, each simultaneous call one channel, no timeout of
  someone else's server between the caller and the game.
- **Independent of the provider:** Asterisk speaks plain SIP, the standard
  of every SIP trunk, and the runner only knows Asterisk. The trunk is a few
  values in `credentials.rb` (server, user, password, number; switched off
  when they are missing, like other features), no provider's API anywhere.
  Changing the provider means changing these values, not code; the same
  holds for another installation with its own number.
- **Provider to start:** easybell (Berlin; Business 2 to start, free for six months,
  then 1.95 € a month, two calls at once; Business 10 for 4.95 € a month
  with ten). The maintainer pays at first; once it runs, ask easybell (or
  another provider) for sponsoring. Alternative: sipgate trunking, if the
  maintainer's existing sipgate account still has its Berlin number.
- **One number for all games:** a menu or a code chooses the game; several
  calls at once, one process per call.
- **Several languages:** not only Python. Games speak a small protocol
  (below), with a short library per language.
- **Music from YouTube stays** (the maintainer: the line is hardly public,
  an old-fashioned nice thing), but it is fetched and mixed when a game is
  published, never during a call.
- **Develop and test in the Workspace** in VS Code, then publish.
- **Mixed live** (the maintainer): music keeps playing while the game waits
  for keys, a key changes what happens without a gap in the music, and the
  music ducks by itself under speech. So the runner makes each call's
  audio itself, 50 frames a second, instead of asking Asterisk to play
  files one after the other.
- **A good German voice:** Piper with Thorsten to start (the old
  Hackschule's voice, now fast enough to speak during a call); other voices
  to compare later.

## Architecture

```text
caller ── easybell (SIP trunk) ── Asterisk (service "phone")
                                     │ ARI: call events, play, keys
                                     ▼
                              phone runner (Ruby, service)
                                     │ one process per call
                                     ▼
                     the student's game (any language), JSON lines
                                     │
                     speech (Piper) and music, cached as files
```

- **Asterisk** registers at the trunk named in `credentials.rb`, answers,
  and hands every call to the runner through ARI (the Asterisk REST
  Interface: a WebSocket for events such as keys and hang-ups, HTTP for
  commands). Only the trunk and the runner talk to it.
- **The audio of a call** comes from the runner as a stream: an External
  Media channel (RTP to and from the runner) in a bridge with the caller.
  The runner sends G.722 (payload type 9), encoded itself: Asterisk 20
  (20.9 and 20.11 tried) does not take 16 kHz linear audio back from
  External Media, neither over RTP with a dynamic payload type (it drops
  the packets without a word) nor over AudioSocket (it reads them as
  8 kHz). G.722 keeps the 16 kHz; a trunk that offers only G.711 gets it
  translated by Asterisk. G.722's RTP clock runs at 8000 per second, so
  the timestamp grows by 160 per 20 ms frame.
- **The mixer** (`src/phone/runner/mixer.py`): looping music, speech on
  top, the music at a quarter of its volume under speech (down in 80 ms,
  back in 400 ms); keys pressed during a sentence wait for the game's next
  question.
- **The runner** plays the menu, starts the chosen game as a process of
  its own (as an unprivileged user, with limits on time, memory and
  processor, without network), translates the game's lines into ARI
  commands and keys back into lines, and ends the process when the caller
  hangs up. It never waits for slow work during a call: speech comes from
  the cache or from Piper, which needs a fraction of a second for a
  sentence.
- **Speech:** Piper with a German voice. Constant sentences are rendered
  when a game is published (the old server already found them by reading
  the script); sentences with values in them (»Du hast die 3 gedrückt«)
  are rendered during the call and cached.
- **Music:** fetched with yt-dlp and mixed when publishing; during the
  call only finished files.

## The protocol

A game reads lines from stdin and writes lines to stdout, each a JSON
object, so any language can play:

```text
→ {"say": "Willkommen im Labyrinth."}
→ {"sleep": 1000}
→ {"music": "https://youtu.be/…", "volume": 0.3}
→ {"keys": 1}                    ask for up to 1 key
← {"keys": "4"}                  the caller pressed 4 ("" after a timeout)
→ {"hangup": true}
```

The library per language turns this into `say("…")`, `keys(1)` and so on
(Python, Ruby, JavaScript first; Java, C#, Dart, Go later, as the
Workspace's other languages). The game ends when its program ends; no busy
loop.

## Developing and testing in the Workspace

- A simulator in the Workspace image runs the game exactly as the runner
  would: the spoken text in the terminal and as audio in the browser
  preview, the keys from a keypad there or from the keyboard.
- For the real thing in class: a softphone (in the browser, or Linphone on
  a phone) dials into the test extension of the Asterisk, without the
  public number.
- Publishing from the activity page: the game gets a code, a teacher sees
  it on a page and can take it offline.

## Where it is

- `docker/phone-asterisk`, `docker/phone-runner` (Piper and the Thorsten
  voice inside), `docker/phone-testclient` (baresip as a test telephone);
- `src/phone/asterisk/` (configuration: a test endpoint, the echo
  extension 600, 100 into the runner), `src/phone/runner/` (runner, mixer,
  speech and music cache, the Python library in `lib/python/telefon.py`),
  `src/phone/games/1/` (Zahlenraten, a demo game);
- tests: `src/phone/test/test_mixer.py` (ducking, speech, G.722 packets)
  and `src/phone/test/e2e.sh` (a whole call: menu, key 1, three guesses,
  the runner's log checked, the recording kept). The test telephone sends
  keys as RFC 4733 events and needs a key release after each key (0x04 on
  baresip's UDP console), or each key arrives only with the next one.

## Steps

1. The maintainer books a trunk (easybell Business 2 to start) and puts the
   SIP credentials and the number into `credentials.rb`. Check when ordering that a Berlin number is
   included or what it costs.
2. Done in the test network: Asterisk with a test endpoint and an echo
   extension. Open: Asterisk as a service in `config.rb`, reachable in
   development only from the local network, the trunk from
   `credentials.rb`.
3. Done in the test network: the runner with live mixing, the protocol,
   the menu, a demo game; Piper for speech. Open: a fixed number of speech
   workers (ten calls at once), rendering a game's fixed sentences when it
   is published.
4. The Python library and the simulator in the Workspace; an activity page
   »Telefonspiel« (Ruby as the default language of new activities, Python
   and the others through the library).
5. Publishing, codes, the teacher page, limits per call.
6. Connect the trunk on the server, open the ports it needs, test calls
   from outside, several at once.
7. Live; then ask for sponsoring.

## Open

- Does easybell's Business 2 include a Berlin number, and which ports does
  the trunk need on the server (SIP and the range for the audio)?
- The licence of the German Piper voice, for the page's credits.
- What is stored: callers' numbers are personal data. Store none, or only
  as long as a call lasts, and write it into `src/ruby/include/privacy.rb`
  before going live (docs/datenschutz.md).
- Recording one's own sentences, as before: later, after the first
  version runs.
