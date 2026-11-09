# Telefonspiel: a game behind a real phone number

Students program a game that answers the phone: someone dials a Berlin
number, a menu offers the published games, and the game talks, plays music
and reacts to the keys of the phone. As of 9 November 2026: the call path
runs end to end in a test network (Asterisk, the speech service, the
runner, a test telephone); the emulator runs in the Workspace (`telefon
spiel.rb`) with the speech service behind `PHONE = true`; the trunk,
publishing and the activity page are open (»Steps«).

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
                              phone runner (Python, service) ── speech service
                                     │ one process per call         (Piper, YouTube,
                                     ▼                               cached as files)
                     the student's game (any language), JSON lines        │
                                                                          │
Workspace: telefon spiel.rb ── the same game code (core/game.py) ─────────┘
           page in the browser: keypad, speech and music mixed there
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
- **The speech service** (`src/phone/speech/server.py`, its own container
  `phone_speech`): Piper with the voice Thorsten in a fixed number of worker
  processes (`PHONE_SPEECH_WORKERS`, 2) for every call and every emulator,
  so ten calls and a class trying their games share the processor instead
  of fighting for it. Each sentence is cached as a file (SHA-1 of voice and
  text); the same sentence is made once even when several ask at once;
  more than 100 waiting requests get 503. Workers load the voice at start;
  a new sentence takes well under a second, a cached one 20 ms.
  Constant sentences are to be rendered when a game is published (the old
  server already found them by reading the script).
- **Music:** a YouTube link goes to the speech service (it fetches nothing
  else, so no one can make it fetch an address inside the network): yt-dlp,
  the first ten minutes at 16 kHz, normalised, cached; about 10 s the first
  time. Publishing is to fetch it in advance, so a call meets only finished
  files. A file in the game's folder is read by the runner or the emulator
  itself, never outside that folder.

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

- **The emulator** (`src/phone/emulator/telefon`, in the Workspace image as
  `telefon`, only the standard library): `telefon spiel.rb` (or `.py`,
  `.js`, or without a name the command from `game.json`) runs the game with
  the runner's own code (`src/phone/core/game.py`) and serves a page on
  port 8040 (the next free one up to 8059), linked through code-server's
  proxy. The page has a keypad (mouse or keyboard, with key tones), a green
  receiver that starts the game and a red one that hangs up. The browser
  mixes as the phone does (the same constants as `mixer.py`, Web Audio).
  The conversation is a chat: the game's sentences in bubbles on the left,
  the keys as key caps on the right. The words appear one by one as they
  are spoken, with a dot as the cursor, in the bubble and on the phone's
  screen; Piper gives no word timings, so the page takes them from the
  sound (where speech starts and ends, its pauses put at the commas, the
  words in between by length), on the audio clock. The terminal shows each
  sentence when it starts to play; the game's own output and errors stay
  there. Without the speech service
  the browser's own voice reads, and the page says so. Closing or reloading
  the page hangs up; a second tab takes over the phone.
- For the real thing in class: a softphone (in the browser, or Linphone on
  a phone) dials into the test extension of the Asterisk, without the
  public number.
- Publishing from the activity page: the game gets a code, a teacher sees
  it on a page and can take it offline.

## Where it is

- `docker/phone-asterisk`, `docker/phone-speech` (Piper and the Thorsten
  voice, yt-dlp), `docker/phone-runner` (Python, Ruby, Node for the games),
  `docker/phone-testclient` (baresip as a test telephone);
- `src/phone/asterisk/` (configuration: a test endpoint, the echo
  extension 600, 100 into the runner), `src/phone/core/game.py` (running a
  game and its protocol, shared by runner and emulator), `src/phone/runner/`
  (runner, mixer, sounds), `src/phone/speech/`, `src/phone/emulator/`
  (`telefon` and its page), `src/phone/lib/` (the libraries for Python,
  Ruby, JavaScript), `src/phone/games/1/` (Zahlenraten, a demo game with
  its own music file);
- `config.rb`: `phone_speech` with `PHONE = true`, on the students' network,
  reachable from Workspaces on port 8000 only (peer-firewall label
  `…peer_firewall.phone`); Asterisk and the runner join with the trunk;
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
   the menu, a demo game; the speech service with a fixed number of
   workers. Open: rendering a game's fixed sentences and fetching its music
   when it is published; the game's process without network.
4. Done: the libraries for Python, Ruby and JavaScript and the emulator in
   the Workspace. Open: an activity page »Telefonspiel« (Ruby as the
   default language of new activities, Python and JavaScript through the
   libraries).
5. Publishing, codes, the teacher page, limits per call.
6. Connect the trunk on the server, open the ports it needs, test calls
   from outside, several at once.
7. Live; then ask for sponsoring.

## Open

- Does easybell's Business 2 include a Berlin number, and which ports does
  the trunk need on the server (SIP and the range for the audio)?
- The licence of the German Piper voice, for the page's credits.
- What is stored: the speech service keeps every sentence a game said and
  every piece of music as files (named by hash, without who asked), and its
  log names the YouTube links; say so in `privacy.rb` before `PHONE` is
  switched on anywhere. Callers' numbers are personal data. Store none, or only
  as long as a call lasts, and write it into `src/ruby/include/privacy.rb`
  before going live (docs/datenschutz.md).
- Recording one's own sentences, as before: later, after the first
  version runs.
