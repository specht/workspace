# Telefonspiel: a game behind a real phone number

Students program a game that answers the phone: someone dials a Berlin
number, a menu offers the published games, and the game talks, plays music
and reacts to the keys of the phone. As of 9 November 2026: the call path
runs end to end in a test network (Asterisk, the speech service, the
runner, a test telephone); the emulator runs in the Workspace (`telefon
spiel.rb`) with the speech service, which every installation has; the
trunk (optional, by the maintainer), publishing and the activity page are
open (»Steps«).

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
- **Game numbers 001 to 999, 000 for a random game** (the maintainer): a
  student chooses a free number when publishing, can switch to another
  free one, and can unpublish (the number is free again). Always exactly
  three digits, also for chance (000, never a single 0), so the menu
  reads three keys without #: »Gib die Nummer des Spiels ein«; keys typed
  during the greeting count. Without Durchwahl the menu reads the titles of
  the published games in random order (the maintainer), so no game is
  always first: five at a time, each with its number (»Die Höhle der
  Echos: eins, zwei, drei«), then »Für weitere Spiele drück die Raute«;
  a number typed at any moment, also in the middle of the list, starts
  that game, and 000 a random one. No selection by the teacher (the
  maintainer): every published game is in the list.
- **The menu's words made in advance** (the maintainer): the digits and the
  menu's own sentences once, each game's title when the game is published,
  so the menu never waits for the speech service during a call.
- **Without a number** (`PHONE_NUMBER` not set) the activity says that a
  real number is being worked on (`<!-- feature: !phone_number -->`).
- **Submitting** (proposed, the maintainer agreed to the Telefonbuch): a
  card »Dein Spiel veröffentlichen« on the activity page, below »Deine
  Geschichte«. The student picks the file in `/workspace/telefon`, gives a
  title and a line of description, and chooses a free number 001 to 999
  (the card shows the taken ones). Submitting copies the game: the file and
  the files it uses (music, required files); the phone plays that copy, so
  later edits reach it only when submitted again. The card lists the
  student's submissions: »wartet auf Freigabe«, »veröffentlicht unter
  123«, »abgelehnt« with the reason; the number can be changed while a
  free one is chosen.
- **Moderation by the maintainer**: each submission sends an email with a
  link into the Workspace (login, admins only): title, number, class and
  name, the code, the sentences in order, the music links, a test run in
  the browser, and the buttons »Veröffentlichen« and »Ablehnen« (with an
  optional reason the student sees).
- **Taking back** at any time, without moderation: the game is off the
  phone at once and its number free again. A new version of a published
  game is moderated again; the old one stays playable until it is approved.
- **The Telefonbuch** (the maintainer): the public list of published games
  at `/telefonbuch`, no login: number, title, the line of description, and
  the phone number (or that one is being worked on). No names of students.
  A button »Telefonbuch« on the public start page and on the activity page.
  The menu on the phone reads the same titles.
- **Durchwahl when the trunk allows it:** Asterisk gets the dialled number,
  and the runner starts the game of its last three digits without the
  menu (»Ruf 030 … 123 an«). This needs a single number with an appended
  extension of three digits or a block of 1000 numbers; easybell's
  Business trunks have SIP-DDI, a 10-number block would only cover the
  teacher's selection. Whether easybell routes a three-digit appended
  extension: to ask easybell or test once the trunk runs.
- **Durchwahl is a setting** (the maintainer): schools have different
  providers, and whether one passes extra digits on cannot be told in
  advance. `credentials.rb` holds the number and a switch, off by default
  (the menu works with every provider): `PHONE_NUMBER` and
  `PHONE_DIRECT_DIAL`. On, the runner starts the game of the dialled
  number's last three digits; a call without them still gets the menu, so
  a wrong setting strands no one. The page and the list of games show the
  number to match: »Ruf 030 … 123 an« or »Ruf 030 … an und gib 123 ein«.
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
  the audio kept as YouTube sends it, and from it the piece the game asks
  for (`start`, `stop`; without them the first ten minutes, never more than
  ten) at 16 kHz, normalised, cached; about 10 s the first time, another
  piece of the same video a fraction of a second. yt-dlp updates itself (PyPI, daily and after a failed download),
  since YouTube refuses old versions. Publishing is to fetch it in advance, so a call meets only finished
  files. A file in the game's folder is read by the runner or the emulator
  itself, never outside that folder.
- **In advance in the Workspace:** `telefon` looks at the game's folder
  every two seconds (.rb, .py, .js, at most 50 files) and has the speech
  service fetch every YouTube link and render every fixed text of `say`
  (no `#{…}`, no f-string) at once, so a call waits for neither; the
  terminal says »Musik wird geholt«, »Musik bereit«, »… Sätze werden
  vorbereitet«, »Sätze bereit«. Texts made while the game runs still come
  at the call. Publishing is to do the same for the real phone.
- **Pieces and loops:** `music` takes `start` and `stop` in seconds or as
  `'1:15'` (also a `t=` in a YouTube link); the phone loops the piece from
  start to stop. `core/game.py` (`music_span`) reads the times for the
  runner and the emulator alike; a wrong one is an error in the game, at its
  line. A new `music` replaces the playing one at once (no crossfade).

## The protocol

A game reads lines from stdin and writes lines to stdout, each a JSON
object, so any language can play:

```text
→ {"say": "Willkommen im Labyrinth."}
→ {"say": "Ein langer Text …", "interrupt": true}   a key stops it and stays for keys
→ {"say": "Psst.", "mood": "whisper"}   Thorsten in a mood (his emotional voice, eight speakers)
→ {"sleep": 1000}
→ {"music": "https://youtu.be/…", "volume": 0.3, "start": "1:15", "stop": 112}
← {"ok": true}                   or {"error": "music: …"} for a time that is none
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
  Audio first: the page shows what is said and pressed, never the music
  (not even that there is any); a music problem goes only to the terminal.
  The conversation is a chat: the game's sentences in bubbles on the left,
  the keys as key caps on the right. The words appear one by one as they
  are spoken, with a dot as the cursor, in the bubble (only there: the
  phone's screen shows the state of the call, keys and the time to press); Piper gives no word timings, so the page takes them from the
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
- `config.rb`: `phone_speech` always, on the students' network,
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
  every piece of music as files (named by hash, without who asked) until
  they are 90 days unplayed; `privacy.rb` says so, and names YouTube.
  Callers' numbers are personal data. Store none, or only
  as long as a call lasts, and write it into `src/ruby/include/privacy.rb`
  before going live (docs/datenschutz.md).
- The Telefonbuch: does it show the class (»aus Klasse 8b«) or nothing
  about who made a game? May a teacher approve the games of their own
  class, or only the maintainer?
- What publishing stores (the copies, titles, statuses, who submitted):
  into `privacy.rb`; deleting an account removes its games.
- Recording one's own sentences, as before: later, after the first
  version runs.
- Interactive fiction (the BIF stories) on the phone: a story read out,
  choices on the keys. A natural connection; kept independent for now.
