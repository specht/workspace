<div class='meta'>
kind: reference
search: Hash, SHA-256, sha256sum, Salz, yescrypt, mkpasswd, AES, openssl, RSA, öffentlicher Schlüssel, privater Schlüssel, Signatur, ssh-keygen, Fingerabdruck, HMAC, Base64, Base32, Zufall
</div>

<div
    class="autotoc-secondary-trigger"
    data-title="Auf dieser Seite"
    data-levels="h2,h3">
</div>

# Kryptographie im Terminal

<p class='abstract'>
Die Bausteine aus <a href='/crypto'>Kryptographie im Terminal</a> und <a href='/totp'>Zwei Faktoren</a> zum Nachschlagen: was ein Hash, ein Schlüssel und eine Unterschrift sind, welcher Befehl was tut, und wie dasselbe in Ruby und Python aussieht. Dazu, welcher Baustein für welche Aufgabe der richtige ist.
</p>

## Welcher Baustein wofür?

<!-- part: krypto-ueberblick -->
<!-- short: Hash für Fingerabdrücke, Passwort-Hash mit Salz für Passwörter, AES mit einem Schlüssel, RSA mit zwei, Signatur mit dem privaten Schlüssel, HMAC für Prüfsummen mit Schlüssel. -->
- **Prüfen, ob eine Datei unverändert ist:** ein Hash (SHA-256), im Terminal `sha256sum datei`.
- **Passwörter speichern:** ein Passwort-Hash mit Salz, absichtlich langsam (yescrypt, bcrypt, Argon2), im Terminal `mkpasswd -m yescrypt`.
- **Etwas mit einem Passwort verschlüsseln:** symmetrisch mit AES, ein Schlüssel für beide Richtungen, im Terminal `openssl enc -aes-256-cbc -pbkdf2`.
- **Jemandem etwas schicken, das nur er lesen kann:** asymmetrisch mit RSA und seinem öffentlichen Schlüssel, im Terminal `openssl pkeyutl -encrypt`.
- **Zeigen, dass etwas von dir kommt und unverändert ist:** eine Signatur mit deinem privaten Schlüssel (Ed25519), im Terminal `ssh-keygen -Y sign`.
- **Eine Prüfsumme, die nur berechnen kann, wer den Schlüssel hat:** HMAC; der Code einer Authenticator-App kommt daher. Wie das geht, steht unter [In Programmen](#krypto-programme).

Ein Passwort wird nie verschlüsselt gespeichert, sondern gehasht: Was sich entschlüsseln lässt, kann jemand entschlüsseln, der an die Datenbank und den Schlüssel kommt.
<!-- /part -->

## Hashes

<!-- part: krypto-hash -->
<!-- short: sha256sum: aus beliebig vielen Bytes 64 Hexziffern; gleiche Daten, gleicher Hash; zurückrechnen geht nicht. -->
<div class='scroll-x'><table class='table table-sm reference-table'>
<thead><tr><th>Befehl</th><th>Was er tut</th></tr></thead>
<tbody>
<tr><td><code>sha256sum datei</code></td><td>den SHA-256-Hash einer Datei: 64 Hexziffern, 256 Bit</td></tr>
<tr><td><code>echo -n text | sha256sum</code></td><td>den Hash eines Texts; <code>-n</code> lässt das Zeilenende weg, das <code>echo</code> sonst anhängt und das den Hash ändert</td></tr>
<tr><td><code>sha256sum *.txt &gt; summen.txt</code></td><td>die Hashes aller Textdateien speichern</td></tr>
<tr><td><code>sha256sum -c summen.txt</code></td><td>später prüfen, ob die Dateien noch dieselben sind</td></tr>
</tbody>
</table>
</div>

Ein Hash ist ein **Fingerabdruck**: Gleiche Daten geben immer denselben Hash, ein einziges geändertes Zeichen einen völlig anderen. Aus dem Hash lassen sich die Daten nicht zurückrechnen. MD5 und SHA-1 gelten als gebrochen: Man kann zwei Dateien mit demselben Hash bauen. Für Neues nimmst du SHA-256.
<!-- /part -->

<!-- part: krypto-passwort -->
<!-- short: Ein Passwort-Hash hat ein zufälliges Salz und ist absichtlich langsam; mkpasswd -m yescrypt. -->
### Passwörter

```bash
mkpasswd -m yescrypt geheim
```

```text
$y$j9T$mL3J0fX…$k8bH…
```

`$y$` sagt, welches Verfahren es ist, dahinter stehen die Einstellungen, das **Salz** und der Hash. Das Salz ist bei jedem Aufruf anders, deshalb gibt dasselbe Passwort jedes Mal einen anderen Hash, und eine vorbereitete Tabelle von Hashes häufiger Passwörter nützt einem Angreifer nichts. Ein Passwort-Hash ist außerdem absichtlich langsam (`-R` stellt ein, wie langsam): Einmal anmelden merkt man nicht, Milliarden Passwörter durchprobieren dauert dagegen Jahre. Ein schneller Hash wie SHA-256 allein ist für Passwörter deshalb falsch.
<!-- /part -->

## Verschlüsseln

<!-- part: krypto-aes -->
<!-- short: openssl enc -aes-256-cbc -pbkdf2 -in a -out b verschlüsselt mit einem Passwort, -d entschlüsselt. -->
### Mit einem Schlüssel: AES

```bash
# verschlüsseln, fragt nach einem Passwort
openssl enc -aes-256-cbc -pbkdf2 -in letter.txt -out letter.enc
# die Bytes ansehen: nichts mehr zu erkennen
xxd letter.enc | head
# zurück, mit demselben Passwort
openssl enc -d -aes-256-cbc -pbkdf2 -in letter.enc
```

`-pbkdf2` macht aus dem Passwort einen Schlüssel, langsam und mit Salz, wie bei Passwort-Hashes. Wer entschlüsseln will, braucht dasselbe Passwort und dieselben Angaben. Mit `-a` kommt das Ergebnis als Text in Base64 heraus, das man in eine Nachricht kopieren kann; beim Entschlüsseln dann ebenfalls `-a`.
<!-- /part -->

<!-- part: krypto-rsa -->
<!-- short: openssl genpkey erzeugt ein Schlüsselpaar; mit dem öffentlichen verschlüsseln, mit dem privaten entschlüsseln. -->
### Mit zwei Schlüsseln: RSA

```bash
# ein privater Schlüssel; er bleibt bei dir
openssl genpkey -algorithm RSA -out private.pem
# der öffentliche Schlüssel dazu; ihn darf jeder haben
openssl pkey -in private.pem -pubout -out public.pem
# verschlüsseln, mit dem öffentlichen Schlüssel des Empfängers
openssl pkeyutl -encrypt -pubin -inkey public.pem -in a.txt -out a.enc
# entschlüsseln: Das kann nur der private Schlüssel
openssl pkeyutl -decrypt -inkey private.pem -in a.enc
```

RSA verschlüsselt nur kurze Daten, bei einem Schlüssel mit 2048 Bit höchstens gut 200 Bytes. Für mehr schickt man mit RSA einen zufälligen AES-Schlüssel und verschlüsselt den Rest mit AES; so macht es auch HTTPS.
<!-- /part -->

## Unterschreiben

<!-- part: krypto-signatur -->
<!-- short: ssh-keygen -t ed25519 erzeugt ein Paar; -Y sign unterschreibt mit dem privaten, -Y verify prüft mit dem öffentlichen Schlüssel; -lf zeigt den Fingerabdruck. -->
```bash
# ein Schlüsselpaar: key privat, key.pub öffentlich,
# ohne Passwort (-N ""), mit dem Namen alice als Kommentar
ssh-keygen -t ed25519 -f key -N "" -C "alice"
# unterschreiben; die Unterschrift steht in letter.txt.sig
ssh-keygen -Y sign -f key -n file letter.txt
# eine Liste, wem welcher öffentliche Schlüssel gehört
echo "alice $(cat key.pub)" > allowed_signers
# prüfen: Good "file" signature for alice, oder ein Fehler,
# wenn sich der Brief geändert hat
ssh-keygen -Y verify -f allowed_signers -I alice -n file -s letter.txt.sig < letter.txt
# der Fingerabdruck des Schlüssels, SHA256:…, kurz genug zum Vergleichen
ssh-keygen -lf key.pub
```

Eine Unterschrift beweist zweierlei: Der Text ist unverändert, und er kommt von dem, der den privaten Schlüssel hat. Ob ein öffentlicher Schlüssel wirklich zu der Person gehört, sagt sie nicht: Das prüft man auf einem anderen Weg, etwa indem man den Fingerabdruck vorliest.
<!-- /part -->

## Bytes als Text

<!-- part: krypto-kodierung -->
<!-- short: Hexadezimal, Base64 und Base32 schreiben Bytes als Text; das ist keine Verschlüsselung. -->
<div class='scroll-x'><table class='table table-sm reference-table reference-table-wide'>
<thead><tr><th>Schreibweise</th><th>Zeichen</th><th>Im Terminal</th></tr></thead>
<tbody>
<tr><th>hexadezimal</th><td><code>0</code>–<code>9</code>, <code>a</code>–<code>f</code>; zwei Zeichen je Byte</td><td><code>xxd datei</code></td></tr>
<tr><th>Base64</th><td><code>A</code>–<code>Z</code>, <code>a</code>–<code>z</code>, <code>0</code>–<code>9</code>, <code>+</code>, <code>/</code>; vier Zeichen je drei Bytes, <code>=</code> füllt auf</td><td><code>base64 datei</code>, zurück mit <code>base64 -d</code></td></tr>
<tr><th>Base32</th><td><code>A</code>–<code>Z</code>, <code>2</code>–<code>7</code>; je Zeichen 5 Bit, gut abzutippen</td><td><code>base32 datei</code>, zurück mit <code>base32 -d</code></td></tr>
</tbody>
</table>
</div>

Alle drei lassen sich ohne Schlüssel zurückverwandeln. Sie machen Bytes nur lesbar und kopierbar, geheim machen sie nichts.
<!-- /part -->

## In Programmen

<!-- part: krypto-programme -->
<!-- short: SHA-256, HMAC, Base64 und Zufall in Ruby und Python. -->
In Python sind `data` und `key` Bytes, kein Text: `"geheim".encode()` macht aus einem Text Bytes.

### SHA-256

Der Hash von Bytes, als 64 Hexziffern.

<!-- language: ruby -->
```ruby
require 'digest'
Digest::SHA256.hexdigest(data)
```
<!-- /language -->
<!-- language: python -->
```python
import hashlib
hashlib.sha256(data).hexdigest()
```
<!-- /language -->

### HMAC

Eine Prüfsumme mit Schlüssel, wie sie eine Authenticator-App rechnet, hier mit SHA-1. Heraus kommen Bytes.

<!-- language: ruby -->
```ruby
require 'openssl'
OpenSSL::HMAC.digest('SHA1', key, data)
```
<!-- /language -->
<!-- language: python -->
```python
import hmac, hashlib
hmac.new(key, data, hashlib.sha1).digest()
```
<!-- /language -->

### Base64

Bytes als Text und zurück.

<!-- language: ruby -->
```ruby
text = [data].pack('m0')
data = text.unpack1('m0')
```
<!-- /language -->
<!-- language: python -->
```python
import base64
text = base64.b64encode(data)
data = base64.b64decode(text)
```
<!-- /language -->

### Zufällige Bytes

Für Schlüssel und Salz, hier 20 Bytes. Nimm dafür nie `rand` oder `random`: Deren Zahlen lassen sich vorhersagen.

<!-- language: ruby -->
```ruby
require 'securerandom'
SecureRandom.random_bytes(20)
```
<!-- /language -->
<!-- language: python -->
```python
import secrets
secrets.token_bytes(20)
```
<!-- /language -->
<!-- /part -->

## Zum Nachschlagen

- [OpenSSL: Manual Pages](https://docs.openssl.org/master/man1/): `openssl enc`, `genpkey`, `pkeyutl` und alle anderen Befehle mit allen Optionen, auf Englisch.
- [Python: `hashlib`](https://docs.python.org/3/library/hashlib.html), [`hmac`](https://docs.python.org/3/library/hmac.html) und [`secrets`](https://docs.python.org/3/library/secrets.html).
- [Ruby: `OpenSSL`](https://docs.ruby-lang.org/en/3.4/OpenSSL.html): Hashes, Verschlüsselung und Schlüssel; HMAC auf einer eigenen Seite, [`OpenSSL::HMAC`](https://docs.ruby-lang.org/en/3.4/OpenSSL/HMAC.html).
