Lernende probieren die Bausteine der Kryptographie im Terminal aus, ohne zu programmieren: Hashes mit `sha256sum`, Passwort-Hashes mit Salz (`mkpasswd`), symmetrische Verschlüsselung mit `openssl enc`, asymmetrische mit `openssl pkeyutl` und Unterschriften mit `ssh-keygen -Y`. Die Seite ordnet alles den drei Zielen Unversehrtheit, Geheimhaltung und Echtheit zu und endet bei SSH, Git und HTTPS. Sie setzt TCP/IP voraus, wegen `netcat` im Partnerteil und weil sie an dessen Ende anknüpft (»was unverschlüsselt durchs Netz geht, kann jeder mitlesen«).

<table class='table'>
<tr><th>Ablauf</th><td><ul>
<li><b>1. Doppelstunde:</b> Hashes und ihre Eigenschaften, Prüfsummen, Passwörter speichern (Salz, absichtlich langsame Hashfunktionen), AES mit Passwort.</li>
<li><b>2. Doppelstunde:</b> zwei Schlüssel mit RSA, der Partnerteil über Port 1234, Unterschriften mit Ed25519, Fingerabdrücke, SSH, Git und HTTPS.</li>
</ul></td></tr>
<tr><th>Dateien</th><td>Alles liegt in <code>/workspace/crypto</code>: <code>letter.txt</code>, <code>letter.enc</code>, <code>private.pem</code> und <code>public.pem</code>, <code>secret.txt</code> und <code>secret.enc</code>, <code>key</code> und <code>key.pub</code>, <code>letter.txt.sig</code>, <code>allowed_signers</code>. Die Schlüssel sind nur zum Ausprobieren da; die Seite legt nichts in <code>~/.ssh</code> ab.</td></tr>
</table>

#### Was schiefgeht

- `echo` ohne `-n` hängt einen Zeilenumbruch an, der mitgehasht wird; die Aufgabe »Ein Hash von Hand« erkennt diesen Fehler.
- `openssl enc` zeigt beim Tippen des Passworts nichts an.
- `ssh-keygen -t ed25519 -f key` fragt, ob es überschreiben soll, wenn `key` schon existiert.
- Wer `sed` zweimal ausführt, hat einen anderen Brief als die Bilder; das stört nicht.
- Im Partnerteil muss der Empfänger `nc -l` starten, bevor der Sender schickt, und der Sender braucht `-N`, damit die Verbindung nach der Datei endet.

#### Passwörter

Die Seite zeigt, wie ein Server Passwörter schützt: Salz (zweimal `mkpasswd` mit verschiedenen Ergebnissen) und absichtlich langsame Funktionen (`time` mit `-R 11` gegen `sha256sum`). Wie man erbeutete Hashes errät, zeigt sie nicht. Die Botschaft für die eigene Praxis steht am Ende des Abschnitts: lange, nicht wiederverwendete Passwörter aus einem Passwort-Manager.

#### Die Aufgaben

- »Ein Hash von Hand« erwartet `5576e0e8` (die ersten acht Zeichen des SHA-256 von `Hackschule` ohne Zeilenumbruch).
- »Ein verschlüsselter Brief« prüft, ob `crypto/letter.enc` existiert, »Dein unterschriebener Brief«, ob in `crypto/letter.txt.sig` eine SSH-Unterschrift steht.
- »Eine Ziffer anders«, »Zweimal dasselbe Passwort« und »Nach der Unterschrift geändert« sind Vorhersagen.
- Es gibt keine Abgabe: Der Ordner enthält private Schlüssel, und die Seite sagt, dass man die niemandem gibt.

#### Weiter

Für ein Gespräch eignen sich die Frage am Ende des Partnerteils (woher weiß ich, dass der öffentliche Schlüssel echt ist?) und das Schloss im Browser: Ein Klick darauf zeigt das Zertifikat einer Website, also einen öffentlichen Schlüssel mit Unterschrift.
