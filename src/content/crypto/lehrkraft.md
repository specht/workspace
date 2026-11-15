Lernende probieren die Bausteine der Kryptographie im Terminal aus, ohne zu programmieren: Hashes mit `sha256sum`, Passwort-Hashes mit Salz (`mkpasswd`), symmetrische Verschlüsselung mit `openssl enc`, asymmetrische mit `openssl pkeyutl` und Unterschriften mit `ssh-keygen -Y`. Die Seite ordnet alles den drei Zielen Unversehrtheit, Geheimhaltung und Echtheit zu und endet bei SSH, Git und HTTPS. Sie setzt TCP/IP voraus, wegen `netcat` im Partnerteil und weil sie an dessen Ende anknüpft (»was unverschlüsselt durchs Netz geht, kann jeder mitlesen«).

<table class='table'>
<tr><th>Ziele</th><td>Die Lernenden können danach<ul>
<li>die Eigenschaften eines Hashes erklären und mit <code>sha256sum</code> prüfen, ob eine Datei verändert wurde,</li>
<li>begründen, warum Passwörter mit Salz und einer absichtlich langsamen Hashfunktion gespeichert werden,</li>
<li>symmetrische und asymmetrische Verschlüsselung unterscheiden und sagen, wann welche passt,</li>
<li>eine Unterschrift prüfen und erklären, wie Hashes, Schlüssel und Unterschriften hinter SSH, Git und HTTPS zusammenarbeiten.</li>
</ul></td></tr>
<tr><th>Ablauf</th><td><ul>
<li><b>1. Doppelstunde:</b> Hashes und ihre Eigenschaften, Prüfsummen, Passwörter speichern (Salz, absichtlich langsame Hashfunktionen), AES mit Passwort.</li>
<li><b>2. Doppelstunde:</b> zwei Schlüssel mit RSA, der Partnerteil über Port 1234, Unterschriften mit Ed25519, Fingerabdrücke, SSH, Git und HTTPS.</li>
</ul></td></tr>
<tr><th>Ohne Rechner</th><td><ul>
<li><b>Zu Beginn der 2. Doppelstunde, 15 Minuten, die ganze Klasse:</b> ein Federmäppchen mit Reißverschluss (oder eine Kassette mit Öse) und einige kleine gleichschließende Vorhängeschlösser (ein Schlüssel für alle), offen; den Schlüssel hast nur du. Du verteilst die offenen Schlösser. Wer dir etwas schicken will, legt einen Zettel ins Mäppchen, hängt ein Schloss durch die beiden Griffe des Reißverschlusses, drückt es zu und gibt es durch die Klasse nach vorn. Unterwegs darf jede Person es halten; öffnen kannst nur du.</li>
<li><b>Was sichtbar wird:</b> Zum Zuschließen genügt das offene Schloss, zum Öffnen braucht es den Schlüssel. Das Schloss darf jeder haben, der Schlüssel bleibt bei dir, und niemand musste vorher ein Passwort verabreden: Das löst das Problem aus »Ein Brief in eine andere Stadt«.</li>
<li><b>Zurück zur Seite:</b> In <a href='#zwei-schlüssel'>»Zwei Schlüssel«</a> ist <code>public.pem</code> das offene Schloss und <code>private.pem</code> der Schlüssel; <a href='#ein-geheimnis-für-jemand-anderen'>»Ein Geheimnis für jemand anderen«</a> ist dieselbe Runde zu zweit. Am Ende dieses Abschnitts fragt die Seite, woher man weiß, dass ein öffentlicher Schlüssel wirklich vom Partner kommt; im Spiel heißt das: Wer sagt, dass das Schloss deins ist? Die Antwort steht in <a href='#unterschreiben'>»Unterschreiben«</a> und bei den Fingerabdrücken in <a href='#was-hinter-ssh-git-und-https-steckt'>»Was hinter SSH, Git und HTTPS steckt«</a>.</li>
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
