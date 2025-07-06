# Deine eigene Subdomain

Um deine eigene Subdomain zu bekommen, musst du zuerst ein SSH-Schlüsselpaar erstellen (falls du noch keins hast).

## Schlüsselpaar erstellen

Überprüfe, ob du bereits ein Schlüsselpaar hast:

```bash
ls ~/.ssh/id_rsa.pub
```
Falls nicht, erstelle ein neues Schlüsselpaar:

```bash
ssh-keygen
```

Bestätige den Speicherort, eine Passphrase ist nicht nötig. Das Schlüsselpaar wird in `~/.ssh/id_rsa` und `~/.ssh/id_rsa.pub` gespeichert.

<div class="hint">
Achtung: Dein Schlüsselpaar besteht aus zwei Dateien: <code>id_rsa</code> (privat) und <code>id_rsa.pub</code> (öffentlich). Teile niemals deine private Datei!
Die öffentliche Datei kannst du jedoch mit anderen teilen, um ihnen Zugriff auf deine Ressourcen zu gewähren.
</div>

Lasse dir den öffentlichen Schlüssel anzeigen:

```bash
cat ~/.ssh/id_rsa.pub
```

Das sollte z. B. so aussehen:

```
ssh-rsa AAAAB3NzaC1yc2EAAAADAQABA... abc@0ea48f711732
```

Schicke den kompletten öffentlichen Schlüssel an <a href='mailto:specht@gymnasiumsteglitz.de'>specht@gymnasiumsteglitz.de</a> und gib an, welche Subdomain du haben möchtest. Du bekommst als Antwort eine E-Mail, in der zwei Dinge stehen:

- die Subdomain, die du bekommen hast (z. B. `cobolultras.hackschule.de`)
- dein Nutzername für die Subdomain (z. B. `max`)

## Subdomain einrichten

Wenn deine Subdomain eingerichtet ist, kannst du sie in deinem Browser aufrufen. Du wirst aber noch nichts sehen, da du noch keine Dateien hochgeladen hast.

Bevor du Dateien hochladen kannst, musst du deine SSH-Konfiguration anpassen. Erstelle die Datei `/workspace/.ssh/config`:

```bash
touch /workspace/.ssh/config
```

…und öffne sie in einem Texteditor und füge Folgendes hinzu:

```bash
Host cobolultras.hackschule.de
  User max
  IdentityFile ~/.ssh/id_rsa
  Port 2222
```

<div class="hint">
Achte darauf, dass du den Hostnamen und den Benutzernamen durch die Werte ersetzt, die du in der E-Mail erhalten hast.
Wenn du mehrere Subdomains hast, kannst du für jede Subdomain einen eigenen Block hinzufügen. Achte darauf, dass der Hostname und der Benutzername jeweils korrekt sind.
</div>

Versuche nun, dich mit dem Server zu verbinden:

```bash
ssh cobolultras.hackschule.de
```

Wenn du dich zum ersten Mal mit dem Server verbindest, wirst du gefragt, ob du dem Host vertraust. Gib `yes` ein und drücke Enter.

```bash
abc@0ea48f711732:~/website-test$ ssh cobolultras.hackschule.de
The authenticity of host '[cobolultras.hackschule.de]:2222 ([138.199.166.247]:2222)' cannot be established.
ED25519 key fingerprint is SHA256:G71MgKyMMzC8lD3LwVsct3eXTJEDzjj4MetU43NcfQs.
This key is not known by any other names
Are you sure you want to continue connecting (yes/no/[fingerprint])?
```

Du befindest dich jetzt auf einem anderen Server, aber auch hier kannst du ganz normal mit `ls` und `cd` arbeiten. Du findest ein Verzeichnis, das so heisst wie deine Subdomain (z. B. `cobolultras`). Das ist der Ort, an dem du deine Dateien ablegen kannst. Du kannst in dieses Verzeichnis wechseln:

```bash
cd cobolultras
```

Du kannst auch Dateien erstellen, z. B. eine `index.html`:

```bash
echo "<h1>Hallo Welt</h1>" > index.html
```

Diese Datei kannst du jetzt in deinem Browser aufrufen, indem du die URL `http://cobolultras.hackschule.de` eingibst. Du solltest den Text "Hallo Welt" sehen.

## Dateien mit rsync hochladen

Um Dateien auf deinen Server hochzuladen, kannst du `rsync` verwenden. Das ist ein Programm, das Dateien zwischen zwei Computern synchronisiert. Du kannst es verwenden, um Dateien von deinem lokalen Computer auf den Server zu kopieren. Gehe dazu in das Verzeichnis, das du hochladen möchtest. Wenn du z. B. die Dateien in `/workspace/website-test` hast, gehe in dieses Verzeichnis:

```bash
cd /workspace/website-test
```

Anschließend kannst du `rsync` verwenden, um die Dateien auf den Server zu kopieren. Ersetze `max` und `cobolultras` durch deinen Benutzernamen und deine Subdomain:

```bash
rsync -avz . max@cobolultras.hackschule.de:cobolultras
```

Das `-a` steht für "archive" und sorgt dafür, dass die Dateien rekursiv (also mit allen Unterverzeichnissen) kopiert werden und die Dateiberechtigungen beibehalten werden. Das `-v` steht für "verbose" und zeigt dir an, welche Dateien kopiert werden. Das `-z` steht für "compress" und sorgt dafür, dass die Daten komprimiert übertragen werden, was die Übertragungsgeschwindigkeit erhöht.

Wenn du rsync mehrfach ausführst, werden nur die Dateien übertragen, die sich geändert haben. Das ist besonders nützlich, wenn du an deiner Website arbeitest und immer wieder neue Dateien hochladen möchtest.

Viel Spaß beim Programmieren!