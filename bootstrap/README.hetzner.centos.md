# Schnellstart: Workspace bei Hetzner Cloud hosten

Wenn du deinen eigenen Workspace bei Hetzner hosten möchtest, ist diese Anleitung die Richtige für dich.

## Voraussetzungen

Du benötigst:

- einen Hetzner-Account
- einen Public Key

## Schritt 0: Server und Volume anlegen

Erstelle einen Server:

- Cent OS Stream 10
- Shared vCPU (x86), zum Ausprobieren z. B. CPX21
- Gib gleich deinen SSH-Key an
- Servername: z. B. workspace-server

Erstelle ein Volume:

- 10 GB reichen zum ausprobieren (in meinem Workspace verwende ich 100 GB und es reicht momentan für 350 Nutzer:innen)
- Mount-Option: manuell (da wir nicht direkt XFS verwenden, sondern darunter noch VDO aufsetzen)

## Schritt 1: Server vorbereiten

Füge einen Eintrag in deine `~/.ssh/config` hinzu (oasse die Werte entsprechend an &ndash; trage unter `user` den Login ein, den du auf dem Server verwenden möchtest):

```
host workspace-server
hostname 188.34.187.108
user specht
port 22
identityFile /home/michael/.ssh/id_ed25519_hetzner
```


Verbinde dich als `root` mit deinem Server:

```
ssh root@workspace-server
```

Führe dann das 1. Setup-Skript aus:

```
curl --proto '=https' --tlsv1.2 -sSf https://raw.githubusercontent.com/specht/workspace/refs/heads/master/bootstrap/01-prepare-server.sh | sh
```

