# TODO

## Subdomains

Statt den Workspace über einen Pfad wie `https://workspace.hackschule.de/123` zu erreichen, könnte man das System auch so umstellen, dass es über Subdomains funktioniert, z.B. `https://123.workspace.hackschule.de`. Dazu wird allerdings ein Wildcard-Zertifikat benötigt, das für alle Subdomains gilt. Das ist bei Let's Encrypt zwar möglich, erfordert aber regelmäßige Erneuerung, die nicht so einfach automatisiert werden kann.

Es muss also einen automatischen Erinnerungsprozess geben, der das aktuelle Zertifikat regelmäßig überprüft und rechtzeitig vor Ablauf (2 Wochen) eine E-Mail sendet, damit das Zertifikat erneuert werden kann.

Dieses Feature wäre wichtig, um Neo4j zu unterstützen, da das Bolt-Protokoll nur über Host und Port funktioniert und und keine Pfade unterstützt. Das würde bedeuten, dass Neo4j dann über Subdomains erreichbar wäre, z. B.:

- `https://neo4j.123.workspace.hackschule.de`
- `https://bolt.123.workspace.hackschule.de`