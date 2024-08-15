# Prozesse

In diesem Kapitel schauen wir uns an, wie unter Linux Prozesse funktionieren. Wir werden einige Befehle kennenlernen, mit denen wir Prozesse anzeigen, starten, beenden und überwachen können. Wir werden auch sehen, wie wir Prozesse priorisieren und steuern können.

## Grundlagen

Ein Prozess ist ein laufendes Programm auf einem Computer. Jeder Prozess hat eine eindeutige Prozess-ID (PID), die es ermöglicht, ihn zu identifizieren und zu steuern. Prozesse können miteinander kommunizieren, indem sie Daten austauschen oder Signale senden.

Prozesse werden in einer Hierarchie organisiert. Ein Prozess kann einen oder mehrere Kindprozesse haben, die von ihm abgeleitet sind. Ein Prozess kann auch von einem Elternprozess abgeleitet sein. Die oberste Ebene der Prozesshierarchie ist der Init-Prozess, der den Computer beim Start initialisiert.

## Befehle

In der Kommandozeile verwenden wir Befehle, um mit Prozessen zu arbeiten. Hier sind einige der wichtigsten Befehle, die wir verwenden werden:

- `ps`: Zeigt Informationen über laufende Prozesse an.
- `top`: Zeigt eine Echtzeitübersicht über laufende Prozesse an.
- `kill`: Beendet einen Prozess.
- `killall`: Beendet alle Prozesse mit einem bestimmten Namen.
- `pkill`: Beendet Prozesse anhand von Kriterien.
- `nice`: Ändert die Priorität eines Prozesses.
- `renice`: Ändert die Priorität eines laufenden Prozesses.
- `nohup`: Führt einen Prozess im Hintergrund aus.
- `bg`: Setzt einen Prozess in den Hintergrund.
- `fg`: Holt einen Prozess in den Vordergrund.
- `jobs`: Zeigt laufende Prozesse an.
- `at`: Führt einen Befehl zu einem bestimmten Zeitpunkt aus.
- `cron`: Führt Befehle zu bestimmten Zeitpunkten regelmäßig aus.
