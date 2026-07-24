import * as vscode from "vscode";
import type { Checkpoint } from "./types";

export async function askCheckpointName(
  suggestedName = ""
): Promise<string | undefined> {
  const value = await vscode.window.showInputBox({
    title: "Checkpoint erstellen",
    prompt: "Du kannst dem Checkpoint einen Namen geben oder das Feld leer lassen.",
    value: suggestedName,
    placeHolder: "Zum Beispiel: Bevor ich die Beleuchtung ändere",
    ignoreFocusOut: true,
    validateInput(input) {
      return input.trim().length > 120
        ? "Bitte verwende höchstens 120 Zeichen."
        : undefined;
    }
  });

  // undefined means Escape. An empty string is a valid unnamed checkpoint.
  return value === undefined ? undefined : value.trim();
}

export async function chooseCheckpoint(
  checkpoints: Checkpoint[]
): Promise<Checkpoint | undefined> {
  const selected = await vscode.window.showQuickPick(
    checkpoints.map(checkpoint => ({
      label: checkpoint.action === "restore"
        ? `$(history) ${checkpoint.name}`
        : `$(archive) ${checkpoint.name}`,
      description: new Date(checkpoint.timestamp * 1000).toLocaleString("de-DE"),
      detail: "Auswählen und anschließend ausdrücklich wiederherstellen",
      checkpoint
    })),
    {
      title: "Checkpoint auswählen",
      placeHolder: "Neuere Checkpoints bleiben beim Wiederherstellen erhalten.",
      ignoreFocusOut: true,
      matchOnDescription: true,
      matchOnDetail: true
    }
  );

  return selected?.checkpoint;
}
