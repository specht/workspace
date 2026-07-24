import * as vscode from "vscode";
import { formatBytes } from "./checkpointTree";
import type { ByteStats } from "./types";

interface DiffSection {
  fileName: string;
  status: "geändert" | "neu" | "gelöscht" | "umbenannt" | "binär geändert";
  body: string[];
}

function cleanGitPath(value: string | undefined): string | undefined {
  if (!value || value === "/dev/null") return undefined;
  return value.replace(/^[ab]\//, "");
}

function parseSection(block: string): DiffSection | undefined {
  const lines = block.split("\n");
  const header = /^diff --git a\/(.+) b\/(.+)$/.exec(lines[0] ?? "");
  if (!header) return undefined;

  const oldHeader = lines.find(line => line.startsWith("--- "))?.slice(4).trim();
  const newHeader = lines.find(line => line.startsWith("+++ "))?.slice(4).trim();
  const oldPath = cleanGitPath(oldHeader) ?? header[1];
  const newPath = cleanGitPath(newHeader) ?? header[2];

  let status: DiffSection["status"] = "geändert";
  if (lines.some(line => line.startsWith("new file mode")) || oldHeader === "/dev/null") {
    status = "neu";
  } else if (lines.some(line => line.startsWith("deleted file mode")) || newHeader === "/dev/null") {
    status = "gelöscht";
  } else if (lines.some(line => line.startsWith("rename from "))) {
    status = "umbenannt";
  } else if (lines.some(line => line.startsWith("Binary files ") || line.startsWith("GIT binary patch"))) {
    status = "binär geändert";
  }

  const body: string[] = [];
  let inHunk = false;

  for (const line of lines.slice(1)) {
    if (line.startsWith("@@")) {
      inHunk = true;
      body.push("@@ Ausschnitt @@");
      continue;
    }

    if (line.startsWith("Binary files ") || line.startsWith("GIT binary patch")) {
      body.push("# Der Inhalt dieser Datei kann nicht als Text angezeigt werden.");
      inHunk = false;
      continue;
    }

    if (inHunk) body.push(line);
  }

  if (body.length === 0) {
    if (status === "neu") body.push("# Neue Datei ohne darstellbare Textänderungen.");
    else if (status === "gelöscht") body.push("# Datei wurde gelöscht.");
    else if (status === "umbenannt") body.push(`# Umbenannt: ${oldPath} → ${newPath}`);
    else body.push("# Keine darstellbaren Textänderungen.");
  }

  return {
    fileName: status === "gelöscht" ? oldPath : newPath,
    status,
    body,
  };
}

export function formatFriendlyDiff(
  checkpointName: string,
  rawDiff: string,
  byteStats: ByteStats,
): string {
  const blocks = rawDiff
    .split(/(?=^diff --git )/m)
    .map(block => block.trimEnd())
    .filter(block => block.startsWith("diff --git "));

  const sections = blocks
    .map(parseSection)
    .filter((section): section is DiffSection => section !== undefined);

  if (sections.length === 0) {
    return [
      `# Vergleich mit „${checkpointName}“`,
      "#",
      "# Seit diesem Checkpoint hat sich nichts geändert.",
      "",
    ].join("\n");
  }

  const output: string[] = [
    `# Vergleich mit „${checkpointName}“`,
    `# ${sections.length} ${sections.length === 1 ? "Datei" : "Dateien"} betroffen`,
    `# +${formatBytes(byteStats.addedBytes)} neue oder veränderte Daten`,
    `# −${formatBytes(byteStats.removedBytes)} ersetzte oder entfernte Daten`,
    "# Grün (+) ist neu, Rot (-) wurde entfernt.",
    "",
  ];

  for (const section of sections) {
    output.push(
      `# Datei: ${section.fileName} (${section.status})`,
      "# ────────────────────────────────────────────────",
      ...section.body,
      "",
    );
  }

  return output.join("\n");
}

export class CheckpointDiffProvider
  implements vscode.TextDocumentContentProvider
{
  static readonly scheme = "hackschule-checkpoint-diff";

  private readonly contents = new Map<string, string>();

  provideTextDocumentContent(uri: vscode.Uri): string {
    return this.contents.get(uri.toString()) ?? "Vergleich nicht mehr verfügbar.\n";
  }

  createDocumentUri(
    checkpointName: string,
    rawDiff: string,
    byteStats: ByteStats,
  ): vscode.Uri {
    const safeName = checkpointName.replace(/[\\/:*?\"<>|]/g, "-");
    const uri = vscode.Uri.from({
      scheme: CheckpointDiffProvider.scheme,
      path: `/Vergleich mit ${safeName}.diff`,
      query: String(Date.now()),
    });
    this.contents.set(
      uri.toString(),
      formatFriendlyDiff(checkpointName, rawDiff, byteStats),
    );
    return uri;
  }
}
