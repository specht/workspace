import * as vscode from "vscode";

interface DiffSection {
  fileName: string;
  status: "geändert" | "neu" | "gelöscht" | "umbenannt" | "binär geändert";
  additions: number;
  deletions: number;
  body: string[];
}

export interface FriendlyDiff {
  content: string;
  fileCount: number;
  additions: number;
  deletions: number;
}

function singularOrPlural(count: number, singular: string, plural: string): string {
  return `${count} ${count === 1 ? singular : plural}`;
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

  let additions = 0;
  let deletions = 0;
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

    if (!inHunk) continue;

    if (line.startsWith("+") && !line.startsWith("+++")) additions += 1;
    if (line.startsWith("-") && !line.startsWith("---")) deletions += 1;
    body.push(line);
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
    additions,
    deletions,
    body,
  };
}

export function formatFriendlyDiff(checkpointName: string, rawDiff: string): FriendlyDiff {
  const blocks = rawDiff
    .split(/(?=^diff --git )/m)
    .map(block => block.trimEnd())
    .filter(block => block.startsWith("diff --git "));

  const sections = blocks
    .map(parseSection)
    .filter((section): section is DiffSection => section !== undefined);

  const additions = sections.reduce((sum, section) => sum + section.additions, 0);
  const deletions = sections.reduce((sum, section) => sum + section.deletions, 0);

  if (sections.length === 0) {
    return {
      fileCount: 0,
      additions: 0,
      deletions: 0,
      content: [
        `# Vergleich mit „${checkpointName}“`,
        "#",
        "# Seit diesem Checkpoint hat sich nichts geändert.",
        "",
      ].join("\n"),
    };
  }

  const summary = [
    singularOrPlural(sections.length, "Datei", "Dateien"),
    `${additions} ${additions === 1 ? "Zeile hinzugefügt" : "Zeilen hinzugefügt"}`,
    `${deletions} ${deletions === 1 ? "Zeile entfernt" : "Zeilen entfernt"}`,
  ].join(" · ");

  const output: string[] = [
    `# Vergleich mit „${checkpointName}“`,
    `# ${summary}`,
    "# Grün (+) ist neu, Rot (-) wurde entfernt.",
    "",
  ];

  for (const section of sections) {
    output.push(
      `# Datei: ${section.fileName} (${section.status})`,
      `# ${section.additions} hinzugefügt · ${section.deletions} entfernt`,
      "# ────────────────────────────────────────────────",
      ...section.body,
      "",
    );
  }

  return {
    content: output.join("\n"),
    fileCount: sections.length,
    additions,
    deletions,
  };
}

export class CheckpointDiffProvider
  implements vscode.TextDocumentContentProvider
{
  static readonly scheme = "hackschule-checkpoint-diff";

  private readonly contents = new Map<string, string>();

  provideTextDocumentContent(uri: vscode.Uri): string {
    return this.contents.get(uri.toString()) ?? "Vergleich nicht mehr verfügbar.\n";
  }

  createDocumentUri(checkpointName: string, rawDiff: string): vscode.Uri {
    const safeName = checkpointName.replace(/[\\/:*?\"<>|]/g, "-");
    const uri = vscode.Uri.from({
      scheme: CheckpointDiffProvider.scheme,
      path: `/Vergleich mit ${safeName}.diff`,
      query: String(Date.now()),
    });
    this.contents.set(uri.toString(), formatFriendlyDiff(checkpointName, rawDiff).content);
    return uri;
  }
}
