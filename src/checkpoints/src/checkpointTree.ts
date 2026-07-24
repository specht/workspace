import * as vscode from "vscode";
import type {
  ByteStats,
  Checkpoint,
  RepositoryContext,
} from "./types";
import {
  checkpointByteStats,
  listCheckpoints,
} from "./git";

interface CheckpointDayGroup {
  kind: "day";
  key: string;
  label: string;
  checkpoints: Checkpoint[];
}

type CheckpointTreeNode = CheckpointDayGroup | Checkpoint;

function isDayGroup(node: CheckpointTreeNode): node is CheckpointDayGroup {
  return "kind" in node && node.kind === "day";
}

function localDayKey(date: Date): string {
  const year = date.getFullYear();
  const month = String(date.getMonth() + 1).padStart(2, "0");
  const day = String(date.getDate()).padStart(2, "0");
  return `${year}-${month}-${day}`;
}

function localDayLabel(date: Date): string {
  return new Intl.DateTimeFormat("de-DE", {
    weekday: "long",
    day: "2-digit",
    month: "2-digit",
    year: "numeric",
  }).format(date);
}

function localTimeLabel(timestamp: number): string {
  return new Intl.DateTimeFormat("de-DE", {
    hour: "2-digit",
    minute: "2-digit",
    second: "2-digit",
  }).format(new Date(timestamp * 1000));
}

export function formatBytes(bytes: number): string {
  if (bytes < 1024) return `${bytes} B`;
  const units = ["KB", "MB", "GB", "TB"];
  let value = bytes / 1024;
  let unit = units[0];
  for (let index = 1; index < units.length && value >= 1024; index += 1) {
    value /= 1024;
    unit = units[index];
  }
  const digits = value >= 100 ? 0 : value >= 10 ? 1 : 2;
  return `${value.toLocaleString("de-DE", {
    minimumFractionDigits: 0,
    maximumFractionDigits: digits,
  })} ${unit}`;
}

function statsLabel(stats: ByteStats | undefined): string {
  if (!stats) return "+? −?";
  return `+${formatBytes(stats.addedBytes)} −${formatBytes(stats.removedBytes)}`;
}

function groupByLocalDay(checkpoints: Checkpoint[]): CheckpointDayGroup[] {
  const groups: CheckpointDayGroup[] = [];

  for (const checkpoint of checkpoints) {
    const date = new Date(checkpoint.timestamp * 1000);
    const key = localDayKey(date);
    const latestGroup = groups.at(-1);

    if (!latestGroup || latestGroup.key !== key) {
      groups.push({
        kind: "day",
        key,
        label: localDayLabel(date),
        checkpoints: [checkpoint],
      });
    } else {
      latestGroup.checkpoints.push(checkpoint);
    }
  }

  return groups;
}

export class CheckpointTreeProvider
  implements vscode.TreeDataProvider<CheckpointTreeNode>
{
  private readonly changedEmitter =
    new vscode.EventEmitter<CheckpointTreeNode | undefined | void>();

  private readonly statsCache = new Map<string, ByteStats>();

  readonly onDidChangeTreeData = this.changedEmitter.event;

  constructor(
    private readonly getContext:
      () => Promise<RepositoryContext | undefined>,
  ) {}

  refresh(clearStats = false): void {
    if (clearStats) this.statsCache.clear();
    this.changedEmitter.fire();
  }

  getTreeItem(node: CheckpointTreeNode): vscode.TreeItem {
    if (isDayGroup(node)) {
      const item = new vscode.TreeItem(
        node.label,
        vscode.TreeItemCollapsibleState.Expanded,
      );
      item.contextValue = "hackschuleCheckpointDay";
      item.iconPath = new vscode.ThemeIcon("calendar");
      item.tooltip = `${node.checkpoints.length} Checkpoint${
        node.checkpoints.length === 1 ? "" : "s"
      }`;
      return item;
    }

    const item = new vscode.TreeItem(
      node.name,
      vscode.TreeItemCollapsibleState.None,
    );
    item.description = `${localTimeLabel(node.timestamp)} · ${statsLabel(node.byteStats)}`;
    item.iconPath = new vscode.ThemeIcon(
      node.action === "restore" ? "history" : "archive",
    );
    item.contextValue = "hackschuleCheckpoint";

    const stats = node.byteStats;
    item.tooltip = new vscode.MarkdownString([
      `**${node.name}**`,
      "",
      new Date(node.timestamp * 1000).toLocaleString("de-DE"),
      "",
      stats
        ? `**Neue oder veränderte Daten:** ${formatBytes(stats.addedBytes)}`
        : "Byte-Statistik nicht verfügbar.",
      stats
        ? `**Ersetzte oder entfernte Daten:** ${formatBytes(stats.removedBytes)}`
        : "",
      stats
        ? `**Betroffene Dateien:** ${stats.changedFiles}`
        : "",
      "",
      "Die Byte-Werte beschreiben geänderte Projektinhalte, nicht den exakten Speicherbedarf in Git.",
      "",
      "Benutze den Vergleichs- oder Wiederherstellen-Knopf rechts.",
    ].filter(Boolean).join("\n"));
    return item;
  }

  async getChildren(
    element?: CheckpointTreeNode,
  ): Promise<CheckpointTreeNode[]> {
    if (element) {
      return isDayGroup(element) ? element.checkpoints : [];
    }

    const context = await this.getContext();
    if (!context) {
      await vscode.commands.executeCommand(
        "setContext",
        "hackschuleCheckpoints.hasItems",
        false,
      );
      return [];
    }

    const checkpoints = await listCheckpoints(context);
    await vscode.commands.executeCommand(
      "setContext",
      "hackschuleCheckpoints.hasItems",
      checkpoints.length > 0,
    );

    const enriched: Checkpoint[] = [];
    // Keep Git process usage bounded. A classroom project may accumulate many
    // checkpoints, and starting hundreds of diff processes at once would make
    // the sidebar slower rather than faster.
    for (const checkpoint of checkpoints) {
      let byteStats = this.statsCache.get(checkpoint.oid);
      if (!byteStats) {
        byteStats = await checkpointByteStats(context, checkpoint);
        this.statsCache.set(checkpoint.oid, byteStats);
      }
      enriched.push({ ...checkpoint, byteStats });
    }

    return groupByLocalDay(enriched);
  }
}
