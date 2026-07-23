import * as vscode from "vscode";
import type { Checkpoint, RepositoryContext } from "./types";
import { listCheckpoints } from "./git";

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

  readonly onDidChangeTreeData = this.changedEmitter.event;

  constructor(
    private readonly getContext:
      () => Promise<RepositoryContext | undefined>,
  ) {}

  refresh(): void {
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
    item.description = localTimeLabel(node.timestamp);
    item.iconPath = new vscode.ThemeIcon(
      node.action === "restore" ? "history" : "archive",
    );
    item.contextValue = "hackschuleCheckpoint";
    item.tooltip = new vscode.MarkdownString([
      `**${node.name}**`,
      "",
      new Date(node.timestamp * 1000).toLocaleString("de-DE"),
      "",
      "Benutze den Vergleichs- oder Wiederherstellen-Knopf rechts.",
    ].join("\n"));
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

    return groupByLocalDay(checkpoints);
  }
}
