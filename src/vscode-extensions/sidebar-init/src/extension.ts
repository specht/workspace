import * as fs from "fs";
import * as vscode from "vscode";

// On the first start of a Workspace: the sidebar shown, and for a brand-new
// Workspace (nothing in /workspace but hidden files) the walkthrough »Willkommen
// im Hackschule Workspace«, once. Existing Workspaces do not get a new tab in
// the middle of a lesson; the walkthrough stays under Help › Welcome.
const SIDEBAR_KEY = "hackschule.sidebarInitialized.v1";
const WELCOME_KEY = "hackschule.welcomeShown.v1";
const WALKTHROUGH = "hackschule.hackschule-sidebar-init#willkommen";

function workspaceIsNew(): boolean {
  try {
    return fs.readdirSync("/workspace").every((name) => name.startsWith("."));
  } catch {
    return false;
  }
}

export async function activate(context: vscode.ExtensionContext) {
  if (!context.globalState.get<boolean>(SIDEBAR_KEY, false)) {
    try {
      // Make primary sidebar visible + focused if it was hidden.
      await vscode.commands.executeCommand("workbench.action.focusSideBar");
      await context.globalState.update(SIDEBAR_KEY, true);
    } catch {
      // Don't brick startup; try again next time.
    }
  }

  if (!context.globalState.get<boolean>(WELCOME_KEY, false)) {
    await context.globalState.update(WELCOME_KEY, true);
    if (workspaceIsNew()) {
      try {
        await vscode.commands.executeCommand("workbench.action.openWalkthrough", WALKTHROUGH, false);
      } catch {
        // The walkthrough is a welcome, not a requirement.
      }
    }
  }
}

export function deactivate() {}
