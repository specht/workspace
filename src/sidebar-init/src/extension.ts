import * as vscode from "vscode";

const FLAG_KEY = "hackschule.sidebarInitialized.v1";

export async function activate(context: vscode.ExtensionContext) {
  // Run once per user/profile
  const already = context.globalState.get<boolean>(FLAG_KEY, false);
  if (already) return;

  try {
    // Make primary sidebar visible + focused if it was hidden.
    await vscode.commands.executeCommand("workbench.action.focusSideBar");

    // Mark done so it doesn't keep running.
    await context.globalState.update(FLAG_KEY, true);
  } catch {
    // If anything goes wrong, don't brick startup. Just try again next time.
    // (Intentionally silent.)
  }
}

export function deactivate() {}
