const vscode = require("vscode");

class TutorialViewProvider {
    constructor(context) {
        console.log("HEYA");
        this.context = context;
    }

    /**
    * Called by VS Code when it needs to render the view.
    */
    resolveWebviewView(webviewView, context, token) {
        console.log("HEY!");
        this.webviewView = webviewView;

        webviewView.webview.options = {
            enableScripts: true,
            localResourceRoots: [this.context.extensionUri],
        };

        webviewView.webview.html = this.getHtmlForWebview(webviewView.webview);
    }

    getHtmlForWebview(webview) {
        return /* html */ `
      <!DOCTYPE html>
      <html lang="de">
      <head>
        <meta charset="UTF-8" />
        <meta name="viewport" content="width=device-width, initial-scale=1.0" />
        <style>
        </style>
      </head>
      <body>
        <p>Herzlich Willkommen zum Keyboard Tutorial!</p>
      </body>
      </html>
    `;
    }
}

module.exports = TutorialViewProvider;
