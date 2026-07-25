const assert = require("assert");
const fs = require("fs");
const path = require("path");
const vm = require("vm");
const vscode = require("vscode");
const yaml = require("yaml");

const extensionRoot = path.resolve(__dirname, "..");
const tutorialRoot = path.join(extensionRoot, "tutorial");

function readSections() {
    return yaml.parse(fs.readFileSync(path.join(tutorialRoot, "sections.yaml"), "utf8"));
}

suite("Hackschule Keyboard Tutorial", () => {
    test("extension version matches the current release", () => {
        const manifest = JSON.parse(
            fs.readFileSync(path.join(extensionRoot, "package.json"), "utf8"),
        );
        assert.strictEqual(manifest.version, "0.5.1");
    });

    vscode.window.showInformationMessage("Testing Hackschule Keyboard Tutorial");

    test("every listed tutorial step exists and has valid JavaScript", () => {
        const sections = readSections();
        const keys = [];

        for (const section of sections.sections) {
            assert.ok(section.heading, "Every section needs a heading");
            assert.ok(Array.isArray(section.steps), `${section.heading} needs a steps array`);

            for (const step of section.steps) {
                assert.ok(step.heading, "Every step needs a heading");
                assert.ok(step.key, `${step.heading} needs a key`);
                assert.ok(!keys.includes(step.key), `Duplicate tutorial key: ${step.key}`);
                keys.push(step.key);

                const htmlPath = path.join(tutorialRoot, `${step.key}.html`);
                assert.ok(fs.existsSync(htmlPath), `Missing tutorial file: ${step.key}.html`);

                const html = fs.readFileSync(htmlPath, "utf8");
                const script = html.match(/<script>([\s\S]*?)<\/script>/i)?.[1] ?? "";
                assert.doesNotThrow(
                    () => new vm.Script(script, { filename: `${step.key}.html` }),
                    `Invalid JavaScript in ${step.key}.html`,
                );
            }
        }
    });

    test("every tutorial fixture referenced from YAML exists", () => {
        const sections = readSections();

        for (const section of sections.sections) {
            for (const step of section.steps) {
                const html = fs.readFileSync(path.join(tutorialRoot, `${step.key}.html`), "utf8");
                const metadataText = html.match(/<yaml>([\s\S]*?)<\/yaml>/i)?.[1];
                if (!metadataText) {
                    continue;
                }

                const metadata = yaml.parse(metadataText);
                if (metadata?.file) {
                    const fixturePath =
                        path.join(extensionRoot, metadata.file);
                    assert.ok(
                        fs.existsSync(fixturePath),
                        `Missing fixture: ${metadata.file}`,
                    );
                }

                if (metadata?.workspace) {
                    const fixturePath =
                        path.join(extensionRoot, metadata.workspace);
                    assert.ok(
                        fs.existsSync(fixturePath) &&
                            fs.statSync(fixturePath).isDirectory(),
                        `Missing workspace fixture: ${metadata.workspace}`,
                    );
                }
            }
        }
    });
});
