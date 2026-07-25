const vscode = acquireVsCodeApi();
let state = {};
let currentStepKey = null;
let currentStepPersistedComplete = false;
let markedAsComplete = false;
let stepCleanupCallbacks = [];
let showingCompletion = false;

function nop() { }

let handleOnDidChangeTextDocument = nop;
let handleOnDidSaveTextDocument = nop;
let handleOnDidChangeTextEditorSelection = nop;
let handleOnDidChangeActiveTextEditor = nop;
let handleOnDidChangeTextEditorOptions = nop;
let handleOnDidChangeTextEditorVisibleRanges = nop;
let handleOnDidCreateFiles = nop;
let handleOnDidRenameFiles = nop;
let handleOnDidDeleteFiles = nop;
let handleOnDidChangeTabs = nop;

function resetStepRuntime() {
    for (const cleanup of stepCleanupCallbacks.splice(0)) {
        try {
            cleanup();
        } catch (error) {
            console.error("Tutorial cleanup failed", error);
        }
    }

    handleOnDidChangeTextDocument = nop;
    handleOnDidSaveTextDocument = nop;
    handleOnDidChangeTextEditorSelection = nop;
    handleOnDidChangeActiveTextEditor = nop;
    handleOnDidChangeTextEditorOptions = nop;
    handleOnDidChangeTextEditorVisibleRanges = nop;
    handleOnDidCreateFiles = nop;
    handleOnDidRenameFiles = nop;
    handleOnDidDeleteFiles = nop;
    handleOnDidChangeTabs = nop;
}

function onStepCleanup(callback) {
    stepCleanupCallbacks.push(callback);
}

function addStepEventListener(target, type, listener, options) {
    target.addEventListener(type, listener, options);
    onStepCleanup(() => target.removeEventListener(type, listener, options));
}

function runTutorialAction(action, completeStep = false) {
    vscode.postMessage({
        command: "run_tutorial_action",
        action,
        completeStep: completeStep
            ? currentStepKey
            : undefined,
    });
}

function completeManualTask(id, action) {
    setCheckBox(id, true);
    checkTaskSolved();

    if (action) {
        /*
         * Include the step in the action message as well. Moving a view can
         * recreate its webview, so the extension persists completion before
         * it changes the workbench layout.
         */
        runTutorialAction(action, true);
    }
}

function applySnapshot(snapshot) {
    if (!snapshot) {
        return;
    }

    const documentEvent = {
        document: snapshot.document,
        contentChanges: [],
    };
    handleOnDidChangeTextDocument(documentEvent, snapshot.contents);
    handleOnDidChangeTextEditorSelection({
        document: snapshot.document,
        selections: snapshot.selections,
    });
    handleOnDidChangeActiveTextEditor({
        document: snapshot.document,
        selections: snapshot.selections,
        visibleRanges: snapshot.visibleRanges,
    });
    handleOnDidChangeTextEditorVisibleRanges({
        document: snapshot.document,
        visibleRanges: snapshot.visibleRanges,
    });
}

window.addEventListener("message", event => {
    const message = event.data;
    switch (message.command) {
        case "load_step_return": {
            if (message.key !== currentStepKey) {
                return;
            }

            state = message.state;
            currentStepPersistedComplete = state[currentStepKey] === true;
            markedAsComplete = currentStepPersistedComplete;
            document.querySelector("#instruction").innerHTML = message.step.instruction;

            try {
                // Tutorial files are bundled with the extension and act as small,
                // deliberately flexible exercise plug-ins.
                eval(message.step.script ?? "");
                applySnapshot(message.snapshot);
            } catch (error) {
                console.error(error);
                showError(`Der Tutorial-Schritt konnte nicht geladen werden: ${error.message}`);
            }

            const nextButton = document.querySelector("#bu_next");
            nextButton.disabled = !currentStepPersistedComplete;
            updateToc();
            break;
        }
        case "update_state":
            state = message.state;
            currentStepPersistedComplete = currentStepKey !== null && state[currentStepKey] === true;
            if (currentStepPersistedComplete && !showingCompletion) {
                document.querySelector("#bu_next").disabled = false;
                markedAsComplete = true;
            }
            updateToc();
            break;
        case "click_step":
            clickStep(message.step);
            break;
        case "tutorial_reset_complete":
            state = message.state;
            currentStepPersistedComplete = false;
            markedAsComplete = false;
            showingCompletion = false;
            updateToc();
            clickStep(0, true);
            break;
        case "show_completion":
            showTutorialCompletion();
            break;
        case "show_error":
            showError(message.message);
            break;
        case "onDidChangeTextDocument":
            handleOnDidChangeTextDocument(message.event, message.contents);
            break;
        case "onDidSaveTextDocument":
            handleOnDidSaveTextDocument(message.event, message.contents);
            break;
        case "onDidChangeTextEditorSelection":
            handleOnDidChangeTextEditorSelection(message.event);
            break;
        case "onDidChangeActiveTextEditor":
            handleOnDidChangeActiveTextEditor(message.event);
            break;
        case "onDidChangeTextEditorOptions":
            handleOnDidChangeTextEditorOptions(message.event);
            break;
        case "onDidChangeTextEditorVisibleRanges":
            handleOnDidChangeTextEditorVisibleRanges(message.event);
            break;
        case "onDidCreateFiles":
            handleOnDidCreateFiles(message.event);
            break;
        case "onDidRenameFiles":
            handleOnDidRenameFiles(message.event);
            break;
        case "onDidDeleteFiles":
            handleOnDidDeleteFiles(message.event);
            break;
        case "onDidChangeTabs":
            handleOnDidChangeTabs(message.event);
            break;
    }
});

function showError(message) {
    document.querySelector("#instruction").innerHTML = `
        <h2>Da ist etwas schiefgegangen</h2>
        <div class="error">${escapeHtml(message)}</div>
    `;
    document.querySelector("#bu_next").disabled = true;
}

function escapeHtml(text) {
    const element = document.createElement("div");
    element.textContent = text;
    return element.innerHTML;
}

function updateToc() {
    const resetRow = document.querySelector("#reset_tutorial_row");
    if (resetRow) {
        const tutorialComplete =
            stepOrder.length > 0 &&
            stepOrder.every(key => state[key] === true);
        resetRow.hidden = !tutorialComplete;
    }

    for (const element of document.querySelectorAll('tr[data-type="section"], tr[data-type="step"]')) {
        element.classList.remove("active");
    }

    for (const element of document.querySelectorAll('tr[data-type="step"]')) {
        element.style.display = "none";
    }

    if (stepOrder.length === 0) {
        return;
    }

    for (const element of document.querySelectorAll(
        `tr[data-type="step"][data-section-index="${stepSection[stepIndex]}"]`,
    )) {
        element.style.display = "";
    }

    document.querySelector(`tr[data-type="step"][data-step-index="${stepIndex}"]`)?.classList.add("active");
    document.querySelector(`tr[data-type="section"][data-section-index="${stepSection[stepIndex]}"]`)?.classList.add("active");

    const sectionUnsolved = {};
    let maxSection = 0;

    for (let i = 0; i < stepOrder.length; i += 1) {
        maxSection = Math.max(maxSection, stepSection[i]);
        const check = document.querySelector(
            `tr[data-type="step"][data-step-key="${stepOrder[i]}"] .check`,
        );
        if (state[stepOrder[i]]) {
            check?.classList.add("checked");
        } else {
            check?.classList.remove("checked");
            sectionUnsolved[stepSection[i]] = true;
        }
    }

    for (let i = 0; i <= maxSection; i += 1) {
        const check = document.querySelector(`tr[data-type="section"][data-section-index="${i}"] .check`);
        check?.classList.toggle("checked", !sectionUnsolved[i]);
    }
}

function checkTaskSolved() {
    const checks = document.querySelectorAll("#instruction .check");
    const solved = checks.length > 0 && Array.from(checks).every(
        element => element.classList.contains("checked"),
    );

    if (solved) {
        markTaskComplete();
    } else {
        markTaskIncomplete();
    }
}

function markTaskComplete() {
    const button = document.querySelector("#bu_next");
    button.disabled = false;

    if (!markedAsComplete) {
        button.classList.remove("pop");
        void button.offsetWidth;
        button.classList.add("pop");
    }

    markedAsComplete = true;
    if (!state[currentStepKey]) {
        state[currentStepKey] = true;
        currentStepPersistedComplete = true;
        updateToc();
        vscode.postMessage({
            command: "mark_step_complete",
            step: currentStepKey,
        });
    }
}

function markTaskIncomplete() {
    if (currentStepPersistedComplete) {
        return;
    }
    document.querySelector("#bu_next").disabled = true;
    markedAsComplete = false;
}

function clickStep(n, restart = false) {
    const parsedIndex = Number.parseInt(`${n}`, 10);
    if (!Number.isInteger(parsedIndex) || parsedIndex < 0 || parsedIndex >= stepOrder.length) {
        return;
    }

    stepIndex = parsedIndex;
    currentStepKey = stepOrder[stepIndex];
    currentStepPersistedComplete = state[currentStepKey] === true;
    document.querySelector("#bu_reload").disabled = false;
    markedAsComplete = currentStepPersistedComplete;
    showingCompletion = false;
    resetStepRuntime();
    document.querySelector("#instruction").innerHTML = "<p>Schritt wird geladen …</p>";
    const nextButton = document.querySelector("#bu_next");
    nextButton.querySelector("span").textContent = "Nächster Schritt";
    nextButton.disabled = !currentStepPersistedComplete;
    updateToc();

    vscode.postMessage({
        command: "load_step",
        key: currentStepKey,
        restart,
    });
}

function clickRestartStep() {
    clickStep(stepIndex, true);
}

function clickResetTutorial() {
    vscode.postMessage({ command: "reset_tutorial" });
}

function showTutorialCompletion() {
    showingCompletion = true;
    resetStepRuntime();

    if (stepOrder.length > 0) {
        stepIndex = stepOrder.length - 1;
        currentStepKey = stepOrder[stepIndex];
    }

    const completedSteps = stepOrder.filter(key => state[key]).length;
    document.querySelector("#instruction").innerHTML = `
        <h2>Tutorial abgeschlossen!</h2>
        <p>Du hast alle ${completedSteps} Tutorial-Schritte erledigt.</p>
        <p>Der Tutorial-Arbeitsordner ist geschlossen. VS Code ist wieder bereit für dein nächstes Projekt.</p>
        <p>Über das Inhaltsverzeichnis kannst du einzelne Übungen jederzeit wiederholen. Mit <strong>Gesamtes Tutorial zurücksetzen</strong> kannst du später noch einmal ganz von vorne beginnen.</p>
    `;
    const nextButton = document.querySelector("#bu_next");
    nextButton.querySelector("span").textContent = "Tutorial abgeschlossen";
    nextButton.disabled = true;
    document.querySelector("#bu_reload").disabled = true;
    updateToc();
}

function clickNextStep() {
    if (stepIndex + 1 < stepOrder.length) {
        clickStep(stepIndex + 1);
        return;
    }

    showTutorialCompletion();
}

function clickSection(n) {
    const sectionIndex = Number.parseInt(`${n}`, 10);
    if (firstStepForSection[sectionIndex] === null) {
        return;
    }
    clickStep(firstStepForSection[sectionIndex]);
}

window.addEventListener("DOMContentLoaded", () => {
    let number = 0;
    const tbody = document.querySelector("table.toc tbody");
    for (const section of sections.sections) {
        number += 1;

        tbody.insertAdjacentHTML("beforeend", `
            <tr data-type="section" data-section-index="${number - 1}">
                <td>${number}.</td>
                <td colspan="2">${section.heading}</td>
                <td>${checkBox()}</td>
            </tr>
        `);
        firstStepForSection.push(null);

        for (const step of section.steps) {
            if (firstStepForSection[firstStepForSection.length - 1] === null) {
                firstStepForSection[firstStepForSection.length - 1] = stepOrder.length;
            }
            stepOrder.push(step.key);
            stepSection.push(number - 1);
            tbody.insertAdjacentHTML("beforeend", `
                <tr data-type="step" data-step-index="${stepOrder.length - 1}"
                    data-step-key="${step.key}" data-section-index="${number - 1}">
                    <td></td>
                    <td style="width: 0.5em;">&ndash;</td>
                    <td>${step.heading}</td>
                    <td>${checkBox()}</td>
                </tr>
            `);
        }
    }

    tbody.addEventListener("click", event => {
        let row = event.target.closest('tr[data-type="section"]');
        if (row) {
            clickSection(row.dataset.sectionIndex);
            return;
        }
        row = event.target.closest('tr[data-type="step"]');
        if (row) {
            clickStep(row.dataset.stepIndex);
        }
    });

    updateToc();
    vscode.postMessage({ command: "ready" });
});

function checkBox(id) {
    const idAttribute = id ? ` id="${id}"` : "";
    return `<span${idAttribute} class="check"><svg class="icon"><use href="#circle-dotted"></use></svg><svg class="icon"><use href="#check"></use></svg></span>`;
}

function setCheckBox(id, flag) {
    const check = document.querySelector(`#instruction #${id}.check`);
    check?.classList.toggle("checked", Boolean(flag));
}

function getCheckBox(id) {
    const check = document.querySelector(`#instruction #${id}.check`);
    return check?.classList.contains("checked") ?? false;
}

function selectionEquals(selection, startLine, startCharacter, endLine, endCharacter) {
    return selection.start.line === startLine &&
        selection.start.character === startCharacter &&
        selection.end.line === endLine &&
        selection.end.character === endCharacter;
}

function isCursorAt(selection, line, character) {
    return selection.isEmpty && selection.start.line === line && selection.start.character === character;
}
