export function parseTerminalLineCount(value) {
    const text = `${value}`.trim();
    if (!/^[1-9]\d*$/.test(text)) {
        throw new Error(`crop-terminal-lines must be a positive integer: ${value}`);
    }

    const lineCount = Number(text);
    if (!Number.isSafeInteger(lineCount)) {
        throw new Error(`crop-terminal-lines must be a positive integer: ${value}`);
    }
    return lineCount;
}

export function parseTerminalLineCropDirective(line) {
    const match = `${line}`.match(/^crop-terminal-lines:\s*(.*)$/);
    if (!match) return null;
    return parseTerminalLineCount(match[1]);
}

export function validateCropDirectives({
    cropTopSpecified,
    cropBottomSpecified,
    cropTerminalLines,
    tab,
}) {
    if (cropTerminalLines == null) return;

    if (cropTopSpecified || cropBottomSpecified) {
        throw new Error(
            'crop-terminal-lines cannot be combined with crop-top or crop-bottom',
        );
    }
    if (tab !== 'workspace') {
        throw new Error('crop-terminal-lines requires tab: workspace');
    }
}

export function terminalLineCropPixels({
    viewportHeight,
    deviceScaleFactor,
    panelTop,
    screenBottom,
    rows,
    lineCount,
}) {
    if (!Number.isInteger(viewportHeight) || viewportHeight <= 0) {
        throw new Error(`Invalid screenshot viewport height: ${viewportHeight}`);
    }
    if (!Number.isFinite(deviceScaleFactor) || deviceScaleFactor <= 0) {
        throw new Error(`Invalid effective device scale factor: ${deviceScaleFactor}`);
    }
    if (!Number.isSafeInteger(lineCount) || lineCount <= 0) {
        throw new Error(`Invalid terminal line count: ${lineCount}`);
    }
    if (!Array.isArray(rows) || rows.length === 0) {
        throw new Error('crop-terminal-lines found no rendered terminal rows');
    }

    const keptRows = Math.min(lineCount, rows.length);
    const lastRow = rows[keptRows - 1];
    if (
        !Number.isFinite(panelTop) ||
        !Number.isFinite(screenBottom) ||
        !Number.isFinite(lastRow?.bottom) ||
        !Number.isFinite(lastRow?.height) ||
        lastRow.height <= 0
    ) {
        throw new Error('crop-terminal-lines received invalid terminal geometry');
    }

    // Preserve half a rendered xterm row below the requested content. This keeps
    // the crop visually separated from the last line while still adapting to the
    // terminal's actual font/line-height settings.
    const bottomCss = Math.min(
        screenBottom,
        lastRow.bottom + lastRow.height * 0.5,
    );
    const topPixels = Math.max(
        0,
        Math.min(viewportHeight - 1, Math.floor(panelTop * deviceScaleFactor)),
    );
    const bottomEdgePixels = Math.max(
        topPixels + 1,
        Math.min(viewportHeight, Math.ceil(bottomCss * deviceScaleFactor)),
    );

    return {
        topPixels,
        bottomPixels: viewportHeight - bottomEdgePixels,
        heightPixels: bottomEdgePixels - topPixels,
        keptRows,
        availableRows: rows.length,
    };
}
