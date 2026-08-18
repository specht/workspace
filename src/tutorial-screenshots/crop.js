export function scalePixelDimensions(width, height, scale) {
    return {
        width: Math.max(1, Math.round(width * scale)),
        height: Math.max(1, Math.round(height * scale)),
    };
}

export function parseTerminalLineCount(value) {
    const text = `${value}`.trim();
    if (text === 'auto') {
        return 'auto';
    }
    if (!/^[1-9]\d*$/.test(text)) {
        throw new Error(`crop-terminal-lines must be a positive integer or auto: ${value}`);
    }

    const lineCount = Number(text);
    if (!Number.isSafeInteger(lineCount)) {
        throw new Error(`crop-terminal-lines must be a positive integer or auto: ${value}`);
    }
    return lineCount;
}

export function parseTerminalLineCropDirective(line) {
    const match = `${line}`.match(/^crop-terminal-lines:\s*(.*)$/);
    if (!match) return null;
    return parseTerminalLineCount(match[1]);
}

export function parseTerminalLineSkip(value, directive) {
    const text = `${value}`.trim();
    if (!/^\d+$/.test(text)) {
        throw new Error(`${directive} must be a non-negative integer: ${value}`);
    }

    const count = Number(text);
    if (!Number.isSafeInteger(count)) {
        throw new Error(`${directive} must be a non-negative integer: ${value}`);
    }
    return count;
}

export function validateCropDirectives({
    cropTopSpecified,
    cropBottomSpecified,
    cropTerminalLines,
    cropTerminalSkipTop = 0,
    cropTerminalSkipBottom = 0,
    tab,
}) {
    if (cropTerminalLines == null) {
        if (cropTerminalSkipTop > 0 || cropTerminalSkipBottom > 0) {
            throw new Error(
                'crop-terminal-skip-top and crop-terminal-skip-bottom require crop-terminal-lines',
            );
        }
        return;
    }

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
    skipTop = 0,
    skipBottom = 0,
}) {
    if (!Number.isInteger(viewportHeight) || viewportHeight <= 0) {
        throw new Error(`Invalid screenshot viewport height: ${viewportHeight}`);
    }
    if (!Number.isFinite(deviceScaleFactor) || deviceScaleFactor <= 0) {
        throw new Error(`Invalid effective device scale factor: ${deviceScaleFactor}`);
    }
    if (!(lineCount === 'auto' || (Number.isSafeInteger(lineCount) && lineCount > 0))) {
        throw new Error(`Invalid terminal line count: ${lineCount}`);
    }
    if (!Array.isArray(rows) || rows.length === 0) {
        throw new Error('crop-terminal-lines found no rendered terminal rows');
    }
    if (!Number.isSafeInteger(skipTop) || skipTop < 0) {
        throw new Error(`Invalid terminal top skip: ${skipTop}`);
    }
    if (!Number.isSafeInteger(skipBottom) || skipBottom < 0) {
        throw new Error(`Invalid terminal bottom skip: ${skipBottom}`);
    }
    if (skipTop + skipBottom >= rows.length) {
        throw new Error(
            'crop-terminal-lines skip values leave no rendered terminal rows',
        );
    }

    const consideredRows = rows.slice(
        skipTop,
        skipBottom === 0 ? rows.length : rows.length - skipBottom,
    );

    let keptRows;
    if (lineCount === 'auto') {
        const lastNonEmptyIndex = consideredRows.findLastIndex(row => !row.empty);
        keptRows = Math.max(1, lastNonEmptyIndex + 1);
    } else {
        keptRows = Math.min(lineCount, consideredRows.length);
    }
    const firstRow = consideredRows[0];
    const lastRow = consideredRows[keptRows - 1];
    if (
        !Number.isFinite(panelTop) ||
        !Number.isFinite(screenBottom) ||
        (skipTop > 0 && !Number.isFinite(firstRow?.top)) ||
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
    const topCss = skipTop > 0 ? firstRow.top : panelTop;
    const topPixels = Math.max(
        0,
        Math.min(viewportHeight - 1, Math.floor(topCss * deviceScaleFactor)),
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
