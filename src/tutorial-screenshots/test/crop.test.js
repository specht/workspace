import assert from 'node:assert/strict';
import test from 'node:test';

import {
    parseTerminalLineCropDirective,
    parseTerminalLineCount,
    parseTerminalLineSkip,
    scalePixelDimensions,
    terminalLineCropPixels,
    validateCropDirectives,
} from '../crop.js';

test('capture scale multiplies native screenshot dimensions', () => {
    assert.deepEqual(scalePixelDimensions(1000, 500, 2), {
        width: 2000,
        height: 1000,
    });
    assert.deepEqual(scalePixelDimensions(1000, 500, 1.5), {
        width: 1500,
        height: 750,
    });
});

test('crop-terminal-lines directive parser recognizes integer and auto syntax', () => {
    assert.equal(parseTerminalLineCropDirective('crop-terminal-lines: 3'), 3);
    assert.equal(parseTerminalLineCropDirective('crop-terminal-lines:   12'), 12);
    assert.equal(parseTerminalLineCropDirective('crop-terminal-lines: auto'), 'auto');
    assert.equal(parseTerminalLineCropDirective('crop-bottom: 25%'), null);
    assert.throws(
        () => parseTerminalLineCropDirective('crop-terminal-lines:'),
        /crop-terminal-lines must be a positive integer or auto/,
    );
});

test('crop-terminal-lines parses positive integers or auto only', () => {
    assert.equal(parseTerminalLineCount('3'), 3);
    assert.equal(parseTerminalLineCount(' 12 '), 12);
    assert.equal(parseTerminalLineCount(' auto '), 'auto');
    for (const value of ['0', '-1', '1.5', 'three', '9007199254740992']) {
        assert.throws(
            () => parseTerminalLineCount(value),
            /crop-terminal-lines must be a positive integer or auto/,
        );
    }
});

test('terminal crop skip directives parse non-negative integers only', () => {
    assert.equal(parseTerminalLineSkip('0', 'crop-terminal-skip-top'), 0);
    assert.equal(parseTerminalLineSkip(' 2 ', 'crop-terminal-skip-bottom'), 2);
    for (const value of ['-1', '1.5', 'one', '9007199254740992']) {
        assert.throws(
            () => parseTerminalLineSkip(value, 'crop-terminal-skip-bottom'),
            /crop-terminal-skip-bottom must be a non-negative integer/,
        );
    }
});

test('crop-terminal-lines conflicts with manual crops and preview capture', () => {
    assert.doesNotThrow(() => validateCropDirectives({
        cropTopSpecified: false,
        cropBottomSpecified: false,
        cropTerminalLines: 3,
        tab: 'workspace',
    }));
    assert.doesNotThrow(() => validateCropDirectives({
        cropTopSpecified: false,
        cropBottomSpecified: false,
        cropTerminalLines: 'auto',
        tab: 'workspace',
    }));
    assert.throws(
        () => validateCropDirectives({
            cropTopSpecified: true,
            cropBottomSpecified: false,
            cropTerminalLines: 3,
            tab: 'workspace',
        }),
        /cannot be combined with crop-top or crop-bottom/,
    );
    assert.throws(
        () => validateCropDirectives({
            cropTopSpecified: false,
            cropBottomSpecified: true,
            cropTerminalLines: 3,
            tab: 'workspace',
        }),
        /cannot be combined with crop-top or crop-bottom/,
    );
    assert.throws(
        () => validateCropDirectives({
            cropTopSpecified: false,
            cropBottomSpecified: false,
            cropTerminalLines: 'auto',
            tab: 'preview',
        }),
        /requires tab: workspace/,
    );
});

test('terminal crop skips require crop-terminal-lines', () => {
    assert.throws(
        () => validateCropDirectives({
            cropTopSpecified: false,
            cropBottomSpecified: false,
            cropTerminalLines: null,
            cropTerminalSkipTop: 0,
            cropTerminalSkipBottom: 1,
            tab: 'workspace',
        }),
        /require crop-terminal-lines/,
    );
});

test('terminal crop keeps the panel header, requested rows and half-row padding', () => {
    const crop = terminalLineCropPixels({
        viewportHeight: 929,
        deviceScaleFactor: 1.8,
        panelTop: 34.25,
        screenBottom: 500,
        rows: [
            { bottom: 80, height: 20, empty: false },
            { bottom: 100, height: 20, empty: false },
            { bottom: 120, height: 20, empty: false },
            { bottom: 140, height: 20, empty: false },
        ],
        lineCount: 3,
    });

    assert.deepEqual(crop, {
        topPixels: 61,
        bottomPixels: 695,
        heightPixels: 173,
        keptRows: 3,
        availableRows: 4,
    });
});

test('terminal crop counts blank rendered rows for numeric crops', () => {
    const crop = terminalLineCropPixels({
        viewportHeight: 500,
        deviceScaleFactor: 2,
        panelTop: 10,
        screenBottom: 300,
        rows: [
            { bottom: 60, height: 20, empty: false },
            { bottom: 80, height: 20, empty: true },
            { bottom: 100, height: 20, empty: true },
            { bottom: 120, height: 20, empty: false },
        ],
        lineCount: 3,
    });

    assert.deepEqual(crop, {
        topPixels: 20,
        bottomPixels: 280,
        heightPixels: 200,
        keptRows: 3,
        availableRows: 4,
    });
});

test('terminal crop auto mode keeps rows through the last visible non-empty row', () => {
    const crop = terminalLineCropPixels({
        viewportHeight: 400,
        deviceScaleFactor: 2,
        panelTop: 10,
        screenBottom: 160,
        rows: [
            { bottom: 90, height: 20, empty: false },
            { bottom: 110, height: 20, empty: false },
            { bottom: 130, height: 20, empty: true },
            { bottom: 150, height: 20, empty: true },
        ],
        lineCount: 'auto',
    });

    assert.deepEqual(crop, {
        topPixels: 20,
        bottomPixels: 160,
        heightPixels: 220,
        keptRows: 2,
        availableRows: 4,
    });
});

test('terminal crop auto mode keeps at least one row when all visible rows are empty', () => {
    const crop = terminalLineCropPixels({
        viewportHeight: 300,
        deviceScaleFactor: 2,
        panelTop: 5,
        screenBottom: 100,
        rows: [
            { bottom: 40, height: 20, empty: true },
            { bottom: 60, height: 20, empty: true },
        ],
        lineCount: 'auto',
    });

    assert.deepEqual(crop, {
        topPixels: 10,
        bottomPixels: 200,
        heightPixels: 90,
        keptRows: 1,
        availableRows: 2,
    });
});

test('terminal auto crop ignores skipped rows at the bottom', () => {
    const crop = terminalLineCropPixels({
        viewportHeight: 929,
        deviceScaleFactor: 1.8,
        panelTop: 34.25,
        screenBottom: 500,
        rows: [
            { top: 60, bottom: 80, height: 20, empty: false },
            { top: 80, bottom: 100, height: 20, empty: false },
            { top: 100, bottom: 120, height: 20, empty: false },
            { top: 120, bottom: 140, height: 20, empty: true },
            { top: 140, bottom: 160, height: 20, empty: false },
        ],
        lineCount: 'auto',
        skipBottom: 1,
    });

    assert.deepEqual(crop, {
        topPixels: 61,
        bottomPixels: 695,
        heightPixels: 173,
        keptRows: 3,
        availableRows: 5,
    });
});

test('terminal crop can skip rendered rows from the top', () => {
    const crop = terminalLineCropPixels({
        viewportHeight: 500,
        deviceScaleFactor: 2,
        panelTop: 10,
        screenBottom: 300,
        rows: [
            { top: 40, bottom: 60, height: 20, empty: false },
            { top: 60, bottom: 80, height: 20, empty: false },
            { top: 80, bottom: 100, height: 20, empty: false },
        ],
        lineCount: 2,
        skipTop: 1,
    });

    assert.deepEqual(crop, {
        topPixels: 120,
        bottomPixels: 280,
        heightPixels: 100,
        keptRows: 2,
        availableRows: 3,
    });
});

test('terminal crop rejects skip values that remove every rendered row', () => {
    assert.throws(
        () => terminalLineCropPixels({
            viewportHeight: 300,
            deviceScaleFactor: 2,
            panelTop: 5,
            screenBottom: 100,
            rows: [
                { top: 20, bottom: 40, height: 20, empty: false },
                { top: 40, bottom: 60, height: 20, empty: false },
            ],
            lineCount: 'auto',
            skipTop: 1,
            skipBottom: 1,
        }),
        /leave no rendered terminal rows/,
    );
});
