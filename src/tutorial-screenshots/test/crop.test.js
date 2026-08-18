import assert from 'node:assert/strict';
import test from 'node:test';

import {
    parseTerminalLineCropDirective,
    parseTerminalLineCount,
    terminalLineCropPixels,
    validateCropDirectives,
} from '../crop.js';

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
