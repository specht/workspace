import assert from 'node:assert/strict';
import test from 'node:test';

import {
    parseTerminalLineCropDirective,
    parseTerminalLineCount,
    terminalLineCropPixels,
    validateCropDirectives,
} from '../crop.js';

test('crop-terminal-lines directive parser recognizes and validates the recipe syntax', () => {
    assert.equal(parseTerminalLineCropDirective('crop-terminal-lines: 3'), 3);
    assert.equal(parseTerminalLineCropDirective('crop-terminal-lines:   12'), 12);
    assert.equal(parseTerminalLineCropDirective('crop-bottom: 25%'), null);
    assert.throws(
        () => parseTerminalLineCropDirective('crop-terminal-lines:'),
        /crop-terminal-lines must be a positive integer/,
    );
});

test('crop-terminal-lines parses positive integers only', () => {
    assert.equal(parseTerminalLineCount('3'), 3);
    assert.equal(parseTerminalLineCount(' 12 '), 12);
    for (const value of ['0', '-1', '1.5', 'three', '9007199254740992']) {
        assert.throws(
            () => parseTerminalLineCount(value),
            /crop-terminal-lines must be a positive integer/,
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
            cropTerminalLines: 3,
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
            { bottom: 80, height: 20 },
            { bottom: 100, height: 20 },
            { bottom: 120, height: 20 },
            { bottom: 140, height: 20 },
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

test('terminal crop clamps to the available rendered rows and screen bottom', () => {
    const crop = terminalLineCropPixels({
        viewportHeight: 400,
        deviceScaleFactor: 2,
        panelTop: 10,
        screenBottom: 150,
        rows: [
            { bottom: 130, height: 20 },
            { bottom: 150, height: 20 },
        ],
        lineCount: 10,
    });

    assert.deepEqual(crop, {
        topPixels: 20,
        bottomPixels: 100,
        heightPixels: 280,
        keptRows: 2,
        availableRows: 2,
    });
});
