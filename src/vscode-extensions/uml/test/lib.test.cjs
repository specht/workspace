const test = require('node:test');
const assert = require('node:assert/strict');
const lib = require('../lib.js');
const core = require('../core/uml-core.js');

test('the svg goes next to the file', () => {
    assert.equal(lib.svgPath('/workspace/oop/bibliothek.uml'), '/workspace/oop/bibliothek.svg');
    assert.equal(lib.svgPath('/workspace/oop/Zoo.UML'), '/workspace/oop/Zoo.svg');
});

test('only a diagram the UML-Werkstatt wrote is overwritten without asking', () => {
    assert.ok(lib.isGenerated(core.diagram('class Book\n', { background: '#ffffff' }).svg));
    assert.ok(!lib.isGenerated('<svg xmlns="http://www.w3.org/2000/svg"><circle r="3"/></svg>'));
    assert.ok(!lib.isGenerated(''));
    assert.ok(!lib.isGenerated(null));
});

test('problem ranges stay inside the document', () => {
    const lengths = [10, 0, 20];
    const len = (i) => lengths[i];
    assert.deepEqual(lib.problemRange({ line: 1, from: 2, to: 5 }, 3, len), { line: 0, from: 2, to: 5 });
    assert.deepEqual(lib.problemRange({ line: 9, from: 2, to: 50 }, 3, len), { line: 2, from: 2, to: 20 });
    assert.deepEqual(lib.problemRange({ line: 2, from: 4, to: 8 }, 3, len), { line: 1, from: 0, to: 1 });
    // the missing file of the command line: line 0
    assert.deepEqual(lib.problemRange({ line: 0, msg: 'x' }, 3, len), { line: 0, from: 0, to: 10 });
});
