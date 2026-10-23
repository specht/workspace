import test from 'node:test';
import assert from 'node:assert/strict';
import {
    dependsOnPage,
    expandIncludedRecipes,
    fillRecipeVariables,
    pageMeta,
    partText,
    sourceLine,
} from '../includes.js';

const REFERENCE = `# Workspace

<!-- part: klonen -->
### Klonen

\`\`\`bash_copy
\${template}
\`\`\`

<!-- tutorial-screenshot
clone-start: \${template:-https://github.com/specht/anaglyph.git @ ec0807}
-->

<img class='full' src='git-clone.webp' alt='Die Adresse.'>
<!-- /part -->

<!-- part: fixed -->
<!-- tutorial-screenshot
terminal-open
-->

<img src='terminal.webp' alt='Ein Terminal.'>
<!-- /part -->
`;

const PAGE = `<div class='meta'>
template: https://github.com/specht/backrooms-starter.git
</div>

# Die Backrooms

_include(workspace#klonen)

<!-- tutorial-screenshot
clone-start: https://github.com/specht/backrooms-starter.git @ f19e4c
-->

<img src='vorlage.webp' alt='Die Vorlage.'>
`;

const readReference = slug => (slug === 'workspace' ? { markdown: REFERENCE, path: 'reference/workspace/workspace.md' } : null);

test('pageMeta reads the meta block', () => {
    assert.deepEqual(pageMeta(PAGE), { template: 'https://github.com/specht/backrooms-starter.git' });
    assert.deepEqual(pageMeta('# Ohne'), {});
});

test('partText finds one part', () => {
    assert.match(partText(REFERENCE, 'klonen'), /^### Klonen/);
    assert.match(partText(REFERENCE, 'fixed'), /terminal-open/);
    assert.equal(partText(REFERENCE, 'missing'), null);
});

test('only a part whose recipes use ${template} depends on the page', () => {
    assert.equal(dependsOnPage(partText(REFERENCE, 'klonen')), true);
    assert.equal(dependsOnPage(partText(REFERENCE, 'fixed')), false);
    // ${template} in a code block, not in a recipe, does not count
    assert.equal(dependsOnPage('```bash\n${template}\n```'), false);
});

test('on the Handbuch page itself the fallback is used', () => {
    const filled = fillRecipeVariables(REFERENCE, pageMeta(REFERENCE));
    assert.match(filled, /^clone-start: https:\/\/github\.com\/specht\/anaglyph\.git @ ec0807$/m);
    // outside recipes nothing changes
    assert.match(filled, /^\$\{template\}$/m);
});

test('a page fills in its repository with the commit of its own clone', () => {
    const { markdown } = expandIncludedRecipes(PAGE, 'level-0/level-0.md', readReference);
    assert.match(markdown, /^clone-start: https:\/\/github\.com\/specht\/backrooms-starter\.git @ f19e4c$/m);
    assert.doesNotMatch(markdown, /anaglyph/);
    assert.match(markdown, /src='included-workspace-git-clone\.webp'/);
    assert.match(markdown, /^<!-- tutorial-scope included -->$/m);
    assert.match(markdown, /^<!-- \/tutorial-scope -->$/m);
    // the page's own recipe is untouched
    assert.match(markdown, /src='vorlage\.webp'/);
});

test('without a pinned clone of its own the page clones its repository unpinned', () => {
    const page = PAGE.replace(/ @ f19e4c/, '');
    const { markdown } = expandIncludedRecipes(page, 'level-0/level-0.md', readReference);
    assert.match(markdown, /^clone-start: https:\/\/github\.com\/specht\/backrooms-starter\.git$/m);
});

test('a part that does not depend on the page stays an include', () => {
    const page = PAGE.replace('workspace#klonen', 'workspace#fixed');
    const expansion = expandIncludedRecipes(page, 'p/p.md', readReference);
    assert.equal(expansion.markdown, page);
    assert.deepEqual(expansion.regions, []);
});

test('lines of the expanded page lead back to where they were written', () => {
    const expansion = expandIncludedRecipes(PAGE, 'level-0/level-0.md', readReference);
    const lines = expansion.markdown.split('\n');
    const where = text => sourceLine(expansion, lines.indexOf(text) + 1);
    // before the include
    assert.deepEqual(where('# Die Backrooms'), { path: 'level-0/level-0.md', line: 5 });
    // the scope markers stand for the _include line
    assert.deepEqual(where('<!-- tutorial-scope included -->'), { path: 'level-0/level-0.md', line: 7 });
    assert.deepEqual(where('<!-- /tutorial-scope -->'), { path: 'level-0/level-0.md', line: 7 });
    // inside the part: the Handbuch page
    assert.deepEqual(where('### Klonen'), { path: 'reference/workspace/workspace.md', line: 4 });
    assert.deepEqual(where("<img class='full' src='included-workspace-git-clone.webp' alt='Die Adresse.'>"),
        { path: 'reference/workspace/workspace.md', line: 14 });
    // after it: the page again
    assert.deepEqual(where("<img src='vorlage.webp' alt='Die Vorlage.'>"), { path: 'level-0/level-0.md', line: 13 });
});
