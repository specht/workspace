// SQL and Cypher answers in the colours of the page's code blocks. The
// textarea's own text is transparent; a <pre> underneath shows the same text
// with Rouge's token classes, so the gruvbox theme (scope pre) and the
// ```cypher colours (pre.code-cypher) apply as they are. The field grows with
// its text, so it never scrolls away from the <pre>.
//
// The two tokenizers follow Rouge::Lexers::SQL and
// Rouge::Lexers::WorkspaceCypher (src/ruby/include/cypher_lexer.rb); their
// keyword lists come from there (window.CODE_INPUT_KEYWORDS, set by
// task_page_setup). One difference: an unclosed string is coloured to the
// end, so a missing quote shows while typing.
(function () {
    const KEYWORDS = window.CODE_INPUT_KEYWORDS || {};
    const SQL_KEYWORDS = new Set(KEYWORDS.sql || []);
    const SQL_TYPES = new Set(KEYWORDS.sqlTypes || []);
    const CYPHER_KEYWORDS = new Set(KEYWORDS.cypher || []);

    function escape(text) {
        return text.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');
    }

    function span(token, text) {
        return token ? `<span class="${token}">${escape(text)}</span>` : escape(text);
    }

    // rules: [regex (sticky), token or function(match) -> token]; the first
    // that matches at the position wins.
    function scan(text, rules) {
        let out = '';
        let i = 0;
        while (i < text.length) {
            for (const [re, token] of rules) {
                re.lastIndex = i;
                const m = re.exec(text);
                if (!m || m[0].length === 0) continue;
                out += span(typeof token === 'function' ? token(m[0]) : token, m[0]);
                i += m[0].length;
                break;
            }
        }
        return out;
    }

    const SQL_RULES = [
        [/\s+/y, null],
        [/--.*/y, 'c1'],
        [/\/\*[\s\S]*?(?:\*\/|$)/y, 'cm'],
        [/\d+/y, 'mi'],
        [/'(?:\\[\s\S]|''|[^\\'])*'?/y, 's1'],
        [/"(?:\\[\s\S]|""|[^\\"])*"?/y, 'nv'],
        [/`(?:\\[\s\S]|``|[^\\`])*`?/y, 'nv'],
        [/\w+/y, (w) => SQL_TYPES.has(w.toUpperCase()) ? 'nb' : SQL_KEYWORDS.has(w.toUpperCase()) ? 'k' : 'n'],
        [/[+*/<>=~!@#%&|?^-]/y, 'o'],
        [/[;:()[\]{},.]/y, 'p'],
        [/[\s\S]/y, null],
    ];

    function sql(text) {
        return scan(text, SQL_RULES);
    }

    // Cypher: labels (nc), relationship types (nf) and property keys (na)
    // depend on where they stand, so this one keeps a stack of states like
    // the Rouge lexer: root, node (…), relationship […], map {…},
    // arguments of a function (…).
    const IDENT = '(?:[\\p{L}_][\\p{L}\\p{N}_]*|`[^`]+`)';
    const sticky = (source) => new RegExp(source, 'uy');

    const EXPRESSION = [
        [sticky('//.*'), () => [['c1']]],
        [sticky("'(?:\\\\[\\s\\S]|[^'\\\\])*'?"), () => [['s1']]],
        [sticky('"(?:\\\\[\\s\\S]|[^"\\\\])*"?'), () => [['s2']]],
        [sticky('\\d+(?:\\.\\d+)?(?:[eE][+-]?\\d+)?'), () => [['m']]],
        [sticky(`(\\.)(${IDENT})`), () => [['p'], ['na']]],
        [sticky(`(${IDENT})([ \\t]*)(\\()`), (m) => CYPHER_KEYWORDS.has(m[1].toLowerCase())
            ? [['k'], [null], ['p', 'node']]
            : [['nb'], [null], ['p', 'arguments']]],
        [sticky(IDENT), (m) => [[CYPHER_KEYWORDS.has(m[0].toLowerCase()) ? 'k' : 'n']]],
        [sticky('->|<-|<>|<=|>=|=~|\\.\\.|[-+*/%=<>|^]'), () => [['o']]],
        [sticky('[,;]'), () => [['p']]],
        [sticky('\\s+'), () => [[null]]],
    ];
    const OPEN = [
        [sticky('\\('), () => [['p', 'node']]],
        [sticky('\\['), () => [['p', 'relationship']]],
        [sticky('\\{'), () => [['p', 'map']]],
    ];
    const ANY = [[sticky('[\\s\\S]'), () => [[null]]]];
    const POP = (close) => [[sticky(close), () => [['p', 'pop']]]];

    // Each rule gives one [token, push] per group (or the whole match).
    const CYPHER_STATES = {
        root: [...OPEN, [sticky('[)\\]}]'), () => [['p']]], ...EXPRESSION, ...ANY],
        node: [...POP('\\)'),
            [sticky(`(:)([ \\t]*)(${IDENT})`), () => [['p'], [null], ['nc']]],
            ...OPEN, ...EXPRESSION, ...ANY],
        relationship: [...POP('\\]'),
            [sticky(`(:|\\|:?)([ \\t]*)(${IDENT})`), (m) => [[m[1] === ':' ? 'p' : 'o'], [null], ['nf']]],
            [sticky('\\{'), () => [['p', 'map']]],
            ...EXPRESSION, ...ANY],
        map: [...POP('\\}'),
            [sticky(`(${IDENT})([ \\t]*)(:)`), () => [['na'], [null], ['p']]],
            ...OPEN, ...EXPRESSION, ...ANY],
        arguments: [...POP('\\)'), ...OPEN, ...EXPRESSION, ...ANY],
    };

    function cypher(text) {
        const stack = ['root'];
        let out = '';
        let i = 0;
        while (i < text.length) {
            for (const [re, tokens] of CYPHER_STATES[stack[stack.length - 1]]) {
                re.lastIndex = i;
                const m = re.exec(text);
                if (!m || m[0].length === 0) continue;
                const parts = tokens(m);
                const pieces = m.length > 1 ? m.slice(1) : [m[0]];
                parts.forEach(([token, push], k) => {
                    out += span(token, pieces[k] || '');
                    if (push === 'pop') { if (stack.length > 1) stack.pop(); }
                    else if (push) stack.push(push);
                });
                i += m[0].length;
                break;
            }
        }
        return out;
    }

    function attach(textarea) {
        if (textarea.codeInputUpdate) return;
        const language = textarea.classList.contains('task-cypher') ? 'cypher' : 'sql';
        const box = document.createElement('div');
        box.className = 'code-input';
        const view = document.createElement('pre');
        view.className = 'code-input-view' + (language === 'cypher' ? ' code-cypher' : '');
        view.setAttribute('aria-hidden', 'true');
        textarea.parentNode.insertBefore(box, textarea);
        box.appendChild(view);
        box.appendChild(textarea);
        textarea.classList.add('code-input-text');

        function grow() {
            const style = getComputedStyle(textarea);
            textarea.style.height = 'auto';
            textarea.style.height = (textarea.scrollHeight
                + parseFloat(style.borderTopWidth) + parseFloat(style.borderBottomWidth)) + 'px';
        }
        // A final newline would not make a line of its own in the <pre>.
        function update() {
            const text = textarea.value;
            view.innerHTML = (language === 'cypher' ? cypher(text) : sql(text)) + '\n';
            grow();
        }
        textarea.addEventListener('input', update);
        textarea.addEventListener('scroll', () => { view.scrollTop = textarea.scrollTop; });
        // Another width wraps the lines differently.
        if (window.ResizeObserver) {
            let width = 0;
            new ResizeObserver(() => {
                if (box.clientWidth !== width) { width = box.clientWidth; grow(); }
            }).observe(box);
        }
        textarea.codeInputUpdate = update;
        update();
    }

    window.CodeInput = { attach, sql, cypher };
})();
