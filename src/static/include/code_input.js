// SQL and Cypher answers, and the code of a Kata (Ruby, Python), in the
// colours of the page's code blocks. The
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

    // Ruby and Python for the code of a Kata (KataCheck), with the token
    // classes of Rouge's lexers, so they look like the page's code blocks:
    // keywords k, true/false/nil kc, function names after def nf, built-ins
    // nb, strings s1/s2, numbers mi/mf, comments c1, symbols ss, instance
    // variables vi, constants and classes nc.
    const RUBY_KEYWORDS = new Set(('alias and begin break case class def defined? do else elsif end ensure for if in '
        + 'module next not or redo rescue retry return self super then undef unless until when while yield').split(' '));
    const RUBY_CONSTANTS = new Set(['true', 'false', 'nil']);
    const RUBY_BUILTINS = new Set('puts print p gets require require_relative attr_reader attr_writer attr_accessor raise loop lambda proc'.split(' '));
    const PYTHON_KEYWORDS = new Set(('as assert async await break class continue def del elif else except finally for '
        + 'from global if import lambda nonlocal pass raise return try while with yield').split(' '));
    const PYTHON_WORDS = new Set(['and', 'or', 'not', 'in', 'is']);
    const PYTHON_CONSTANTS = new Set(['True', 'False', 'None']);
    const PYTHON_BUILTINS = new Set(('abs all any bool dict enumerate filter float input int isinstance len list map max '
        + 'min open print range repr reversed round set sorted str sum tuple type zip').split(' '));

    // After def the next word is the function's name.
    function named(set, constants, builtins, words) {
        let afterDef = false;
        return (w) => {
            if (afterDef) { afterDef = false; return 'nf'; }
            if (w === 'def') { afterDef = true; return 'k'; }
            if (constants.has(w)) return 'kc';
            if (words && words.has(w)) return 'ow';
            if (set.has(w)) return 'k';
            if (builtins.has(w)) return 'nb';
            return /^[A-Z]/.test(w) ? 'nc' : 'n';
        };
    }

    function ruby(text) {
        return scan(text, [
            [/\s+/y, null],
            [/#.*/y, 'c1'],
            [/"(?:\\[\s\S]|[^"\\])*"?/y, 's2'],
            [/'(?:\\[\s\S]|[^'\\])*'?/y, 's1'],
            [/:[A-Za-z_]\w*[?!]?/y, 'ss'],
            [/@@?[A-Za-z_]\w*/y, 'vi'],
            [/\d[\d_]*\.\d+/y, 'mf'],
            [/\d[\d_]*/y, 'mi'],
            [/[A-Za-z_]\w*[?!]?/y, named(RUBY_KEYWORDS, RUBY_CONSTANTS, RUBY_BUILTINS)],
            [/[+*/<>=~!&|%^-]+/y, 'o'],
            [/[;:()[\]{},.]/y, 'p'],
            [/[\s\S]/y, null],
        ]);
    }

    function python(text) {
        return scan(text, [
            [/\s+/y, null],
            [/#.*/y, 'c1'],
            [/[rRbBfF]{0,2}"""[\s\S]*?(?:"""|$)/y, 's2'],
            [/[rRbBfF]{0,2}"(?:\\[\s\S]|[^"\\\n])*"?/y, 's2'],
            [/[rRbBfF]{0,2}'(?:\\[\s\S]|[^'\\\n])*'?/y, 's1'],
            [/@[A-Za-z_]\w*/y, 'nd'],
            [/\d+\.\d*|\.\d+/y, 'mf'],
            [/\d+/y, 'mi'],
            [/[A-Za-z_]\w*/y, named(PYTHON_KEYWORDS, PYTHON_CONSTANTS, PYTHON_BUILTINS, PYTHON_WORDS)],
            [/[+*/<>=~!&|%^@-]+/y, 'o'],
            [/[;:()[\]{},.]/y, 'p'],
            [/[\s\S]/y, null],
        ]);
    }

    // JavaScript, as Rouge's lexer: declarations kd (function, const, let),
    // keywords k, true/false/null/undefined kc, built-ins nb, names nx, the
    // name after function and a method called after a dot nf.
    const JS_DECLARATIONS = new Set(['function', 'const', 'let', 'var', 'class']);
    const JS_KEYWORDS = new Set(('if else for while do return break continue new of in typeof instanceof switch case '
        + 'default throw try catch finally this async await yield delete void extends super').split(' '));
    const JS_CONSTANTS = new Set(['true', 'false', 'null', 'undefined', 'NaN', 'Infinity']);
    const JS_BUILTINS = new Set('Math Array Object String Number Boolean JSON Date Map Set Promise parseInt parseFloat isNaN'.split(' '));

    function javascript(text) {
        let afterFunction = false;
        let afterDot = false;
        const plain = (token) => () => { afterDot = false; return token; };
        return scan(text, [
            [/\s+/y, null],
            [/\/\/.*/y, plain('c1')],
            [/\/\*[\s\S]*?(?:\*\/|$)/y, plain('cm')],
            [/`(?:\\[\s\S]|[^`\\])*`?/y, plain('s2')],
            [/"(?:\\[\s\S]|[^"\\\n])*"?/y, plain('s2')],
            [/'(?:\\[\s\S]|[^'\\\n])*'?/y, plain('s1')],
            [/\d+\.\d+/y, plain('mf')],
            [/\d+/y, plain('mi')],
            [/[A-Za-z_$][\w$]*(?=\s*\()/y, (w) => {
                const token = afterFunction || afterDot ? 'nf' : JS_KEYWORDS.has(w) ? 'k' : JS_BUILTINS.has(w) ? 'nb' : 'nx';
                afterFunction = JS_DECLARATIONS.has(w) && w === 'function';
                afterDot = false;
                return JS_DECLARATIONS.has(w) ? 'kd' : token;
            }],
            [/[A-Za-z_$][\w$]*/y, (w) => {
                const token = afterFunction ? 'nf' : JS_DECLARATIONS.has(w) ? 'kd' : JS_KEYWORDS.has(w) ? 'k'
                    : JS_CONSTANTS.has(w) ? 'kc' : JS_BUILTINS.has(w) ? 'nb' : 'nx';
                afterFunction = w === 'function';
                afterDot = false;
                return token;
            }],
            [/\./y, () => { afterDot = true; return 'p'; }],
            [/[+*/<>=~!&|%^?-]+/y, plain('o')],
            [/[;:()[\]{},]/y, plain('p')],
            [/[\s\S]/y, null],
        ]);
    }

    const HIGHLIGHT = { sql, cypher, ruby, python, javascript };
    const INDENT = { python: '    ', ruby: '  ', javascript: '  ' };

    // Writing code: Tab indents (the selected lines, or at the cursor),
    // Shift+Tab takes it back, Enter keeps the line's indentation and adds
    // one after a line that opens a block. Escape lets the next Tab leave the
    // field, as everywhere else on the page. Edits go through insertText, so
    // Ctrl+Z still undoes them.
    function opensBlock(language, line) {
        if (language === 'javascript') return /[{([]$/.test(line.replace(/\/\/.*$/, '').trimEnd());
        const code = line.replace(/#.*$/, '').trimEnd();
        if (language === 'python') return code.endsWith(':');
        return /^\s*(def|class|module|if|unless|while|until|for|case|when|begin|else|elsif|rescue|ensure)\b/.test(code)
            || /\bdo(\s*\|[^|]*\|)?$/.test(code);
    }

    function insert(textarea, text, from, to) {
        textarea.setSelectionRange(from, to);
        if (!document.execCommand || !document.execCommand('insertText', false, text)) {
            textarea.setRangeText(text, from, to, 'end');
            textarea.dispatchEvent(new Event('input'));
        }
    }

    // A line that closes a block or starts its next part goes back to the
    // indentation of the line that opened it, as soon as it is typed:
    // Ruby's end, else, elsif, when, rescue, ensure; Python's else, elif,
    // except, finally with their colon; JavaScript's closing brace.
    const CLOSES = {
        ruby: /^(end|else|elsif|when|rescue|ensure)$/,
        python: /^(else|elif\b.*|except\b.*|finally):$/,
        javascript: /^\}$/,
    };

    function outdent(textarea, language) {
        const value = textarea.value;
        const caret = textarea.selectionStart;
        const first = value.lastIndexOf('\n', caret - 1) + 1;
        const lineEnd = value.indexOf('\n', caret) < 0 ? value.length : value.indexOf('\n', caret);
        if (caret !== lineEnd || caret !== textarea.selectionEnd) return;
        const line = value.slice(first, lineEnd);
        const indent = line.match(/^[ \t]*/)[0];
        if (!indent || !CLOSES[language].test(line.trim())) return;
        // the opening line: the nearest line above with less indentation
        const above = value.slice(0, first).split('\n').reverse();
        const opener = above.find((l) => l.trim() !== '' && l.match(/^[ \t]*/)[0].length < indent.length);
        if (opener === undefined) return;
        const target = opener.match(/^[ \t]*/)[0];
        insert(textarea, target, first, first + indent.length);
        const end = first + target.length + line.length - indent.length;
        textarea.setSelectionRange(end, end);
    }

    function codeKeys(textarea, language) {
        const unit = INDENT[language];
        let leaving = false;
        textarea.addEventListener('input', (e) => {
            if (e.inputType === 'insertText' && e.data && CLOSES[language]) outdent(textarea, language);
        });
        textarea.addEventListener('keydown', (e) => {
            // a plan to read (pip_world.js, pair): no keys that write
            if (textarea.readOnly) return;
            if (e.key === 'Escape') { leaving = true; return; }
            if (e.key === 'Tab' && !leaving && !e.ctrlKey && !e.altKey && !e.metaKey) {
                e.preventDefault();
                const value = textarea.value;
                const start = textarea.selectionStart;
                const end = textarea.selectionEnd;
                const first = value.lastIndexOf('\n', start - 1) + 1;
                if (!e.shiftKey && start === end) {
                    insert(textarea, unit, start, end);
                    return;
                }
                const last = end > start && value[end - 1] === '\n' ? end - 1 : end;
                const lines = value.slice(first, last).split('\n');
                const changed = lines.map((line) => e.shiftKey
                    ? line.replace(new RegExp('^ {1,' + unit.length + '}'), '')
                    : unit + line).join('\n');
                insert(textarea, changed, first, last);
                textarea.setSelectionRange(first, first + changed.length);
                return;
            }
            leaving = false;
            if (e.key === 'Enter' && !e.ctrlKey && !e.metaKey && !e.shiftKey && !e.altKey) {
                const value = textarea.value;
                const start = textarea.selectionStart;
                const line = value.slice(value.lastIndexOf('\n', start - 1) + 1, start);
                const indent = line.match(/^[ \t]*/)[0] + (opensBlock(language, line) ? unit : '');
                e.preventDefault();
                insert(textarea, '\n' + indent, start, textarea.selectionEnd);
            }
        });
    }

    function attach(textarea) {
        if (textarea.codeInputUpdate) return;
        const language = textarea.dataset.codeLanguage || (textarea.classList.contains('task-cypher') ? 'cypher' : 'sql');
        const box = document.createElement('div');
        box.className = 'code-input';
        const view = document.createElement('pre');
        view.className = 'code-input-view' + (language === 'cypher' ? ' code-cypher' : '');
        view.setAttribute('aria-hidden', 'true');
        textarea.parentNode.insertBefore(box, textarea);
        box.appendChild(view);
        box.appendChild(textarea);
        textarea.classList.add('code-input-text');
        // A Kata's code has line numbers (messages and Pip's replay name
        // lines). A long line wraps, so each number is as high as its line:
        // an invisible copy of the text, one element per line, measures them.
        const numbered = textarea.classList.contains('task-code');
        const gutter = document.createElement('div');
        const mirror = document.createElement('div');
        if (numbered) {
            box.classList.add('code-input-numbered');
            gutter.className = 'code-input-gutter';
            mirror.className = 'code-input-mirror';
            gutter.setAttribute('aria-hidden', 'true');
            mirror.setAttribute('aria-hidden', 'true');
            box.insertBefore(mirror, view);
            box.insertBefore(gutter, view);
        }
        function number() {
            if (!numbered) return;
            const lines = textarea.value.split('\n');
            mirror.replaceChildren(...lines.map((line) => Object.assign(document.createElement('div'), { textContent: line || '\u200b' })));
            const heights = [...mirror.children].map((line) => line.getBoundingClientRect().height);
            const current = gutter.querySelector('.current');
            const at = current ? [...gutter.children].indexOf(current) : -1;
            gutter.replaceChildren(...heights.map((height, i) => {
                const n = Object.assign(document.createElement('div'), { textContent: i + 1 });
                n.style.height = height + 'px';
                if (i === at) n.className = 'current';
                return n;
            }));
        }

        function grow() {
            const style = getComputedStyle(textarea);
            textarea.style.height = 'auto';
            textarea.style.height = (textarea.scrollHeight
                + parseFloat(style.borderTopWidth) + parseFloat(style.borderBottomWidth)) + 'px';
        }
        // A final newline would not make a line of its own in the <pre>.
        function update() {
            const text = textarea.value;
            view.innerHTML = (HIGHLIGHT[language] || sql)(text) + '\n';
            grow();
            number();
        }
        textarea.addEventListener('input', update);
        // An editor with a fixed height (Pip's plan beside the map) scrolls:
        // the colours, the numbers and the marked line go along.
        let marked = null;
        textarea.addEventListener('scroll', () => {
            view.scrollTop = textarea.scrollTop;
            gutter.scrollTop = textarea.scrollTop;
            if (marked) place(marked);
        });
        // Another width wraps the lines differently.
        if (window.ResizeObserver) {
            let width = 0;
            new ResizeObserver(() => {
                if (box.clientWidth !== width) { width = box.clientWidth; grow(); number(); }
            }).observe(box);
        }
        if (INDENT[language]) codeKeys(textarea, language);
        textarea.codeInputUpdate = update;
        // The line a run is at (Pip's plan, pip_world.js), as a bar behind the
        // text; null takes it away. Long lines wrap, so the line's place is
        // measured: a marker at its first character, taken out again.
        // where the mark stands, from the line's place in the text and how far
        // the editor is scrolled; out of sight it is hidden
        function place(at) {
            const mark = box.querySelector('.code-input-mark');
            if (!mark) return;
            const top = at.top - textarea.scrollTop;
            mark.style.top = top + 'px';
            mark.hidden = top + at.height <= 0 || top >= textarea.clientHeight;
        }
        textarea.codeInputMark = function (line) {
            let mark = box.querySelector('.code-input-mark');
            [...gutter.children].forEach((n, i) => n.classList.toggle('current', i + 1 === line));
            if (!line) {
                marked = null;
                if (mark) mark.hidden = true;
                return;
            }
            if (!mark) {
                mark = document.createElement('div');
                mark.className = 'code-input-mark';
                box.insertBefore(mark, textarea);
            }
            const walker = document.createTreeWalker(view, NodeFilter.SHOW_TEXT);
            let at = null;
            let seen = 1;
            let node;
            while (!at && (node = walker.nextNode())) {
                if (seen === line) { at = [node, 0]; break; }
                const text = node.nodeValue;
                for (let i = 0; i < text.length; i++) {
                    if (text[i] === '\n' && ++seen === line) { at = [node, i + 1]; break; }
                }
            }
            if (!at) { mark.hidden = true; return; }
            const rest = at[1] < at[0].nodeValue.length ? at[0].splitText(at[1]) : null;
            const probe = document.createElement('span');
            at[0].parentNode.insertBefore(probe, rest || at[0].nextSibling);
            const top = probe.offsetTop;
            probe.remove();
            view.normalize();
            const style = getComputedStyle(view);
            const height = parseFloat(style.lineHeight);
            mark.style.height = height + 'px';
            // a run goes on below or above what is in view: the editor
            // follows, so that the line stays a third from the top
            if (textarea.scrollHeight > textarea.clientHeight &&
                (top < textarea.scrollTop || top + height > textarea.scrollTop + textarea.clientHeight)) {
                textarea.scrollTop = Math.max(0, top - textarea.clientHeight / 3);
            }
            marked = { top, height };
            place(marked);
        };
        update();
    }

    window.CodeInput = { attach, sql, cypher, ruby, python, javascript };
})();
