require 'json'
require 'shellwords'
require_relative 'call_check'

# The harness for JavaScript: the counterpart of PythonCheck and RubyCheck,
# for the code typed into a Kata's card (KataCheck, docs/plan-katas.md) and,
# as check type javascript (docs/aufgaben.md), for a file in the Workspace,
# run there with node as the guest (command), like ruby and python.
#
# The harness runs the code in a vm context of its own with the console
# switched off, so top-level lines that call the function to try it out do
# not disturb, then evaluates each call and each expected value in that
# context and compares them with util.isDeepStrictEqual: arrays and objects
# by their content. It prints one JSON line after RESULT_MARK, as the others
# do. forbid (KataCheck) names functions and methods such as Math.max or
# Array.prototype.sort; they fail when the person's code calls them.
#
# A file in the Workspace may be a module: it gets require, module and
# exports, so it can end with module.exports = { … } for another file that
# runs it (supermarkt/simulation.js). The code of a Kata's card does not.
module JavascriptCheck
    RESULT_MARK = CallCheck::RESULT_MARK

    HARNESS = <<~JS
        const path = require('path');
        const fs = require('fs');
        const util = require('util');
        const vm = require('vm');
        const [file, payload, forbidden, modules] = process.argv.slice(2);
        const cases = JSON.parse(payload);
        const full = path.resolve(file);
        const result = { cases: [] };
        // an array as Ruby and Python show it, [1, 2, 3], so that what came
        // out and what is right read alike in one message; util.inspect would
        // write [ 1, 2, 3 ]
        const inspect = (value, depth) => Array.isArray(value) && depth < 4
            ? '[' + value.map((item) => inspect(item, depth + 1)).join(', ') + ']'
            : util.inspect(value, { depth: 4 - depth, breakLength: Infinity });
        const short = (value) => {
            let shown;
            try { shown = inspect(value, 0); } catch (e) { shown = String(value); }
            return shown.length <= 200 ? shown : shown.slice(0, 200) + '…';
        };
        const text = (error) => ((error && error.name ? error.name : 'Error') + ': ' +
            (error && error.message !== undefined ? error.message : String(error))).split('\\n')[0].slice(0, 300);
        const lineOf = (error) => {
            for (const line of String((error && error.stack) || '').split('\\n')) {
                const at = line.indexOf(full + ':');
                if (at >= 0) return Number(line.slice(at + full.length + 1).split(':')[0]) || null;
            }
            return null;
        };
        const quiet = () => {};
        const globals = { console: { log: quiet, info: quiet, warn: quiet, error: quiet } };
        if (modules === 'modules') {
            const own = { exports: {} };
            Object.assign(globals, { require: require('module').createRequire(full), module: own, exports: own.exports });
        }
        const context = vm.createContext(globals);
        for (const name of JSON.parse(forbidden || '[]')) {
            const parts = name.split('.');
            const key = parts.pop();
            const owner = vm.runInContext(parts.join('.'), context);
            const original = owner[key];
            owner[key] = function (...args) {
                const caller = String(new Error().stack).split('\\n')[2] || '';
                if (caller.includes(full)) {
                    const error = new Error(name);
                    error.name = '#{'KataForbidden'}';
                    throw error;
                }
                return original.apply(this, args);
            };
        }
        const run = (source) => vm.runInContext(source, context, { timeout: 30000 });
        // several steps divided by ; (const a = new Account(5); a.deposit(3); a.balance())
        // in a block of their own, so that two cases may both say const a; the block
        // gives the value of its last step
        const call = (source) => run(/;\\s*\\S/.test(source) ? '{' + source + '\\n}' : source);
        (() => {
            let source;
            try {
                source = fs.readFileSync(full, 'utf8');
            } catch (error) {
                result.error = { kind: 'missing' };
                return;
            }
            try {
                new vm.Script(source, { filename: full });
            } catch (error) {
                result.error = { kind: 'syntax', line: lineOf(error), message: String(error.message).slice(0, 300) };
                return;
            }
            try {
                vm.runInContext(source, context, { filename: full, timeout: 30000 });
            } catch (error) {
                result.error = { kind: 'load', line: lineOf(error), message: text(error) };
                return;
            }
            for (const c of cases) {
                const entry = { call: c.call, expect: c.expect };
                if (c.raises) {
                    try {
                        entry.got = short(call(c.call));
                        entry.ok = false;
                    } catch (error) {
                        entry.ok = true;
                        entry.error = text(error);
                    }
                    result.cases.push(entry);
                    continue;
                }
                try {
                    const expected = run('(' + c.expect + ')');
                    if (c.show) entry.shown = short(expected);
                    const got = call(c.call);
                    entry.got = short(got);
                    entry.ok = util.isDeepStrictEqual(got, expected);
                } catch (error) {
                    entry.ok = false;
                    entry.error = text(error);
                    entry.line = lineOf(error);
                }
                result.cases.push(entry);
            }
        })();
        process.stdout.write('\\n#{RESULT_MARK}' + JSON.stringify(result) + '\\n');
    JS

    # The cases of a check as [{ 'call' =>, 'expect' => (JavaScript), 'say' => }].
    def self.cases(check)
        CallCheck.cases(check, method(:javascript_value))
    end

    def self.javascript_value(value)
        case value
        when true then 'true'
        when false then 'false'
        when nil then 'null'
        else value.to_s
        end
    end

    # The harness as a list of arguments (KataCheck runs it as a file).
    # modules: the file may use require and module.exports (a Workspace file).
    def self.argv(file, cases, forbid = [], modules: false)
        payload = cases.map { |c| { 'call' => c['call'], 'expect' => c['expect'], 'raises' => c['raises'], 'show' => c['show'] }.compact }
        ['node', '-e', HARNESS, file, JSON.generate(payload), JSON.generate(forbid), *('modules' if modules)]
    end

    # The script for a file in the Workspace, run in /workspace as the guest:
    # the harness goes into a file of its own, since node -e would take the
    # file for the script's first argument.
    def self.command(file, cases)
        _, _, harness, *arguments = argv(file, cases, modules: true)
        <<~SH
            t=$(mktemp -d) || exit 1
            trap 'rm -rf "$t"' EXIT
            cat > "$t/harness.js" <<'__HS_HARNESS__'
            #{harness.chomp}
            __HS_HARNESS__
            node "$t/harness.js" #{Shellwords.join(arguments)}
        SH
    end

    def self.spec_errors(check)
        CallCheck.spec_errors(check, '.js')
    end

    # [status, message, read] for the harness output.
    def self.evaluate(task, check, output, timed_out: false)
        CallCheck.evaluate(check, cases(check), output, language: 'JavaScript', timed_out: timed_out)
    end
end
