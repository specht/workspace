require 'json'
require_relative 'call_check'

# The harness for a Kata in JavaScript (KataCheck, docs/plan-katas.md): the
# counterpart of PythonCheck and RubyCheck, for the code typed into a card,
# never a file in the Workspace (there is no check type javascript).
#
# The harness runs the code in a vm context of its own with the console
# switched off, so top-level lines that call the function to try it out do
# not disturb, then evaluates each call and each expected value in that
# context and compares them with util.isDeepStrictEqual: arrays and objects
# by their content. It prints one JSON line after RESULT_MARK, as the others
# do. forbid (KataCheck) names functions and methods such as Math.max or
# Array.prototype.sort; they fail when the person's code calls them.
module JavascriptCheck
    RESULT_MARK = CallCheck::RESULT_MARK

    HARNESS = <<~JS
        const path = require('path');
        const fs = require('fs');
        const util = require('util');
        const vm = require('vm');
        const [file, payload, forbidden] = process.argv.slice(2);
        const cases = JSON.parse(payload);
        const full = path.resolve(file);
        const result = { cases: [] };
        const short = (value) => {
            let shown;
            try { shown = util.inspect(value, { depth: 4, breakLength: Infinity }); } catch (e) { shown = String(value); }
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
        const context = vm.createContext({ console: { log: quiet, info: quiet, warn: quiet, error: quiet } });
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
                        entry.got = short(run(c.call));
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
                    const got = run(c.call);
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
    def self.argv(file, cases, forbid = [])
        payload = cases.map { |c| { 'call' => c['call'], 'expect' => c['expect'], 'raises' => c['raises'], 'show' => c['show'] }.compact }
        ['node', '-e', HARNESS, file, JSON.generate(payload), JSON.generate(forbid)]
    end

    # [status, message, read] for the harness output.
    def self.evaluate(task, check, output, timed_out: false)
        CallCheck.evaluate(check, cases(check), output, language: 'JavaScript', timed_out: timed_out)
    end
end
