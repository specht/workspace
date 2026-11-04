#!/usr/bin/env ruby
# Compares the compile service (HotCompile, docs/katas.md, »Java and Dart
# compiled hot«) with compiling in each run: the examples of the Java and
# Dart Katas, Pip and Tests schreiben (task-examples.yaml, and each card's
# automatic examples), plus one version per card that does not compile, each
# checked twice in real Kata containers, cold and hot; for function Katas
# also »Ausführen« (the solution with a main, the broken version, a class of
# its own). Says where verdict, message or output differ, and how long each
# took.
#
#   ./config.rb exec ruby ruby /src/scripts/compare-hot-compile.rb [ACTIVITY …] [--tasks N] [--language java|dart]
#
# Without activities: every Kata set, Pip and Tests schreiben. --tasks N
# takes the first N cards of each (all by default), --language only one of
# the two languages (both by default).
require 'yaml'
require_relative '../ruby/include/task_examples'
require_relative '../ruby/include/hot_compile'

content = File.expand_path('../content', __dir__)
limit = nil
languages = %w[java dart]
names = []
args = ARGV.dup
until args.empty?
    a = args.shift
    case a
    when '--tasks' then limit = args.shift.to_i
    when '--language' then languages = [args.shift]
    else names << a
    end
end
names = Dir[File.join(content, '{kata-*,pip-*}', 'tasks.yaml')].map { |f| File.basename(File.dirname(f)) }.sort if names.empty?

run = lambda do |files:, argv:, timeout:, dialog: nil|
    KataRunner.run(:files => files, :argv => argv, :timeout => timeout, :dialog => dialog)
end

# both services ready before the first comparison
HotCompile.enabled = true
languages.each do |language|
    started = Time.now
    HotCompile.server(language).start
    puts "#{language} service ready after #{(Time.now - started).round(1)} s"
end

times = Hash.new { |h, k| h[k] = { :cold => [], :hot => [] } }
differences = []
count = 0
names.each do |activity|
    dir = File.join(content, activity)
    tasks = TaskSpec.load_file(File.join(dir, 'tasks.yaml'))['tasks']
    tasks = tasks.first(limit) if limit
    path = File.join(dir, TaskExamples::FILE)
    examples = File.exist?(path) ? (YAML.safe_load(File.read(path))['examples'] || {}) : {}
    tasks.each do |task|
        languages.each do |language|
            next unless task[language].is_a?(Hash) && task[language].key?('check')

            variant = CodeLanguages.variant(task, language)
            next unless %w[function pip tests].include?(variant['check']['type'])

            given = (examples[task['id']] || {})[language] || {}
            cases = TaskExamples.automatic_cases(variant)
            %w[pass fail].each { |kind| (given[kind] || []).each { |e| cases << [kind, e] } }
            starter = variant['starter'].to_s
            cases << ['broken', { 'answer' => "#{starter}\nthis is not #{language} at all (\n" }]
            # »Ausführen«: the solution with a main, the broken version, and
            # a class of its own as the launcher allows it
            if variant['check']['type'] == 'function'
                main = language == 'java' ? "void main() {\n    IO.println(\"hallo\");\n}\n" : "void main() {\n  print('hallo');\n}\n"
                own = language == 'java' ? "public class Main {\n    public static void main(String[] a) { System.out.println(42); }\n}\n" : main
                solution = variant['solution'].to_s.gsub(/```\w*\n?/, '')
                [['run solution', "#{solution}\n#{main}"], ['run broken', cases.last[1]['answer']], ['run own class', own]].each do |kind, answer|
                    results = {}
                    [false, true].each do |hot|
                        HotCompile.enabled = hot
                        started = Time.now
                        r = KataCheck.run_program(language, answer, run)
                        times["#{language} run"][hot ? :hot : :cold] << Time.now - started
                        results[hot] = [r[:status], r[:output].to_s, r[:message].to_s]
                    end
                    count += 1
                    label = "#{activity}/#{task['id']} (#{language}) #{kind}"
                    if results[false] != results[true]
                        differences << "#{label}\n  cold: #{results[false].inspect[0, 600]}\n  hot:  #{results[true].inspect[0, 600]}"
                        puts "DIFFERENT #{label}"
                    else
                        puts "same      #{label}: #{results[true][0]}"
                    end
                end
            end
            cases.each do |kind, example|
                answer = example['answer'].to_s
                next if answer.strip.empty?

                results = {}
                [false, true].each do |hot|
                    HotCompile.enabled = hot
                    started = Time.now
                    result = TaskCheck.check(variant, answer, :kata => run, :variables => example['variables'] || {})
                    times[language][hot ? :hot : :cold] << Time.now - started
                    results[hot] = [result.status, result.message.to_s]
                end
                count += 1
                label = "#{activity}/#{task['id']} (#{language}) #{kind}"
                if results[false] != results[true]
                    differences << "#{label}\n  cold: #{results[false].inspect[0, 600]}\n  hot:  #{results[true].inspect[0, 600]}"
                    puts "DIFFERENT #{label}"
                else
                    puts "same      #{label}: #{results[true][0]}"
                end
            end
        end
    end
end

puts
puts "#{count} examples, #{differences.size} with a different verdict or message"
times.each do |language, t|
    avg = ->(list) { list.empty? ? 0 : (list.sum / list.size).round(2) }
    puts "#{language}: cold #{avg.(t[:cold])} s, hot #{avg.(t[:hot])} s on average (#{t[:hot].size} checks)"
end
puts differences.join("\n")
HotCompile.stop_all
exit(differences.empty? ? 0 : 1)
