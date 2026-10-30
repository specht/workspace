require_relative 'code_languages'
# What a teacher sees under an open task (reflection) on the activity page
# (docs/aufgaben.md, »Antworten unter offenen Aufgaben«): the example
# answers from tasks.yaml or the answers of the current group. Plain Ruby,
# so it can be tested without Neo4j; main.rb only fetches the data.
module OpenAnswers
    VIEWS = %w[examples group off].freeze

    # The open tasks of an activity, in page order.
    def self.task_ids(tasks)
        tasks[:on_page].select { |id| (tasks[:by_id][id] || {}).dig('check', 'type') == 'reflection' }
    end

    # The view to open with: the one chosen last if it can be shown,
    # otherwise closed. Nothing opens by itself under the cards, so the page
    # can go on a projector as it is.
    # The language of an example answer that is nothing but one code block
    # in one of the page languages (```ruby … ```), else nil: on a page in
    # Ruby, the example in Python is no help.
    def self.example_language(markdown)
        m = markdown.to_s.strip.match(/\A```([a-z]+)\n.*\n```\z/m)
        return nil unless m && !m[0][3..-4].include?('```')

        language = { 'js' => 'javascript' }.fetch(m[1], m[1])
        CodeLanguages::LANGUAGES.include?(language) ? language : nil
    end

    def self.view(wanted, examples:, groups:)
        possible = VIEWS.select { |v| v == 'off' || (v == 'examples' && examples) || (v == 'group' && groups) }
        possible.include?(wanted) ? wanted : 'off'
    end

    # members: [{ email:, name: }]
    # tasks:   { id => { version: } } of the open tasks
    # states:  { email => { task_id => stored TaskState (symbol keys) } }
    # Returns { task_id => [{ name:, answer:, updated:, stale: }] }, sorted by
    # name; only who has saved an answer.
    def self.collect(members, tasks, states)
        tasks.to_h do |id, task|
            answers = members.sort_by { |m| m[:name].to_s.downcase }.filter_map do |m|
                s = (states[m[:email]] || {})[id]
                next if s.nil? || s[:last_answer].to_s.strip.empty?

                { :name => m[:name], :answer => s[:last_answer].to_s, :updated => s[:updated].to_i,
                  :stale => s[:version] != task[:version] }
            end
            [id, answers]
        end
    end
end
