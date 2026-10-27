require 'cgi'
require 'set'

# The band at the foot of a catalog card, a Kata set or a language tile
# (Main#usage_html), in three kinds:
#
# - admins: how often each task was solved, by everybody but admins,
#   teachers and test accounts; a segment per task, darker the more often,
#   next to the activity's most solved task;
# - a teacher: the same, counted over the students of the groups the teacher
#   teaches, darker the larger the share of them (all of them: darkest);
# - a student: their own tasks, as the pinned card shows them (green done,
#   amber not yet, orange changed since), with »3/14«, on the cards they
#   have begun only, so that the catalog stays calm.
#
# Every segment has a title that says what it counts; the band a label.
# Plain Ruby, tested on its own (card_usage_test.rb).
module CardUsage
    # Five steps of one blue (default.css, .usage-level-1 … -5); 0: never
    def self.level(count, top)
        return 0 if count.to_i <= 0

        [(5.0 * count / [top.to_i, 1].max).ceil, 5].min
    end

    # The students a teacher's numbers count: members of the groups the
    # teacher teaches (teachers: email => groups, as @@teachers), without
    # the test groups and the excluded accounts (admins, teachers, test
    # accounts). A Set of emails.
    def self.students_of(teacher, teachers:, group_members:, skip: [], excluded: [])
        groups = Array(teachers[teacher]) - Array(skip)
        students = groups.flat_map { |g| Array(group_members[g]) }.to_set
        students - excluded.to_set - [teacher]
    end

    def self.h(text)
        CGI.escapeHTML(text.to_s)
    end

    # The band for admins and teachers. tasks: [title] in page order;
    # per_task: how many solved each; solved: their sum; people: how many
    # solved at least one; of: the teacher's number of students (nil for
    # admins, whose steps follow the most solved task).
    def self.band_html(tasks, per_task, solved:, people:, css_class:, of: nil)
        top = of ? of : per_task.max.to_i
        bars = tasks.each_with_index.map do |title, i|
            n = per_task[i].to_i
            what = of ? "#{n} von #{of} deiner Lernenden gelöst" : "#{n}-mal gelöst"
            "<span class='usage-bar usage-level-#{level(n, top)}' title='#{i + 1}. #{h(title)}: #{what}'></span>"
        end
        text = if of
                   "Deine Lernenden: #{people} von #{of} haben hier etwas gelöst, zusammen #{solved} Aufgaben"
               else
                   "#{solved} gelöst · #{people} #{people == 1 ? 'Person' : 'Personen'}, ohne Lehrkräfte, Admins und Testkonten"
               end
        people_shown = of ? "#{people}/#{of}" : people.to_s
        "<span class='usage-row #{css_class}'>" \
            "<span class='usage-strip' role='img' aria-label='#{h(text)}'>#{bars.join}</span>" \
            "<span class='usage' title='#{h(text)}'>" \
            "<span aria-hidden='true'><i class='bi bi-check2-all'></i>#{solved}</span>" \
            "<span aria-hidden='true'><i class='bi bi-person'></i>#{people_shown}</span>" \
            "<span class='visually-hidden'>#{h(text)}</span></span></span>"
    end

    MARK_WORDS = { :done => 'gelöst', :wrong => 'noch nicht gelöst', :stale => 'Aufgabe geändert, noch einmal prüfen', :open => 'noch offen' }.freeze

    # A student's own band: tasks: [title] of the core tasks (the extras do
    # not count, as on the pinned card); marks: :done, :wrong, :stale or
    # :open each (StudentHome.progress).
    def self.own_html(tasks, marks, css_class:)
        done = marks.count(:done)
        bars = tasks.each_with_index.map do |title, i|
            mark = marks[i] || :open
            "<span class='shc-mark shc-#{mark}' title='#{i + 1}. #{h(title)}: #{MARK_WORDS[mark]}'></span>"
        end
        text = "Du hast #{done} von #{tasks.size} Aufgaben gelöst"
        "<span class='usage-row usage-own #{css_class}'>" \
            "<span class='shc-bar usage-own-bar' role='img' aria-label='#{h(text)}'>#{bars.join}</span>" \
            "<span class='usage' title='#{h(text)}'><span aria-hidden='true'>#{done}/#{tasks.size}</span>" \
            "<span class='visually-hidden'>#{h(text)}</span></span></span>"
    end
end
