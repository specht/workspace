require 'minitest/autorun'
require 'yaml'

# Every activity in the catalog has a teacher note (lehrkraft.md), and its
# first paragraph says what the students do, with »Lernende« in it: a
# teacher who opens the note sees that first (docs/aktivitaet-schreiben.md,
# »Eine Notiz für Lehrkräfte«). Profiles of programming languages
# (kind: steckbrief) and hidden sections are no activities to plan with.
class TeacherNoteTest < Minitest::Test
    CONTENT = File.expand_path('../../content', __dir__)

    def activities
        YAML.load_file(File.join(CONTENT, 'sections.yaml')).reject { |section| section['hidden'] }
            .flat_map { |section| section['entries'] || [] }
            .map { |entry| entry.to_s.delete_prefix('.') }
            .select { |slug| File.file?(File.join(CONTENT, slug, "#{slug}.md")) }
            .reject { |slug| File.read(File.join(CONTENT, slug, "#{slug}.md"))[/^kind:\s*(\S+)/, 1] == 'steckbrief' }
    end

    def test_every_activity_has_a_teacher_note
        missing = activities.reject { |slug| File.file?(File.join(CONTENT, slug, 'lehrkraft.md')) }
        assert_empty missing, "Activities without lehrkraft.md:\n#{missing.join("\n")}"
    end

    def test_the_note_opens_with_what_students_do
        vague = activities.select { |slug| File.file?(File.join(CONTENT, slug, 'lehrkraft.md')) }.reject do |slug|
            File.read(File.join(CONTENT, slug, 'lehrkraft.md')).strip.split(/\n\s*\n/).first.to_s.include?('Lernende')
        end
        assert_empty vague, "The first paragraph of these teacher notes does not say what »Lernende« do:\n#{vague.join("\n")}"
    end
end
