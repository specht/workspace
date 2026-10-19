require 'minitest/autorun'

# Every activity clones its template into a folder no other activity uses
# (docs/checkliste.md, »Vorlagen und Daten«). »Clone Repository« (the
# Handbuch section workspace#repository-klonen) names the folder after the
# repository; VS Code does not clone into an existing folder but silently
# into name-1, name-2 …, and the page then speaks of the wrong folder.
# Activities that share a template clone in the terminal instead:
# git clone ${template} ~/NAME. Handbuch pages explain the projects of
# activities and use their folders; they are not checked.
class CloneFoldersTest < Minitest::Test
    CONTENT = File.expand_path('../../content', __dir__)

    def folders
        Dir[File.join(CONTENT, '*', '*.md')].each_with_object(Hash.new { |h, k| h[k] = [] }) do |file, found|
            slug = File.basename(File.dirname(file))
            next if slug.start_with?('_') || File.basename(file, '.md') != slug

            text = File.read(file, encoding: 'UTF-8')
            template = text[%r{<div class='meta'>.*?^template:\s*(\S+)}m, 1]
            next unless template

            repository = File.basename(template, '.git')
            found[repository] << slug if text.include?('_include(workspace#repository-klonen)')
            # git clone ${template} ~/NAME (or NAME): the folder is NAME.
            text.scan(/^git clone \$\{template\}(?:[ \t]+(\S+))?[ \t]*$/) { |(path)| found[path ? File.basename(path) : repository] << slug }
        end
    end

    def test_no_two_activities_clone_into_the_same_folder
        shared = folders.transform_values(&:uniq).select { |_, slugs| slugs.size > 1 }
        assert_empty shared, "Activities cloning into the same folder: #{shared.map { |f, s| "#{f}: #{s.join(', ')}" }.join('; ')}"
    end

    def test_the_pixelram_activities_have_their_own_folders
        found = folders
        assert_equal ['pixelram'], found['pixelram-starter']
        %w[gif jpeg feuerwerk].each { |slug| assert_equal [slug], found[slug], slug }
    end
end
