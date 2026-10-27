require 'cgi'

# Activities in more than one programming language (docs/aktivitaet-schreiben.md).
#
# A page that offers Ruby and Python says so in its meta block,
#
#   languages: [ruby, python]
#
# and marks what differs between them, on lines of their own:
#
#   <!-- language: ruby -->
#   ```ruby
#   def compress(data)
#   ```
#   <!-- /language -->
#   <!-- language: python -->
#   ```python
#   def compress(data):
#   ```
#   <!-- /language -->
#
# Both versions are in the page; CSS shows the one in data-code-language on
# the page container, and the switch in the page header changes it without a
# reload. A task in tasks.yaml can have a variant per language, a key with
# the language's name that replaces prompt, check, hint, solution, feedback
# or file; it keeps its id and version, so a task solved in one language is
# solved in the other.
#
# Which language a person sees: the first the page offers of their own
# choice (u.code_language, set with the switch), the languages of their
# classes a teacher has set one for, and Ruby; else the page's first.
#
# A class sets Ruby or Python (GROUP_LANGUAGES). JavaScript, Dart and Java
# are offered where a page needs them: JavaScript on the Katas
# (docs/plan-katas.md), all five on the activity about classes
# (docs/plan-lehrplan-luecken.md). Someone who picks one of these three keeps
# it apart (u.kata_code_language) and still sees Ruby or Python elsewhere.
#
# A page without languages: in its meta (a Handbuch page) may use the same
# markers to show both: a Ruby part and a Python part right after each other
# stand side by side, each labelled with its language (pair).
module CodeLanguages
    LANGUAGES = %w[ruby python javascript dart java].freeze
    NAMES = { 'ruby' => 'Ruby', 'python' => 'Python', 'javascript' => 'JavaScript', 'dart' => 'Dart', 'java' => 'Java' }.freeze
    # What a teacher can set for a class
    GROUP_LANGUAGES = %w[ruby python].freeze
    DEFAULT = 'ruby'
    VARIANT_KEYS = %w[prompt check hint solution feedback file starter].freeze

    START = /^[ \t]*<!--[ \t]*language:[ \t]*([a-z]+)[ \t]*-->[ \t]*$/
    FINISH = /^[ \t]*<!--[ \t]*\/language[ \t]*-->[ \t]*$/

    def self.known?(language)
        LANGUAGES.include?(language.to_s)
    end

    # The languages a page offers, from its meta block; [] for a page in one
    # language (or none).
    def self.of_page(meta)
        list = meta.is_a?(Hash) ? Array(meta['languages']).map { |l| l.to_s.strip.downcase } : []
        list = list.select { |l| known?(l) }.uniq
        list.size > 1 ? list : []
    end

    # The markers become empty marker elements that Redcarpet passes through;
    # wrap puts what is between them into one element after rendering.
    # Fenced code blocks are left alone, so pages can show the markers.
    def self.mark(markdown, warn: nil)
        markdown.split(/(^```.*?^```[ \t]*$)/m).map.with_index do |piece, i|
            next piece if i.odd?

            piece.gsub(START) do
                warn&.call("<!-- language: #{$1} -->: unknown language") unless known?($1)
                "\n<div class='code-language-start' data-language='#{CGI.escapeHTML($1)}'></div>\n"
            end.gsub(FINISH, "\n<div class='code-language-end'></div>\n")
        end.join
    end

    # In a rendered page (Nokogiri), everything from a start marker to the
    # next end marker among its siblings goes into
    # <div class='code-language' data-language='…'>.
    def self.wrap(root, warn: nil)
        root.css('div.code-language-start').each do |start|
            box = Nokogiri::XML::Node.new('div', root.document)
            box['class'] = 'code-language'
            box['data-language'] = start['data-language']
            node = start.next_sibling
            closed = false
            while node
                following = node.next_sibling
                if node.element? && node['class'] == 'code-language-end'
                    node.remove
                    closed = true
                    break
                end
                box.add_child(node)
                node = following
            end
            warn&.call("<!-- language: #{start['data-language']} --> without <!-- /language -->") unless closed
            start.replace(box)
        end
        root.css('div.code-language-end').each do |stray|
            warn&.call('<!-- /language --> without <!-- language: … -->')
            stray.remove
        end
        root
    end

    # On a page in one language (a Handbuch page), parts for different
    # languages that follow each other directly stand side by side: they go
    # into <div class='code-compare'>, and CSS labels each with its language.
    # On a page with the switch only one of them is shown, so they stay apart.
    def self.pair(root)
        root.css('div.code-language').each do |box|
            next if box.parent.nil? || box.parent['class'] == 'code-compare'

            group = [box]
            node = box.next_element
            while node && node.name == 'div' && node['class'] == 'code-language' &&
                  group.none? { |b| b['data-language'] == node['data-language'] }
                group << node
                node = node.next_element
            end
            next if group.size < 2

            compare = Nokogiri::XML::Node.new('div', root.document)
            compare['class'] = 'code-compare'
            box.add_previous_sibling(compare)
            group.each { |b| compare.add_child(b) }
        end
        root
    end

    # The languages a task has a variant for, besides its own.
    def self.variant_languages(task)
        LANGUAGES.select { |l| task[l].is_a?(Hash) }
    end

    # The task as it is in one language: the variant's fields over the
    # task's own, without the variants themselves.
    def self.variant(task, language)
        base = task.reject { |k, _| LANGUAGES.include?(k) }
        own = task[language.to_s]
        own.is_a?(Hash) ? base.merge(own) : base
    end

    # Which language a person sees: own choice, then the first class with a
    # setting, then Ruby. group_languages: { group => language } for the
    # person's groups, in file order.
    # offered: the page's languages; without them, any known language.
    def self.resolve(own, group_languages, offered = LANGUAGES)
        candidates = [own, *group_languages.values, DEFAULT]
        candidates.find { |l| known?(l) && offered.include?(l) } || offered.first
    end

    # The switch in the page header: one button per language, with its logo
    # (static/images/languages/), the current one pressed; the look follows
    # aria-pressed (default.css, .code-language-switch).
    # The name keeps the width it has in bold (data-name, default.css), and a
    # pill behind the buttons slides to the chosen one (main.rb,
    # code_language_setup).
    def self.switch_html(languages, current)
        buttons = languages.map do |l|
            pressed = l == current
            "<button type='button' class='code-language-option' data-set-language='#{l}' aria-pressed='#{pressed}'>" \
                "<img src='/images/languages/#{l}.webp' alt='' width='16' height='16'>" \
                "<span class='code-language-option-name' data-name='#{NAMES[l]}'>#{NAMES[l]}</span></button>"
        end
        "<span class='code-language-switch' role='group' aria-label='Programmiersprache'>" \
            "<span class='code-language-thumb' aria-hidden='true'></span>#{buttons.join}</span>"
    end
end
