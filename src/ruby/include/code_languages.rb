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
# Which language a person sees: their own choice (u.code_language, set with
# the switch), else that of the first of their classes a teacher has set one
# for, else Ruby.
module CodeLanguages
    LANGUAGES = %w[ruby python].freeze
    NAMES = { 'ruby' => 'Ruby', 'python' => 'Python' }.freeze
    DEFAULT = 'ruby'
    VARIANT_KEYS = %w[prompt check hint solution feedback file].freeze

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
    def self.resolve(own, group_languages)
        return own if known?(own)

        from_group = group_languages.values.find { |l| known?(l) }
        from_group || DEFAULT
    end

    # The switch in the page header: one button per language, with its logo
    # (static/images/languages/), the current one pressed; the look follows
    # aria-pressed (default.css, .code-language-switch).
    def self.switch_html(languages, current)
        buttons = languages.map do |l|
            pressed = l == current
            "<button type='button' class='code-language-option' data-set-language='#{l}' aria-pressed='#{pressed}'>" \
                "<img src='/images/languages/#{l}.webp' alt='' width='16' height='16'>#{NAMES[l]}</button>"
        end
        "<span class='code-language-switch' role='group' aria-label='Programmiersprache'>#{buttons.join}</span>"
    end
end
