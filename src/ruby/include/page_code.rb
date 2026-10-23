# Is the code on a page the code that is tested? A screenshot recipe that
# writes a file from the activity's folder (write-file: … <- file:stufen/…)
# shows what the page describes since the recipe before it: one of the code
# blocks in between, in the language of the files it writes, must be part of
# one of those files.
# Lines that only say »…« stand for left-out code; indentation does not
# count, since excerpts are often shown without it.
#
# A block that is meant to differ, a starting point the student changes or
# an example that has nothing to do with the recipe after it, says so on
# the line before its fence: <!-- page-code: own -->
#
# A step marker for teachers (<!-- stage: stufen/03.c as main.c -->,
# StageCode) counts like a recipe that writes that file.
module PageCode
    LANGUAGES = {
        '.rb' => %w[ruby], '.py' => %w[python], '.html' => %w[html css js javascript], '.js' => %w[js javascript],
        '.c' => %w[c], '.css' => %w[css], '.yaml' => %w[yaml], '.yml' => %w[yaml], '.tex' => %w[tex latex],
        '.fs' => %w[glsl], '.vs' => %w[glsl], '.md' => %w[markdown md], '.svelte' => %w[svelte html css js javascript],
        '.frag' => %w[glsl], '.lua' => %w[lua], '.h' => %w[c],
    }.freeze
    STAGE = /^<!--\s*stage:\s*(\S+)/
    FENCE = /^```([A-Za-z0-9]*)(?:_[a-z]+)*[ \t]*$/
    ELISION = /\A\s*(?:#|\/\/|<!--|--|%)?\s*(?:…|\.\.\.)\s*(?:-->)?\s*\z/

    Mismatch = Struct.new(:page, :line, :source, :reason, keyword_init: true)

    def self.check(page_path)
        dir = File.dirname(page_path)
        lines = File.read(page_path).lines
        blocks = []          # code blocks since the last recipe: [line, language, text]
        mismatches = []
        i = 0
        in_recipe = false
        written = []         # files the current recipe writes: [source, text]
        while i < lines.size
            line = lines[i]
            if in_recipe
                in_recipe = false if line.include?('-->')
                if (m = line.match(/^write-file:\s*(\S+)\s*<-\s*file:(\S+)/))
                    source = File.join(dir, m[2])
                    written << [m[2], File.read(source)] if LANGUAGES[File.extname(m[2])] && File.exist?(source)
                end
                if !in_recipe
                    mismatches.concat(unmatched(page_path, blocks, written))
                    blocks = []
                    written = []
                end
            elsif (m = line.match(STAGE))
                source = File.join(dir, m[1])
                if !File.file?(source)
                    # a typo in a marker would only make the teacher's button go missing
                    mismatches << Mismatch.new(:page => page_path, :line => i + 1, :source => m[1],
                        :reason => 'the stage marker names a file that does not exist')
                elsif LANGUAGES[File.extname(m[1])]
                    mismatches.concat(unmatched(page_path, blocks, [[m[1], File.read(source)]]))
                end
                blocks = []
            elsif line.start_with?('<!-- tutorial-screenshot')
                in_recipe = !line.include?('-->')
                blocks = [] unless in_recipe
            elsif (m = line.match(FENCE)) && !m[1].empty?
                own = i > 0 && lines[i - 1].strip == '<!-- page-code: own -->'
                start = i
                body = []
                i += 1
                while i < lines.size && lines[i] !~ /^```\s*$/
                    body << lines[i]
                    i += 1
                end
                blocks << [start + 1, m[1].downcase, body.join] unless own
            end
            i += 1
        end
        mismatches
    end

    # For each language among the files a recipe writes: at least one block
    # since the last recipe must be part of one of them. A recipe that
    # writes a file the page does not show (a second file, a helper) is fine
    # as long as the step's code is in one of the files.
    def self.unmatched(page_path, blocks, written)
        written.group_by { |source, _| LANGUAGES[File.extname(source)] }.filter_map do |languages, files|
            candidates = blocks.select { |_, language, _| languages.include?(language) }
            next if candidates.empty?
            next if candidates.any? { |_, _, text| files.any? { |_, file| excerpt_problem(text, file).nil? } }
            line, _, text = candidates.last
            Mismatch.new(:page => page_path, :line => line, :source => files.map(&:first).join(', '),
                :reason => excerpt_problem(text, files.last[1]))
        end
    end

    # Context lines (»~ «, CodeHighlight::CONTEXT) show what the file has
    # around the new code at that moment; code from later steps may stand
    # between them in the finished file. They count as left-out code.
    def self.normalize(text)
        text.lines.map { |l| l =~ /\A~(?: |$)/ ? '…' : l.strip }
    end

    # nil if every run of lines between elisions appears in the file, in
    # order; otherwise the first line that does not.
    def self.excerpt_problem(block, file)
        have = normalize(file)
        chunks = normalize(block).slice_when { |a, b| a =~ ELISION || b =~ ELISION }.map { |c| c.reject { |l| l =~ ELISION } }
        chunks = chunks.map { |c| c.drop_while(&:empty?).reverse.drop_while(&:empty?).reverse }.reject(&:empty?)
        position = 0
        chunks.each do |chunk|
            found = (position..have.size - chunk.size).find { |p| have[p, chunk.size] == chunk }
            unless found
                # Name the first line of the chunk that is not where it should be
                missing = chunk.find { |l| !have.include?(l) } || chunk.first
                return "»#{missing.strip}« is not in the file"
            end
            position = found + chunk.size
        end
        nil
    end
end
