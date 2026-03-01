require_relative "core"
require_relative "function_task"
require "yaml"

module CodeBites
    module SingleFileTask
        SECTION_MARKER = "\n__END__\n"

        def self.load(path)
            content = File.read(path)
            ruby_code, sections_text = split_code_and_sections(content)

            task_mod = Module.new
            task_mod.module_eval(ruby_code, path, 1)

            unless task_mod.instance_methods(false).include?(:build_judge)
                raise ArgumentError, "Task file must define build_judge(executor:, rng:)"
            end

            sections = parse_sections(sections_text)

            id = File.basename(path, ".rb")
            title = extract_title_from_markdown(sections["task.md"]) || id

            meta = {}
            if (y = sections["meta.yaml"]) && !y.strip.empty?
                begin
                    meta = YAML.safe_load(y) || {}
                rescue
                    meta = {}
                end
            end

            build = task_mod.instance_method(:build_judge)

            {
                id: id,
                title: title,
                sections: sections,
                meta: meta,
                build_judge: ->(executor:, rng:) { build.bind(task_mod).call(executor: executor, rng: rng) }
            }
        end

        def self.split_code_and_sections(content)
            idx = content.index(SECTION_MARKER)
            return [content, ""] unless idx
            [content[0...idx], content[(idx + SECTION_MARKER.length)..]]
        end

        def self.parse_sections(text)
            out = {}
            current = nil
            buf = +""

            text.each_line do |line|
                if line.start_with?("@@")
                    out[current] = buf.rstrip + "\n" if current
                    current = line.strip.sub("@@", "")
                    buf = +""
                else
                    buf << line
                end
            end

            out[current] = buf.rstrip + "\n" if current
            out
        end

        def self.extract_title_from_markdown(md)
            return nil if md.nil? || md.strip.empty?

            md.each_line do |line|
                # Accept "# Title" or "## Title" (first heading wins)
                m = line.match(/^\s*(#+)\s+(.+?)\s*$/)
                next unless m

                hashes = m[1]
                return m[2].strip if hashes.length <= 2
            end

            nil
        end
    end
end
