require_relative "core"
require_relative "function_task"

module CodeBites
  module SingleFileTask
    SECTION_MARKER = "\n__END__\n"

    def self.load(path)
      # 1) load the ruby code (so it can define build_judge, etc.)
      load path

      # 2) parse embedded sections from the file contents
      content = File.read(path)
      idx = content.index(SECTION_MARKER)
      sections_text = idx ? content[(idx + SECTION_MARKER.length)..] : ""
      sections = parse_sections(sections_text)

      # 3) infer metadata (id, title)
      id = File.basename(path, ".rb")
      title = extract_title_from_markdown(sections["task.md"]) || id

      {
        id: id,
        title: title,
        sections: sections,
        # expect the task file to define build_judge(executor:, rng: ...)
        build_judge: Object.method(:build_judge)
      }
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

      # Accept "# Title" or "## Title" (first heading wins)
      md.each_line do |line|
        if line =~ /^\s*#{1,2}\s+(.+?)\s*$/
          return $1.strip
        end
      end

      nil
    end
  end
end