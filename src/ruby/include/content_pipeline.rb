# The pages: reading every activity and Handbuch page from src/content,
# rendering its Markdown (code, includes, pictures with their variants,
# videos, the table of contents, the task cards), the page's metadata for
# search engines and previews, and the content cache that lets a restart
# skip unchanged pages (parse_content). Moved out of main.rb, which loads
# it before configure reads the content.
class Main < Sinatra::Base
    # The values for ${...} in a content page: its meta block, the site's
    # contact address and its own address. The class repository's variables
    # depend on who looks at the page: in the rendered page they are tokens
    # that page_html fills in per request (ClassRepos); per_viewer: false
    # leaves the placeholders (teacher notes).
    def self.content_variables(meta, per_viewer: true)
        email = contact_email
        values = ContentParts.page_variables(meta).merge('contact_email' => email.empty? ? nil : email, 'web_root' => WEB_ROOT)
        per_viewer ? values.merge(ClassRepos::TOKENS) : values
    end
    # The features of this installation that pages may show parts for
    # (ContentParts.filter_features).
    def self.content_features
        (CaseServer.enabled? ? ['case_server'] : []) + (GitServer.enabled? ? ['git_server'] : [])
    end
    def self.convert_image(image_path)
        image_sha1 = Digest::SHA1.hexdigest(File.read(image_path))[0, 16]
        target_path = "/webcache/#{image_sha1}.webp"
        unless FileUtils.uptodate?(target_path, [image_path])
            STDERR.puts "Copying #{image_path} to cache..."
            FileUtils.cp(image_path, target_path)
        end
        image_sha1
    end
    # A drawing for an overview card (image: name.webp:contain): padded to the
    # card's 16:9 with the colour of its top left corner and a margin, so
    # the card crops nothing, in the light and in the dark variant alike.
    def self.convert_image_padded(image_path)
        image_sha1 = Digest::SHA1.hexdigest(File.read(image_path) + 'pad-16x9')[0, 16]
        target_path = "/webcache/#{image_sha1}.webp"
        unless FileUtils.uptodate?(target_path, [image_path])
            size, = Open3.capture2('identify', '-format', '%w %h', "#{image_path}[0]")
            w, h = size.split.map(&:to_i)
            corner, = Open3.capture2('convert', "#{image_path}[0]", '-format', '%[pixel:p{0,0}]', 'info:')
            margin = 1.1
            width = [w, (h * 16.0 / 9).ceil].max * margin
            height = width * 9 / 16
            ok = system('convert', "#{image_path}[0]", '-background', corner.strip, '-gravity', 'center',
                '-extent', "#{width.round}x#{height.round}", target_path)
            STDERR.puts "...padding of #{image_path} failed!" unless ok
        end
        image_sha1
    end
    def self.inject_autotoc(root, options = {})
        options[:inject_numbers] = true unless options.include?(:inject_numbers)
        h2_list = root.css('h2').reject { |x| x['data-autotoc'] == 'ignore' }
        if h2_list.size > 1
            h2_list.each.with_index do |h2, index|
                autotoc_container = Nokogiri::XML::Node.new('div', root)
                autotoc_container['class'] = 'autotoc-container'
                label = h2.text.strip
                if options[:inject_numbers]
                    label = "#{index + 1}. #{h2.text}".strip
                    h2.content = label
                end
                autotoc_container['data-label'] = label
                h2.add_previous_sibling(autotoc_container)
                autotoc_container.add_child(h2)
                p = autotoc_container.next_element
                while p && p.name != 'h2'
                    pnext = p.next_element
                    autotoc_container.add_child(p)
                    p = pnext
                end
            end
        end
    end
    DEFAULT_PAGE_DESCRIPTION = 'Der Hackschule Workspace ist eine browserbasierte Entwicklungsumgebung für den Informatikunterricht mit Visual Studio Code, Terminal, Git und zahlreichen Tutorials.'
    def self.normalize_page_metadata_text(text)
        CGI.unescapeHTML(text.to_s)
            .delete("\u00ad")
            .gsub(/\s+/, ' ')
            .strip
    end
    def self.truncate_page_metadata_text(text, max_length = 180)
        return text if text.length <= max_length

        shortened = text[0, max_length + 1].sub(/\s+\S*\z/, '').rstrip
        shortened = text[0, max_length].rstrip if shortened.empty?
        "#{shortened}…"
    end
    def self.inject_page_metadata(html, request_path)
        doc = Nokogiri::HTML(html)
        body = doc.at_css('body') || doc
        homepage = ['/', '/index.html'].include?(request_path)

        # The page's own heading: the first h1, else the first h2, but none
        # inside something hidden (the QR panel of the login form, say).
        hidden = ->(node) { node.ancestors.any? { |a| a.respond_to?(:[]) && (a['hidden'] || a['style'].to_s.match?(/display:\s*none/)) } }
        heading = nil
        unless homepage
            heading = body.css('h1').find { |h| !hidden.(h) } || body.css('h2').find { |h| !hidden.(h) }
        end

        heading_text = normalize_page_metadata_text(heading&.text)
        page_title = if heading_text.empty?
            'Hackschule Workspace'
        else
            "#{heading_text} | Hackschule Workspace"
        end

        description_node = nil
        description_node = body.at_css('.abstract') unless homepage
        if description_node.nil?
            description_node = body.css('p').find do |paragraph|
                normalize_page_metadata_text(paragraph.text).length >= 40
            end
        end

        description = normalize_page_metadata_text(description_node&.text)
        description = DEFAULT_PAGE_DESCRIPTION if description.empty?
        description = truncate_page_metadata_text(description)

        escaped_title = CGI.escapeHTML(page_title)
        escaped_description = CGI.escapeHTML(description)
        description_tag = %(<meta name="description" content="#{escaped_description}">)

        result = html.sub(
            /<title\b[^>]*>.*?<\/title>/mi,
            "<title>#{escaped_title}</title>"
        )

        description_pattern = /<meta\s+name=["']description["']\s+content=["'][^"']*["']\s*\/?>/i
        if result.match?(description_pattern)
            result = result.sub(description_pattern, description_tag)
        else
            result = result.sub(
                /<\/title>/i,
                "</title>\n    #{description_tag}"
            )
        end

        result
    end
    # The result of a full parse is kept in CONTENT_CACHE, under a key made
    # of everything it comes from: the code that renders, every file of the
    # content (path, size, time), the installation's variables in pages,
    # and whether this is development. A start with the same inputs loads it
    # instead of rendering every page again; anything changed, and the pages
    # are rendered (parse_content_locked). Only the newest result is kept.
    CONTENT_CACHE = '/webcache/content-cache'
    def self.content_cache_key
        digest = Digest::SHA256.new
        digest.update("format 1\0#{DEVELOPMENT}\0")
        files = Dir.glob('/src/ruby/**/*.rb') + Dir.glob('/src/content/**/*', File::FNM_DOTMATCH).select { |path| File.file?(path) }
        files.sort.each do |path|
            stat = File.stat(path)
            digest.update("#{path}\0#{stat.size}\0#{stat.mtime.to_f}\0")
        end
        digest.update(Main.content_variables({}, :per_viewer => false).to_a.sort.inspect)
        digest.hexdigest
    end
    def self.content_catalog_signature
        digest = Digest::SHA256.new
        ['/src/content/sections.yaml', '/src/content/lehrmaterial/sections.yaml',
         REFERENCE_SECTIONS_PATH].each do |path|
            next unless File.exist?(path)
            digest.update(File.binread(path))
            digest.update("\0")
        end
        Dir['/src/content/**/*.md'].sort.each do |path|
            digest.update(path)
            digest.update("\0")
        end
        digest.hexdigest
    end
    def self.refresh_content_catalog
        return unless DEVELOPMENT
        signature = content_catalog_signature
        return if defined?(@@content_catalog_signature) &&
            @@content_catalog_signature == signature
        parse_content
    end
    def self.refresh_tutorial(slug)
        return unless DEVELOPMENT
        refresh_content_catalog
        path = if defined?(@@content_paths) && @@content_paths
            @@content_paths[slug]
        end
        return unless path && File.file?(path)

        # Generation is intentionally outside @@parse_content_mutex. The
        # generator may launch a Workspace and make requests back to Ruby.
        TutorialScreenshots.prepare(File.read(path), path)
        parse_content(slug)
    end
    # Renders the Markdown of a content file to a Nokogiri document:
    # Redcarpet, reference include boxes, cached images and videos, external
    # links and syntax highlighting. The block may change the Markdown first
    # (the Kenney gallery uses this). Hyphenation is the browser's
    # (hyphens: auto in default.css).
    def self.render_content_markdown(markdown, path, redcarpet)
        markdown = yield(markdown) if block_given?
        html = redcarpet.render(markdown)
        root = Nokogiri::HTML(html)
        wrap_reference_includes(root)
        PictureRights.withhold(root, path, development: DEVELOPMENT)

        root.css('img').each do |img|
            begin
                src = img.attr('src')
                next if src.nil?
                if DEVELOPMENT && !src.start_with?('/cache/')
                    img['data-tutorial-screenshot-source'] = src
                end
                next if src.start_with?('/cache/')
                image_path = File.join(File.dirname(path), src)
                next unless File.exist?(image_path)
                # an SVG stays an SVG: sharp at every size, and as WebP it
                # would be served with the wrong type
                if img.attr('data-noconvert') || File.extname(image_path).downcase == '.svg'
                    sha1 = Digest::SHA1.hexdigest(image_path)
                    extension = File.extname(image_path)
                    target_path = "/webcache/#{sha1}#{extension}"
                    FileUtils.cp(image_path, target_path) unless FileUtils.uptodate?(target_path, [image_path])
                    img['src'] = "/cache/#{sha1}#{extension}"
                else
                    sha1 = convert_image(image_path)
                    img['src'] = "/cache/#{sha1}.webp"
                    add_media_variants(img, image_path) { |variant_path| "/cache/#{convert_image(variant_path)}.webp" }
                end
                # a screenshot of the site (class shot, taken at twice the
                # pixels): at most half its width, so that its text is never
                # bigger than the page's around it
                if img.classes.include?('shot') && (width = webp_width(image_path))
                    img['style'] = ["max-width: #{width / 2}px; min-width: min(1000px, #{width / 2}px)", img['style']].compact.join('; ')
                end
                if img.classes.include?('full')
                    # a picture made from a drawing beside it (x.svg next to
                    # x.webp) has the page as its background: no shadow in
                    # dark mode, unlike a screenshot (dark.css)
                    diagram = File.exist?(image_path.sub(/\.[a-z]+\z/i, '.svg'))
                    img.wrap(diagram ? "<div class='scroll-x diagram'>" : "<div class='scroll-x'>")
                end
            rescue
                STDERR.puts img.to_s
                raise
            end
        end
        # A script next to the page (<script src='x.js'>), e.g. the ER-Werkstatt
        # core for the Handbuch playground: served from the cache by content
        # hash, so a changed file is never stale in a browser.
        root.css('script[src]').each do |script|
            src = script['src'].to_s
            next if src.empty? || src.start_with?('/') || src =~ /\A[a-z]+:/i
            script_path = File.join(File.dirname(path), src)
            next unless File.file?(script_path)
            sha1 = Digest::SHA1.hexdigest(File.binread(script_path))[0, 16]
            target_path = "/webcache/#{sha1}.js"
            FileUtils.cp(script_path, target_path) unless File.exist?(target_path)
            script['src'] = "/cache/#{sha1}.js"
        end
        root.css('svg').each do |img|
            if img.classes.include?('full')
                img.wrap("<div class='scroll-x'>")
            end
        end
        root.css('video').each do |video|
            src = video.attr('src')
            image_path = File.join(File.dirname(path), src)
            next unless File.exist?(image_path)
            video['src'] = cache_video(image_path)
            add_media_variants(video, image_path) { |variant_path| cache_video(variant_path) }
        end
        ExternalLinks.mark(root)
        Tools.link_targets(root)
        highlight_code(root)
        root
    end
    def self.cache_video(video_path)
        sha1 = Digest::SHA1.hexdigest(File.read(video_path))[0, 16]
        target_path = "/webcache/#{sha1}.mp4"
        FileUtils.cp(video_path, target_path) unless FileUtils.uptodate?(target_path, [video_path])
        "/cache/#{sha1}.mp4"
    end
    # A generated image or video can have variants beside it (x-dark.webp,
    # x-anaglyph.webp, x-dark-anaglyph.webp, x-crt.webp; variants: in the
    # screenshot README). media_variants.js shows the one that fits the theme
    # and the toggles in the corner (glasses, CRT).
    MEDIA_VARIANTS = %w[dark anaglyph dark-anaglyph crt folie]
    # The width of a WebP in pixels, from its header (lossy VP8, lossless
    # VP8L, extended VP8X); nil for anything else
    def self.webp_width(path)
        head = File.binread(path, 30)
        return nil unless head && head.size >= 30 && head[0, 4] == 'RIFF' && head[8, 4] == 'WEBP'

        case head[12, 4]
        when 'VP8 ' then head[26, 2].unpack1('v') & 0x3fff
        when 'VP8L' then (head[21, 4].unpack1('V') & 0x3fff) + 1
        when 'VP8X' then (head[24, 3] + "\0").unpack1('V') + 1
        end
    rescue StandardError
        nil
    end
    def self.add_media_variants(element, media_path)
        extension = File.extname(media_path)
        MEDIA_VARIANTS.each do |variant|
            variant_path = media_path.delete_suffix(extension) + "-#{variant}#{extension}"
            element["data-variant-#{variant}"] = yield(variant_path) if File.exist?(variant_path)
        end
    end
    def self.highlight_code(root)
        CodeHighlight.apply(root)
    end
    # _include() leaves two marker divs around the included part (see
    # ContentParts.expand_includes); everything between them becomes one box
    # with a link to the part on its reference page.
    def self.wrap_reference_includes(root)
        root.css('div.reference-include-begin').each do |start|
            box = root.document.create_element('div', 'class' => 'reference-include')
            start.add_previous_sibling(box)
            node = start.next_sibling
            start.remove
            while node && !(node.element? && node['class'] == 'reference-include-end')
                following = node.next_sibling
                box.add_child(node)
                node = following
            end
            next unless node
            # _include(…, done: id): tasks.js folds the box once task id is
            # solved; include_fold.js once the part was done on another page.
            %w[data-done data-part data-title data-short].each do |name|
                box[name] = node[name] if node[name]
            end
            link = root.document.create_element('p', 'class' => 'reference-include-link')
            # The tooltip names where the text comes from, like the breadcrumb
            # on the reference page: »Workspace: Grundlagen › Repository klonen«.
            anchor = root.document.create_element('a', 'href' => node['data-href'], 'title' => node['data-label'].to_s)
            anchor.add_child(root.document.create_element('i', 'class' => 'bi bi-book'))
            anchor.add_child(root.document.create_text_node('Im Handbuch nachschlagen'))
            link.add_child(anchor)
            box.add_child(link)
            node.remove
        end
    end
    # Replaces the task slots left by TaskCard.mark_slots with task cards,
    # numbered in page order.
    def self.insert_task_cards(root, task_slots, page_tasks, redcarpet, path, languages = [])
        render = lambda { |md| redcarpet.render(md.to_s) }
        number = 0
        root.css('div.task-slot').each do |slot|
            id = task_slots[slot['data-slot'].to_i]
            task = page_tasks && page_tasks[:by_id][id]
            html = if task && !languages.empty? && !CodeLanguages.variant_languages(task).empty?
                # A card for each language the page offers, with the same
                # number; only the one for the language shown is visible.
                number += 1
                languages.map do |language|
                    card = TaskCard.html(CodeLanguages.variant(task, language), :activity => page_tasks[:activity],
                        :number => number, :render => render, :language => language,
                        :id_suffix => language == languages.first ? '' : "-#{language}")
                    "<div class='code-language' data-language='#{language}'>#{card}</div>"
                end.join
            elsif task
                number += 1
                TaskCard.html(task, :activity => page_tasks[:activity], :number => number, :render => render)
            else
                STDERR.puts "#{path}: _task(#{id}): no such task in tasks.yaml"
                TaskCard.missing_html(id)
            end
            fragment = root.fragment(html)
            highlight_code(fragment)
            slot.replace(fragment)
        end
    end
    def self.parse_content(only_slug = nil)
        @@parse_content_mutex ||= Mutex.new
        @@parse_content_mutex.synchronize do
            parse_content_locked(only_slug)
        end
    end
    def self.parse_content_locked(only_slug = nil)
        partial = !only_slug.nil?
        parse_started_at = Process.clock_gettime(Process::CLOCK_MONOTONIC)
        cache_key = partial ? nil : (content_cache_key rescue nil)
        cache_file = cache_key && File.join(CONTENT_CACHE, "#{cache_key}.marshal")
        if cache_file && File.file?(cache_file)
            begin
                cached = Marshal.load(File.binread(cache_file))
                @@section_order, @@teaching_section_order, @@reference_section_order, @@sections, @@kenney, @@content, @@content_paths = cached
                @@content_catalog_signature = content_catalog_signature
                @@parse_content_count = (@@parse_content_count ||= 0) + 1
                STDERR.puts "Loaded content from #{cache_file} in #{format('%.3f', Process.clock_gettime(Process::CLOCK_MONOTONIC) - parse_started_at)}s"
                return true
            rescue StandardError => e
                STDERR.puts "Content cache #{cache_file} not usable (#{e.class}: #{e.message}), rendering the pages"
            end
        end
        STDERR.puts(partial ? "Parsing content: #{only_slug}..." : "Parsing content...")
        # Keep the student and teacher catalogs independent while sharing the
        # Markdown renderer, image cache, and screenshot generator.
        student_sections = YAML.load(File.read('/src/content/sections.yaml'))
        # Teacher-only pages (src/content/lehrmaterial) are optional; there
        # are none at the moment.
        teaching_sections = File.exist?('/src/content/lehrmaterial/sections.yaml') ?
            (YAML.load(File.read('/src/content/lehrmaterial/sections.yaml')) || []) : []
        # Reference pages (Handbuch) live in their own catalog and are
        # served under /reference/<slug>. The catalog is optional.
        reference_sections = File.exist?(REFERENCE_SECTIONS_PATH) ?
            (YAML.load(File.read(REFERENCE_SECTIONS_PATH)) || []) : []
        section_order = student_sections.map { |section| section['key'] }
        teaching_section_order = teaching_sections.map { |section| section['key'] }
        reference_section_order = reference_sections.map { |section| section['key'] }
        sections = student_sections.map { |section| section.merge('catalog' => 'student') } +
            teaching_sections.map do |section|
                section.merge('catalog' => 'teaching', 'teacher_only' => true)
            end +
            reference_sections.map { |section| section.merge('catalog' => 'reference') }
        parsed_sections = {}
        parsed_content = {}
        parsed_content_paths = {}
        paths = []
        seen_paths = Set.new()
        sections.each do |section|
            parsed_sections[section['key']] = {}
            section.each_pair do |k, v|
                parsed_sections[section['key']][k.to_sym] = v
            end
            parsed_sections[section['key']][:entries] = []
            (section['entries'] || []).each do |path|
                dev_only = path[0] == '.'
                path = path.sub(/^\./, '')
                content_root = case section['catalog']
                    when 'teaching' then '/src/content/lehrmaterial'
                    when 'reference' then REFERENCE_ROOT
                    else '/src/content'
                end
                slug = section['catalog'] == 'reference' ? "reference/#{path}" : path
                raise "duplicate content slug: #{slug}" unless seen_paths.add?(slug)
                # <name>.md is the page; a teacher note (lehrkraft.md) never is.
                original_path = ContentParts.page_file("#{content_root}/#{path}", path)
                paths << {:section => section['key'], :path => path, :slug => slug, :original_path => original_path, :dev_only => dev_only}
            end
        end
        Dir['/src/content/**/+*.md'].each do |path|
            original_path = path
            path = File.basename(path)
            next if path.include?('/')
            next unless path.include?('+')
            path = path.sub('+', '')
            unless seen_paths.include?(path)
                STDERR.puts "Got custom path: #{path} (#{original_path})"
                paths << {:section => 'misc', :path => path, :dev_only => false, :original_path => original_path, :extra => true}
                seen_paths << path
            end
        end

        parsed_kenney = if partial && defined?(@@kenney) && @@kenney
            @@kenney
        else
            {}
        end
        unless partial
            Dir['/src/content/anaglyph/kenney/*/*.webp'].sort.each do |path|
                kit = path.split('/').last(2).first
                model = path.split('/').last(2).last.sub('.webp', '')
                parsed_kenney[kit] ||= []
                parsed_kenney[kit] << model
            end

            parsed_kenney.keys.each do |kit|
                paths << {:section => 'misc', :path => kit, :original_path => "/src/content/anaglyph/#{kit}.md", :dev_only => false, :kenney => true, :extra => true}
            end
        end

        # the pages a person box on the Personen page may lead to (PersonBoxes)
        person_pages = paths.reject { |entry| entry[:dev_only] || entry[:extra] || %w[hidden parked].include?(entry[:section]) }
            .map { |entry| ["/#{entry[:slug]}", entry[:original_path]] }

        if partial
            paths.select! do |entry|
                entry_path = entry[:original_path]
                next false unless entry_path
                (entry[:slug] || File.basename(entry_path, '.md')
                    .sub(/^[0-9]+\-/, '')
                    .sub('+', '')) == only_slug
            end
            return false if paths.empty?
        end

        redcarpet = Redcarpet::Markdown.new(Redcarpet::Render::HTML, {:fenced_code_blocks => true})
        @@parse_content_count ||= 0
        @@parse_content_count += 1
        parse_content_count = @@parse_content_count

        # Kenney previews are already WebP files, so don't send every image through
        # the generic image-cache pipeline below. That used to spawn one `cp`
        # process per preview image. Copy the complete tree once, with one process,
        # and let generated gallery HTML point at /cache/kenney/... directly.
        if parse_content_count == 1 && !parsed_kenney.empty?
            FileUtils.mkdir_p('/webcache/kenney')
            unless system('cp', '-ru', '/src/content/anaglyph/kenney/.', '/webcache/kenney/')
                raise 'Failed to prepare Kenney preview cache'
            end
        end

        # Rendering a page: everything below for one entry of paths, into
        # parsed_content, parsed_content_paths and the catalog order
        # (parsed_sections, entries).
        render_entries = lambda do |list|
            list.each do |entry|
                entry_started_at = Process.clock_gettime(Process::CLOCK_MONOTONIC)
                section = entry[:section]
                path = entry[:original_path]
                # Kenney kit pages are built once; a later full parse (in
                # development, after a content change) keeps them instead of
                # dropping them, which made every kit page a 404 until a restart.
                if parse_content_count > 1 && entry[:kenney]
                    previous = defined?(@@content) && @@content && @@content[entry[:path]]
                    parsed_content[entry[:path]] = previous if previous
                    next
                end
                markdown = nil
                page_meta = {}
                page_references = []
                page_tasks = nil
                task_slots = []
                unless entry[:kenney]
                    next unless path
                    markdown = File.read(path)
                    markdown.gsub!(/_include_file\(([^)]+)\)/) do |match|
                        options = $1.split(',').map { |x| x.strip }
                        StringIO.open do |io|
                            io.puts "```#{options[1]}_lineno"
                            io.puts File.read(File.join(File.dirname(path), options[0]))
                            io.puts "```"
                            io.string
                        end
                    end
                    markdown.gsub!(/_include_svg\(([^)]+)\)/) do |match|
                        options = $1.split(',').map { |x| x.strip }
                        StringIO.open do |io|
                            svg = File.read(File.join(File.dirname(path), options[0]))
                            dom = Nokogiri::XML(svg)
                            dom.css('svg').first['class'] = (dom.css('svg').first['class'].to_s.split(' ') + ['full']).join(' ')
                            dom.css('svg').each do |s|
                                s.remove_attribute('width')
                                s.remove_attribute('height')
                            end
                            io.puts dom.css('svg').first.to_s
                            io.string
                        end
                    end
                    # Reference building blocks (docs/aufgaben.md): _include(slug#part)
                    # inserts a part of a reference page, _uses(a, b) links to parts,
                    # and part markers become anchors on the reference page itself.
                    page_meta = ContentParts.meta(markdown)
                    page_references = PageKinds.references_of(page_meta, markdown, ContentParts::INCLUDE,
                        :catalog => reference_sections.flat_map { |section| Array(section['entries']).map(&:to_s) })
                    content_warning = lambda { |message| STDERR.puts "#{path}: #{message}" }
                    page_included_parts = ContentParts.included_parts(markdown)
                    markdown = ContentParts.expand_includes(markdown,
                        :page_dir => File.dirname(path), :reference_root => REFERENCE_ROOT,
                        :variables => Main.content_variables(page_meta), :warn => content_warning)
                    markdown = ContentParts.expand_uses(markdown,
                        :references => page_meta['reference'], :reference_root => REFERENCE_ROOT,
                        :warn => content_warning)
                    markdown = ContentParts.fill_variables(markdown, Main.content_variables(page_meta))
                    markdown = ContentParts.filter_features(markdown, Main.content_features, :warn => content_warning)
                    # on the Personen page, the pages that place each person (before
                    # the part markers become anchors)
                    if path == PersonBoxes.file(REFERENCE_ROOT)
                        markdown = PersonBoxes.annotate(markdown, PersonBoxes.uses(person_pages), PersonBoxes.sources(path))
                    end
                    markdown = ContentParts.mark_parts(markdown)
                    # _person(name): a person box from the Handbuch page Personen.
                    markdown = PersonBoxes.expand(markdown, :page_dir => File.dirname(path),
                        :reference_root => REFERENCE_ROOT, :warn => content_warning)
                    # _tool(name): a box that opens a tool of the site in a new tab (Tools).
                    markdown = Tools.expand(markdown, :warn => content_warning)
                    # _schema(dataset): the tables of a dataset as schema lines (Schemas).
                    markdown = Schemas.expand(markdown, :warn => content_warning)
                    # Tasks (docs/aufgaben.md): tasks.yaml next to the page holds
                    # them, _task(id) on a line of its own places a card.
                    tasks_path = File.join(File.dirname(path), 'tasks.yaml')
                    if File.file?(tasks_path)
                        begin
                            task_data = TaskSpec.load_file(tasks_path)
                            page_tasks = {
                                :activity => task_data['activity'],
                                :by_id => task_data['tasks'].to_h { |t| [t['id'], t] },
                            }
                        rescue TaskSpec::Error, Psych::Exception => e
                            STDERR.puts "#{tasks_path}: tasks not loaded: #{e.message}"
                        end
                    end
                    # Parts for one programming language (CodeLanguages), wrapped after rendering.
                    markdown = CodeLanguages.mark(markdown, :warn => content_warning)
                    # Code to write first, folded under »Lösung anzeigen« (SolutionBlocks), wrapped after rendering.
                    markdown = SolutionBlocks.mark(markdown)
                    # Tips in steps before it (HintSteps), wrapped after rendering as well.
                    markdown = HintSteps.mark(markdown)
                    # What gives away a task's answer, until it is answered (AfterTask).
                    markdown = AfterTask.mark(markdown)
                    # Parts for people with a class repository over SSH or HTTPS,
                    # or without one (<!-- if: class_repo -->, ClassRepos): the
                    # page is rendered once for each, and each request picks one
                    # (page_html).
                    page_conditions = ContentParts.conditions(markdown)
                    if !page_conditions.empty? && !ClassRepos.page?(page_meta)
                        content_warning.call("<!-- if: class_repo --> on a page without class_repo: #{ClassRepos::META_VALUE}")
                    end
                    variant_markdowns = ClassRepos.variants(page_conditions).to_h do |variant|
                        [variant, ContentParts.filter_conditions(markdown, ClassRepos::CONDITION_VALUES[variant], :warn => variant.nil? ? content_warning : nil)]
                    end
                else
                    kit = entry[:path]
                    markdown = StringIO.open do |io|
                        title = kit.split('-').map { |x| x.capitalize}. join(' ')
                        io.puts "# #{title}"
                        io.puts
                        io.puts <<~EOS
                            Um das #{title} verwenden zu können, musst du es erst installieren. Öffne dazu ein Terminal, indem du <kbd>Strg</kbd><kbd>J</kbd> drückst. Führe dann den folgenden Befehl aus:

                            ```bash
                            ./download.rb #{kit}
                            ```

                            <div class='hint'>
                                Hinweis: Achte auf die genaue Schreibweise des Befehls.
                            </div>

                            Du kannst dann mit dem Befehl `model` ein Modell zu deiner Szene hinzufügen, also z. B.:

                            ```ini
                            model = #{kit}/#{parsed_kenney[kit].first}
                            ```

                            <div class='hint'>
                                Achtung: Vergiss nicht, den Namen des Kits (<code>#{kit}/</code>) vor dem Modellnamen anzugeben!
                            </div>

                            Die folgenden Modelle stehen zur Auswahl:

                            __KENNEY_GALLERY__
                        EOS
                        io.string
                    end
                end
                slug = entry[:slug] || File.basename(path, '.md').sub(/^[0-9]+\-/, '').sub('+', '')
                parsed_content_paths[slug] = path unless entry[:kenney]
                variant_markdowns = { nil => markdown } if entry[:kenney]
                page_languages = CodeLanguages.of_page(page_meta)
                variants = variant_markdowns.to_h do |variant, variant_markdown|
                    variant_markdown, task_slots = TaskCard.mark_slots(variant_markdown)
                    # At each step, the whole code for teachers (StageCode), on activities.
                    stage_points = []
                    if !entry[:kenney] && parsed_sections.dig(section, :catalog) != 'reference'
                        variant_markdown, stage_points = StageCode.mark(variant_markdown, File.dirname(path))
                    end
                    root = render_content_markdown(variant_markdown, path, redcarpet) do |md|
                        if entry[:kenney]
                            gallery = StringIO.open do |io|
                                io.puts "<div class='gallery tiles'>"
                                parsed_kenney[entry[:path]].each do |model|
                                    io.puts "<figure><img alt='Das Modell #{model}' src='/cache/kenney/#{entry[:path]}/#{model}.webp'><figcaption>#{model}</figcaption></figure>"
                                end
                                io.puts "</div>"
                                io.string
                            end
                            md.sub!('__KENNEY_GALLERY__', gallery)
                        end
                        md
                    end
                    CodeLanguages.wrap(root, :warn => lambda { |message| STDERR.puts "#{path}: #{message}" })
                    SolutionBlocks.wrap(root, :warn => lambda { |message| STDERR.puts "#{path}: #{message}" })
                    HintSteps.wrap(root, :warn => lambda { |message| STDERR.puts "#{path}: #{message}" })
                    AfterTask.wrap(root, :warn => lambda { |message| STDERR.puts "#{path}: #{message}" })
                    CodeLanguages.pair(root) if page_languages.empty?
                    insert_task_cards(root, task_slots, page_tasks, redcarpet, path, page_languages) unless task_slots.empty?
                    ExternalLinks.mark(root)    # also the links in the task cards
                    StageCode.insert(root, stage_points) unless stage_points.empty?
                    ClassRepos.mark_browse(root) if variant == ClassRepos::LOCAL_KEYS
                    # Reference pages: every h2 and h3 gets an anchor for the Handbuch index.
                    outline = parsed_sections.dig(section, :catalog) == 'reference' ? ReferenceIndex.anchor_headings(root) : nil
                    search_sections = outline ? ReferenceIndex.search_sections(root) : nil
                    # Optional chapters (<!-- extra --> before ##): badge, note, marks in the tables of contents
                    ExtraChapters.find(root, :warn => lambda { |message| STDERR.puts "#{path}: #{message}" })
                    inject_autotoc(root)
                    ExtraChapters.mark(root)
                    # Code in a table that is one short word ([0-9], \\d+, --force)
                    # stays in one piece instead of breaking at its hyphen when the
                    # table squeezes its column (default.css, .table code.whole)
                    root.css('table code').each do |code|
                        text = code.text
                        next if text =~ /\s/ || text.length > 24
                        code['class'] = [code['class'], 'whole'].compact.join(' ')
                    end

                    # Scripts a page loads itself (the colour mixer) get a fresh
                    # address with each version, like default.css: nginx lets
                    # browsers keep /include/*.js forever.
                    html = root.to_html.gsub(%r{(<script src=['"]/include/[a-z_]+\.js)(['"])}) do
                        "#{$1}?#{CACHE_BUSTER}#{DEVELOPMENT ? Time.now.to_i : nil}#{$2}"
                    end
                    [variant, { :root => root, :html => html, :task_slots => task_slots, :outline => outline, :search_sections => search_sections }]
                end
                root, html, task_slots, outline, search_sections = variants[nil].values_at(:root, :html, :task_slots, :outline, :search_sections)
                # The Handbuch search is the same for everyone: placeholders for
                # the class repository's variables.
                search_sections&.each { |section_entry| section_entry.transform_values! { |value| ClassRepos.neutral(value) } }
                # root.css('h2, h3, h4, h5, h6').each do |e|
                #     if slug == 'c'
                #         level = e.name.sub('h', '').to_i
                #         STDERR.puts "#{'  ' * (level - 2)}- #{e.text}"
                #     end
                # end
                meta = root.css('.meta').first
                entry_elapsed = Process.clock_gettime(Process::CLOCK_MONOTONIC) - entry_started_at
                STDERR.puts "Added content: #{slug} (#{format('%.3f', entry_elapsed)}s)"
                parsed_content[slug] = {
                    :html => html,
                    :dev_only => entry[:dev_only],
                    :teacher_only => parsed_sections.dig(section, :teacher_only) == true,
                    # in a section hidden from the catalogs (sections.yaml hidden: true)
                    :hidden_section => parsed_sections.dig(section, :hidden) == true,
                    # What kind of page this is (PageKinds.header shows it).
                    :section => section,
                    :kind => page_meta['kind'],
                    :language => page_meta['language'],
                    # Programming languages the page offers (CodeLanguages), [] for one.
                    :languages => page_languages,
                    :duration => page_meta['duration'],
                    # Activities this one builds on (requires:), information
                    # for the strip above the page and the catalog card.
                    :requires => Array(page_meta['requires']).map(&:to_s).reject(&:empty?),
                    :references => page_references,
                    # Reference parts the page includes ('slug#part'), so a box
                    # can shrink once someone has been through it elsewhere.
                    :included_parts => page_included_parts,
                    # Template to clone (template:, template_name:), for the
                    # »new version« hint; nil if the page has none.
                    :template => TemplateFreshness.from_meta(page_meta),
                    :outline => outline,
                    :search_sections => search_sections,
                    # search: words that find the page in the Handbuch search
                    # without standing in its title (Regex for »Reguläre Ausdrücke«)
                    :search_words => page_meta['search'].to_s.split(',').map(&:strip).reject(&:empty?),
                    # A set of Katas (KataMap): its idea in a few words, and the
                    # line of the curriculum teachers see on the overview.
                    :idea => page_meta['idea'],
                    :curriculum => page_meta['curriculum'],
                    # its topics (src/content/curricula/themen.yaml), from which
                    # the plan of another state derives its line (Curriculum)
                    :topics => Array(page_meta['topics']).map(&:to_s),
                    # whom the activity is for, where its curriculum line
                    # names both plans but it is meant for the younger
                    # (CurriculumGraph.age_of): Sek I, Sek I und Oberstufe,
                    # Oberstufe
                    :age => page_meta['age'],
                    # its icon in the strip: bi-braces, or a picture (/images/…)
                    :icon => page_meta['icon'].to_s[%r{\A(bi-[a-z0-9-]+|/images/[\w/.-]+\.(png|webp|svg))\z}],
                }
                if page_tasks
                    parsed_content[slug][:tasks] = page_tasks.merge(:on_page => task_slots & page_tasks[:by_id].keys)
                end
                # class_repo: group (ClassRepos): the variant for people without a
                # class repository, with its own HTML and tasks.
                parsed_content[slug][:class_repo] = ClassRepos.page?(page_meta)
                (variants.keys - [nil]).each do |variant|
                    v = variants[variant]
                    (parsed_content[slug][:variants] ||= {})[variant] = {
                        :html => v[:html],
                        :on_page => page_tasks ? v[:task_slots] & page_tasks[:by_id].keys : [],
                    }
                end
                # Teacher note next to the page (docs/aufgaben.md): rendered like the
                # page, shown only to teachers by teacher_note_panel.
                note_path = File.join(File.dirname(path), ContentParts::NOTE_FILE)
                if !entry[:kenney] && File.file?(note_path)
                    note_markdown = ContentParts.fill_variables(File.read(note_path), Main.content_variables({}, :per_viewer => false))
                    note_root = render_content_markdown(note_markdown, note_path, redcarpet)
                    # Inline code that is one short word (01-karte.c, plane_x)
                    # stays in one piece, in the note's tables and lists alike,
                    # as in the tables of a page (.teacher-note-body code.whole)
                    note_root.css('code').each do |code|
                        next if code.parent&.name == 'pre' || code.text =~ /\s/ || code.text.length > 24
                        code.add_class('whole')
                    end
                    parsed_content[slug][:teacher_note] = (note_root.at_css('body') || note_root).inner_html
                end
                unless entry[:extra]
                    parsed_sections[section][:entries] << slug
                    if meta
                        meta = YAML.load(meta)
                        if meta['image']
                            parts = meta['image'].split(':')
                            # name.webp:contain for a drawing: padded to 16:9 instead of cropped;
                            # name.webp:contain:x:y also sets what a phone's narrower box keeps
                            contain = parts[1] == 'contain'
                            parts = [parts[0], *parts[2..]] if contain
                            convert = contain ? method(:convert_image_padded) : method(:convert_image)
                            image_path = File.join(File.dirname(path), parts[0])
                            if File.file?(image_path)
                                sha1 = convert.call(image_path)
                                parsed_content[slug][:image] = "/cache/#{sha1}.webp"

                                # The dark file as the screenshot tool names it:
                                # box-anaglyph.webp → box-dark-anaglyph.webp.
                                extension = File.extname(image_path)
                                base = image_path.delete_suffix(extension)
                                dark_image_path = base.sub(/(-anaglyph)?\z/) { "-dark#{$1}" } + extension
                                if File.exist?(dark_image_path)
                                    dark_sha1 = convert.call(dark_image_path)
                                    parsed_content[slug][:image_dark] = "/cache/#{dark_sha1}.webp"
                                end
                            else
                                # A generated screenshot may not exist on first load.
                                parsed_content[slug][:image] = '/images/keyboard.png'
                            end

                            parsed_content[slug][:image_x] = (parts[1] || '50').to_i
                            parsed_content[slug][:image_y] = (parts[2] || '50').to_i
                            parsed_content[slug][:needs_contrast] = meta['needs_contrast']
                            # The card's alt text (docs/aufgaben.md): image_alt,
                            # else the alt of the same picture on the page.
                            basename = File.basename(parts[0])
                            same = root.css('img').find { |img| File.basename(img['src'].to_s) == basename && !img['alt'].to_s.strip.empty? }
                            parsed_content[slug][:image_alt] = meta['image_alt'] || (same && same['alt'])
                        end
                    end
                    begin
                        parsed_content[slug][:title] = root.css('h1').first.to_s.sub('<h1>', '').sub('</h1>', '').strip
                    rescue
                    end
                    begin
                        parsed_content[slug][:abstract] = root.css('.abstract').first.text
                    rescue
                    end
                    begin
                        unless parsed_content[slug][:image]
                            parsed_content[slug][:image_alt] = root.css('img').first.attr('alt')
                        end
                        parsed_content[slug][:image] ||= root.css('img').first.attr('src')
                        parsed_content[slug][:image_x] ||= 50
                        parsed_content[slug][:image_y] ||= 50
                    rescue
                    end
                end
            end
        end

        # Rendering every page is most of a start (about ten seconds for 160
        # pages, one after another). On a full parse several processes share
        # the pages, dealt in turn: each renders its part and hands it back
        # (Marshal), and leaves with exit!, so it closes nothing it shares
        # (connections, Puma does not run yet). A part whose process fails
        # is rendered here afterwards; that only costs time.
        workers = partial || paths.size < 20 ? 1 : [Etc.nprocessors, 8].min
        if workers > 1
            # dealt in alphabetical order, so that the »Added content« lines
            # of the workers come roughly in that order and show the progress;
            # the catalog keeps the order of paths (sorted below)
            sorted = paths.sort_by { |entry| entry[:slug] || entry[:path] }
            children = sorted.each_with_index.group_by { |_, i| i % workers }.values.map { |part| part.map(&:first) }.map do |part|
                reader, writer = IO.pipe
                pid = fork do
                    reader.close
                    begin
                        render_entries.call(part)
                        entries = parsed_sections.transform_values { |s| s[:entries] }
                        writer.write(Marshal.dump([parsed_content, parsed_content_paths, entries]))
                        writer.close
                        exit!(0)
                    rescue Exception => e
                        STDERR.puts "Rendering content in a worker failed: #{e.class}: #{e.message}"
                        exit!(1)
                    end
                end
                writer.close
                [pid, reader, part]
            end
            children.each do |pid, reader, part|
                data = reader.read
                reader.close
                Process.wait(pid)
                if $?.success? && !data.empty?
                    content, content_paths, entries = Marshal.load(data)
                    parsed_content.merge!(content)
                    parsed_content_paths.merge!(content_paths)
                    entries.each_pair { |key, slugs| parsed_sections[key][:entries].concat(slugs) }
                else
                    render_entries.call(part)
                end
            end
            # the catalog in the order of paths, as one process would make it
            order = paths.each_with_index.to_h { |entry, i| [entry[:original_path], i] }
            parsed_sections.each_value { |s| s[:entries].sort_by! { |slug| order[parsed_content_paths[slug]] || 0 } }
        else
            render_entries.call(paths)
        end
        if partial
            parsed_content.each_pair do |slug, content|
                @@content[slug] = content
            end
            parsed_content_paths.each_pair do |slug, path|
                @@content_paths[slug] = path
            end
        else
            # Publish only complete parse results. Concurrent requests continue
            # to read the previous complete snapshot while this method is working.
            @@section_order = section_order
            @@teaching_section_order = teaching_section_order
            @@reference_section_order = reference_section_order
            @@sections = parsed_sections
            @@kenney = parsed_kenney
            @@content = parsed_content
            @@content_paths = parsed_content_paths
            @@content_catalog_signature = content_catalog_signature
            if cache_file
                begin
                    FileUtils.mkdir_p(CONTENT_CACHE)
                    data = Marshal.dump([section_order, teaching_section_order, reference_section_order, parsed_sections, parsed_kenney, parsed_content, parsed_content_paths])
                    File.binwrite("#{cache_file}.tmp", data)
                    File.rename("#{cache_file}.tmp", cache_file)
                    (Dir[File.join(CONTENT_CACHE, '*.marshal')] - [cache_file]).each { |old| File.delete(old) rescue nil }
                rescue StandardError => e
                    STDERR.puts "Content cache not written (#{e.class}: #{e.message})"
                end
            end
        end

        parse_elapsed = Process.clock_gettime(Process::CLOCK_MONOTONIC) - parse_started_at
        label = partial ? "content #{only_slug}" : 'content'
        STDERR.puts "Finished parsing #{label} in #{format('%.3f', parse_elapsed)}s"
        true
    end
    private_class_method :parse_content_locked
end
