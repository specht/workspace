require 'cgi'
require 'set'

# Small variants of the pictures on the overview cards. A card shows its
# picture at about 130 px (phone) to 420 px (desktop), the catalog has about
# a hundred of them, so the full pictures would be megabytes for a class on
# school WiFi. Beside each picture in the image cache (/webcache/HASH.webp,
# Main.convert_image) lie HASH-480x270.webp and HASH-960x540.webp, lossy
# WebP, never larger than the picture; the card names them in srcset and the
# browser takes the one that fits its width and pixel density: 480 for a
# phone (130 px at three device pixels each) and a desktop, 960 for a desktop
# or tablet with two device pixels per CSS pixel.
#
# A card crops its picture to 16:9 (object-fit: cover), so a variant fills a
# 16:9 box of its width instead of matching only the width: a wide picture
# (1852 x 428, the torus of the anaglyphs) keeps 270 px of height in the 480
# variant, not 111 px that the card would stretch to 243. Such a variant is
# wider than its w in srcset, which names the box it was made for.
#
# The hash is the picture's content, so a changed picture gets a new name
# and new variants; a variant older than its picture is made again. Missing
# variants are made by one background thread: the page does not wait, its
# srcset lists only the variants that exist, and until there is one, the
# card shows the picture itself.
module CardImages
    WIDTHS = [480, 960].freeze
    # The box of a card's picture (default.css): 16:9.
    BOX = 9.0 / 16
    QUALITY = 82
    URL_PATTERN = %r{\A/cache/([0-9a-f]{16})\.webp\z}

    # How wide a card's picture is shown (default.css, .hscol and
    # .tutorial_card3): a third of the row from 1200 px (at most 432 px, the
    # widest container), half of it from 768 px, on a phone 36 % of the card
    # beside its text.
    CARD_SIZES = '(max-width: 767.98px) 36vw, (max-width: 1199.98px) 50vw, min(33vw, 432px)'
    # The logos of the language tiles: two in a row below 576 px, else four.
    TILE_SIZES = '(max-width: 575.98px) 50vw, 25vw'

    @cache_dir = '/webcache'
    @queue = Queue.new
    @pending = Set.new
    @mutex = Mutex.new
    @worker = nil

    class << self
        attr_accessor :cache_dir

        # The hash of a picture in the image cache, or nil for anything else
        # (a picture from /images, a path with other characters).
        def cache_hash(url)
            url.to_s[URL_PATTERN, 1]
        end

        def source_path(hash)
            File.join(cache_dir, "#{hash}.webp")
        end

        def box_height(width)
            (width * BOX).round
        end

        # Named after the box, so a change of the box makes new variants.
        def variant_name(hash, width)
            "#{hash}-#{width}x#{box_height(width)}.webp"
        end

        def variant_path(hash, width)
            File.join(cache_dir, variant_name(hash, width))
        end

        def variant_current?(hash, width)
            target = variant_path(hash, width)
            File.file?(target) && File.mtime(target) >= File.mtime(source_path(hash))
        end

        # src, srcset and sizes for the <img> of a card, escaped for an
        # attribute in single quotes. A picture outside the image cache keeps
        # its address and gets no srcset.
        def attributes(url, sizes: CARD_SIZES, wait: false)
            hash = cache_hash(url)
            return "src='#{CGI.escapeHTML(url.to_s)}'" unless hash && File.file?(source_path(hash))

            ready = WIDTHS.select do |width|
                next true if variant_current?(hash, width)
                wait ? make_variant(hash, width) : enqueue(hash, width)
                wait && variant_current?(hash, width)
            end
            return "src='#{url}'" if ready.empty?

            srcset = ready.map { |width| "/cache/#{variant_name(hash, width)} #{width}w" }.join(', ')
            # For a browser without srcset the smallest.
            "src='/cache/#{variant_name(hash, ready.first)}' srcset='#{srcset}' sizes='#{sizes}'"
        end

        # Scales the cached picture down until it just covers the box of
        # width (^: fill, >: never up), lossy WebP. Written under another
        # name first, so nginx never serves half a file.
        def make_variant(hash, width)
            source = source_path(hash)
            return false unless File.file?(source)

            target = variant_path(hash, width)
            temporary = "#{target.delete_suffix('.webp')}.#{Process.pid}.#{Thread.current.object_id}.tmp.webp"
            ok = system('convert', "#{source}[0]", '-resize', "#{width}x#{box_height(width)}^>", '-strip',
                '-quality', QUALITY.to_s, '-define', 'webp:lossless=false', temporary,
                [:out, :err] => File::NULL)
            if ok && File.file?(temporary)
                File.rename(temporary, target)
                true
            else
                STDERR.puts "CardImages: scaling #{source} to #{width} px failed"
                File.delete(temporary) if File.exist?(temporary)
                false
            end
        rescue SystemCallError => e
            STDERR.puts "CardImages: #{e.message}"
            false
        end

        # Waits until the background thread has made every variant asked for
        # so far (for tests and scripts).
        def drain
            sleep 0.05 until @mutex.synchronize { @pending.empty? }
        end

        private

        def enqueue(hash, width)
            key = [hash, width]
            @mutex.synchronize do
                return if @pending.include?(key)
                @pending << key
                @worker = Thread.new { work } unless @worker&.alive?
            end
            @queue << key
        end

        def work
            loop do
                key = @queue.pop
                begin
                    make_variant(*key) unless variant_current?(*key)
                rescue StandardError => e
                    STDERR.puts "CardImages: #{e.message}"
                ensure
                    @mutex.synchronize { @pending.delete(key) }
                end
            end
        end
    end
end
