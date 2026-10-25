require 'minitest/autorun'
require 'tmpdir'
require 'open3'
require_relative '../include/card_images'

class CardImagesTest < Minitest::Test
    HASH = '0123456789abcdef'

    def setup
        @dir = Dir.mktmpdir
        @old_dir = CardImages.cache_dir
        CardImages.cache_dir = @dir
    end

    def teardown
        CardImages.drain
        CardImages.cache_dir = @old_dir
        FileUtils.rm_rf(@dir)
    end

    def convert_available?
        system('convert', '-version', [:out, :err] => File::NULL)
    end

    def make_source(width, height)
        skip 'ImageMagick (convert) is not installed' unless convert_available?
        assert system('convert', '-size', "#{width}x#{height}", 'gradient:red-blue', '-quality', '100',
            CardImages.source_path(HASH))
    end

    def size_of(path)
        out, = Open3.capture2('identify', '-format', '%w %h', path)
        out.split.map(&:to_i)
    end

    def test_only_cached_pictures_get_variants
        assert_nil CardImages.cache_hash('/images/white.webp')
        assert_nil CardImages.cache_hash('/cache/../etc/passwd.webp')
        assert_nil CardImages.cache_hash("/cache/#{HASH}.webp/../x.webp")
        assert_nil CardImages.cache_hash("/cache/#{HASH}-480x270.webp")
        assert_equal HASH, CardImages.cache_hash("/cache/#{HASH}.webp")
        assert_equal "src='/images/keyboard.png'", CardImages.attributes('/images/keyboard.png')
    end

    def test_a_picture_missing_from_the_cache_keeps_its_address
        assert_equal "src='/cache/#{HASH}.webp'", CardImages.attributes("/cache/#{HASH}.webp")
        assert_empty Dir.children(@dir)
    end

    def test_variants_are_made_in_the_background_and_listed_once_they_exist
        make_source(1600, 900)
        url = "/cache/#{HASH}.webp"
        # The first page does not wait: the picture itself.
        assert_equal "src='#{url}'", CardImages.attributes(url)
        CardImages.drain
        attributes = CardImages.attributes(url)
        assert_includes attributes, "src='/cache/#{HASH}-480x270.webp'"
        assert_includes attributes, "srcset='/cache/#{HASH}-480x270.webp 480w, /cache/#{HASH}-960x540.webp 960w'"
        assert_includes attributes, "sizes='#{CardImages::CARD_SIZES}'"
        assert_equal [480, 270], size_of(CardImages.variant_path(HASH, 480))
        assert_equal [960, 540], size_of(CardImages.variant_path(HASH, 960))
        # lossy, not the lossless the source's quality 100 would give
        assert_equal 'VP8 ', File.binread(CardImages.variant_path(HASH, 960), 16)[12, 4]
        assert_empty Dir.children(@dir).grep(/tmp/)
    end

    def test_a_small_picture_is_not_scaled_up
        make_source(360, 203)
        CardImages.attributes("/cache/#{HASH}.webp", :wait => true)
        assert_equal [360, 203], size_of(CardImages.variant_path(HASH, 480))
        assert_equal [360, 203], size_of(CardImages.variant_path(HASH, 960))
    end

    # The card crops to 16:9; a wide picture keeps the height of that box
    # (the torus of the anaglyphs, 1852 x 428), but is never scaled up.
    def test_a_wide_picture_keeps_the_height_of_the_box
        make_source(1852, 428)
        CardImages.attributes("/cache/#{HASH}.webp", :wait => true)
        assert_equal [1168, 270], size_of(CardImages.variant_path(HASH, 480))
        assert_equal [1852, 428], size_of(CardImages.variant_path(HASH, 960))
        # a tall picture is still scaled by its width
        make_source(900, 1600)
        File.utime(Time.now + 5, Time.now + 5, CardImages.source_path(HASH))
        CardImages.attributes("/cache/#{HASH}.webp", :wait => true)
        assert_equal [480, 853], size_of(CardImages.variant_path(HASH, 480))
    end

    def test_a_variant_older_than_its_picture_is_made_again
        make_source(1600, 900)
        url = "/cache/#{HASH}.webp"
        CardImages.attributes(url, :wait => true)
        variant = CardImages.variant_path(HASH, 480)
        File.utime(Time.now - 3600, Time.now - 3600, variant)
        refute CardImages.variant_current?(HASH, 480)
        # until the new one is there, the srcset leaves the old one out
        refute_includes CardImages.attributes(url), "#{HASH}-480x270.webp"
        CardImages.drain
        assert CardImages.variant_current?(HASH, 480)
        assert_includes CardImages.attributes(url), "#{HASH}-480x270.webp 480w"
    end
end
