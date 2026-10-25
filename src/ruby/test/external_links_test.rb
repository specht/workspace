require 'minitest/autorun'
begin
    require 'nokogiri'
rescue LoadError
    nil
end
require_relative '../include/external_links' if defined?(Nokogiri)

class ExternalLinksTest < Minitest::Test
    def setup
        skip 'nokogiri only in the Ruby container' unless defined?(Nokogiri)
    end

    def test_other_sites_open_in_a_new_tab_the_site_itself_not
        root = Nokogiri::HTML5.fragment("<a href='https://www.rfc-editor.org/rfc/rfc2324.html'>RFC 2324</a>" \
            "<a href='http://example.org'>alt</a><a href='/raytracing'>intern</a><a href='#oben'>oben</a>")
        ExternalLinks.mark(root)
        links = root.css('a').map { |a| [a['href'], a['target'], a['rel']] }
        assert_equal ['https://www.rfc-editor.org/rfc/rfc2324.html', '_blank', 'noopener'], links[0]
        assert_equal ['http://example.org', '_blank', 'noopener'], links[1]
        assert_equal ['/raytracing', nil, nil], links[2]
        assert_equal ['#oben', nil, nil], links[3]
    end
end
