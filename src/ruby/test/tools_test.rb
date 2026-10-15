require 'minitest/autorun'
require 'nokogiri'
require_relative '../include/tools'

class ToolsTest < Minitest::Test
    def test_tool_line_becomes_a_box_that_opens_a_new_tab
        html = Tools.expand("Vorher\n\n_tool(tcp-console)\n\nNachher\n")
        assert_includes html, "<a class='tool-card' href='/tcp-console' target='_blank' rel='noopener'>"
        assert_includes html, 'TCP-Konsole'
        assert_includes html, "Vorher\n"
    end

    def test_unknown_tool_is_reported_and_left
        warnings = []
        text = Tools.expand("_tool(nonexistent)\n", :warn => ->(m) { warnings << m })
        assert_equal "_tool(nonexistent)\n", text
        assert_equal ['_tool(nonexistent): no such tool'], warnings
    end

    def test_only_a_line_of_its_own_counts
        assert_equal 'Siehe _tool(automatron) hier.', Tools.expand('Siehe _tool(automatron) hier.')
    end

    def test_links_to_tools_open_a_new_tab_and_others_stay
        root = Nokogiri::HTML("<a href='/automatron#x'>A</a><a href='/tcpip'>B</a><a href='/tcp-console'>C</a>")
        Tools.link_targets(root)
        assert_equal ['_blank', nil, '_blank'], root.css('a').map { |a| a['target'] }
        assert_equal 'noopener', root.css('a').first['rel']
    end
end
