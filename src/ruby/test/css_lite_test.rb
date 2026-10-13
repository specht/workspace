require 'minitest/autorun'
require_relative '../include/css_lite'

class CssLiteTest < Minitest::Test
    GOOD = <<~CSS
        @import url("fonts/roboto.css");
        /* Vorlage */
        .shower { --slide-ratio: calc(16 / 9); }
        @media print {
            .slide { break-after: page; }
        }
        @font-face { font-family: Roboto; src: url('fonts/Roboto.woff2'); }
        /* Eigene CSS-Regeln */
        h2 {
            color: #c00;
            font-family: "Comic Neue", sans-serif;
        }
        .slide img.rechts { float: right }
    CSS

    def test_good_css
        r = CssLite.parse(GOOD)
        assert_empty r.problems.map(&:message)
        assert_equal ['.shower', '.slide', '@font-face', 'h2', '.slide img.rechts'], r.rules.map(&:selector)
        assert_equal 9, r.rules[3].line
        assert_equal [['color', '#c00'], ['font-family', '"Comic Neue", sans-serif']], r.rules[3].declarations
        assert_equal ['fonts/roboto.css', 'fonts/Roboto.woff2'], r.urls.map(&:first)
        assert_equal [['fonts/roboto.css', 1]], r.imports
    end

    def test_missing_semicolon
        r = CssLite.parse("h2 {\n  color: red\n  font-size: 2em;\n}\n")
        assert_equal [2], r.problems.map(&:line)
        assert_match(/fehlt vermutlich ein Semikolon/, r.problems.first.message)
    end

    def test_missing_colon_and_value
        r = CssLite.parse("h2 { color red; margin: ; }")
        assert_equal 2, r.problems.size
        assert_match(/fehlt der Doppelpunkt/, r.problems[0].message)
        assert_match(/Bei `margin` fehlt der Wert/, r.problems[1].message)
    end

    def test_braces
        # A missing } looks like CSS nesting, so the open { is reported.
        r = CssLite.parse("h2 {\n color: red;\n\np { color: blue; }\n")
        assert_equal ['Das `{` aus Zeile 1 wird nie mit `}` geschlossen.'], r.problems.map(&:message)
        r = CssLite.parse(".card { color: red; &:hover { color: blue; } p { margin: 0 } }")
        assert_empty r.problems
        assert_equal ['.card', '&:hover', 'p'], r.rules.map(&:selector)
        assert_equal [['color', 'red']], r.rules[0].declarations
        r = CssLite.parse("h2 { color: red; }\n}\n")
        assert_match(/schließt nichts/, r.problems.first.message)
        assert_equal 2, r.problems.first.line
        r = CssLite.parse("h2 { color: red;")
        assert_match(/aus Zeile 1 wird nie mit `}` geschlossen/, r.problems.first.message)
        r = CssLite.parse("{ color: red; }")
        assert_match(/fehlt der Selektor/, r.problems.first.message)
    end

    def test_comment_and_string
        assert_match(/nie mit `\*\/`/, CssLite.parse("h2 { color: red; }\n/* offen").problems.first.message)
        assert_match(/nicht in derselben Zeile/, CssLite.parse("h2 { font-family: \"Arial; }\np { }").problems.first.message)
        assert_empty CssLite.parse("h2::before { content: '{ ; }'; background: url(data:image/png;base64,AAA); }").problems
    end

    def test_declaration_outside_rule
        r = CssLite.parse("color: red;\nh2 { }")
        assert_match(/außerhalb einer Regel/, r.problems.first.message)
    end
end
