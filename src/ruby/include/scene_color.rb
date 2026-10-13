# Colours in Anaglyph scenes (check type scene): what a fill or background
# from scene_dump.js is, as RGB, and which family a person would call it
# (»gelb«, »blau«). Families let a task ask for »a yellow sun« without
# caring whether it is `yellow`, `gold`, `#f9c80e` or `1, 0.8, 0`.
module SceneColor
    # The 148 CSS colour names (CSS Color Module Level 4), as the browser knows them.
    CSS_NAMES = {
        'aliceblue' => '#f0f8ff', 'antiquewhite' => '#faebd7', 'aqua' => '#00ffff',
        'aquamarine' => '#7fffd4', 'azure' => '#f0ffff', 'beige' => '#f5f5dc',
        'bisque' => '#ffe4c4', 'black' => '#000000', 'blanchedalmond' => '#ffebcd',
        'blue' => '#0000ff', 'blueviolet' => '#8a2be2', 'brown' => '#a52a2a',
        'burlywood' => '#deb887', 'cadetblue' => '#5f9ea0', 'chartreuse' => '#7fff00',
        'chocolate' => '#d2691e', 'coral' => '#ff7f50', 'cornflowerblue' => '#6495ed',
        'cornsilk' => '#fff8dc', 'crimson' => '#dc143c', 'cyan' => '#00ffff',
        'darkblue' => '#00008b', 'darkcyan' => '#008b8b', 'darkgoldenrod' => '#b8860b',
        'darkgray' => '#a9a9a9', 'darkgreen' => '#006400', 'darkgrey' => '#a9a9a9',
        'darkkhaki' => '#bdb76b', 'darkmagenta' => '#8b008b', 'darkolivegreen' => '#556b2f',
        'darkorange' => '#ff8c00', 'darkorchid' => '#9932cc', 'darkred' => '#8b0000',
        'darksalmon' => '#e9967a', 'darkseagreen' => '#8fbc8f', 'darkslateblue' => '#483d8b',
        'darkslategray' => '#2f4f4f', 'darkslategrey' => '#2f4f4f', 'darkturquoise' => '#00ced1',
        'darkviolet' => '#9400d3', 'deeppink' => '#ff1493', 'deepskyblue' => '#00bfff',
        'dimgray' => '#696969', 'dimgrey' => '#696969', 'dodgerblue' => '#1e90ff',
        'firebrick' => '#b22222', 'floralwhite' => '#fffaf0', 'forestgreen' => '#228b22',
        'fuchsia' => '#ff00ff', 'gainsboro' => '#dcdcdc', 'ghostwhite' => '#f8f8ff',
        'gold' => '#ffd700', 'goldenrod' => '#daa520', 'gray' => '#808080', 'green' => '#008000',
        'greenyellow' => '#adff2f', 'grey' => '#808080', 'honeydew' => '#f0fff0',
        'hotpink' => '#ff69b4', 'indianred' => '#cd5c5c', 'indigo' => '#4b0082',
        'ivory' => '#fffff0', 'khaki' => '#f0e68c', 'lavender' => '#e6e6fa',
        'lavenderblush' => '#fff0f5', 'lawngreen' => '#7cfc00', 'lemonchiffon' => '#fffacd',
        'lightblue' => '#add8e6', 'lightcoral' => '#f08080', 'lightcyan' => '#e0ffff',
        'lightgoldenrodyellow' => '#fafad2', 'lightgray' => '#d3d3d3', 'lightgreen' => '#90ee90',
        'lightgrey' => '#d3d3d3', 'lightpink' => '#ffb6c1', 'lightsalmon' => '#ffa07a',
        'lightseagreen' => '#20b2aa', 'lightskyblue' => '#87cefa', 'lightslategray' => '#778899',
        'lightslategrey' => '#778899', 'lightsteelblue' => '#b0c4de', 'lightyellow' => '#ffffe0',
        'lime' => '#00ff00', 'limegreen' => '#32cd32', 'linen' => '#faf0e6', 'magenta' => '#ff00ff',
        'maroon' => '#800000', 'mediumaquamarine' => '#66cdaa', 'mediumblue' => '#0000cd',
        'mediumorchid' => '#ba55d3', 'mediumpurple' => '#9370db', 'mediumseagreen' => '#3cb371',
        'mediumslateblue' => '#7b68ee', 'mediumspringgreen' => '#00fa9a',
        'mediumturquoise' => '#48d1cc', 'mediumvioletred' => '#c71585', 'midnightblue' => '#191970',
        'mintcream' => '#f5fffa', 'mistyrose' => '#ffe4e1', 'moccasin' => '#ffe4b5',
        'navajowhite' => '#ffdead', 'navy' => '#000080', 'oldlace' => '#fdf5e6',
        'olive' => '#808000', 'olivedrab' => '#6b8e23', 'orange' => '#ffa500',
        'orangered' => '#ff4500', 'orchid' => '#da70d6', 'palegoldenrod' => '#eee8aa',
        'palegreen' => '#98fb98', 'paleturquoise' => '#afeeee', 'palevioletred' => '#db7093',
        'papayawhip' => '#ffefd5', 'peachpuff' => '#ffdab9', 'peru' => '#cd853f',
        'pink' => '#ffc0cb', 'plum' => '#dda0dd', 'powderblue' => '#b0e0e6', 'purple' => '#800080',
        'rebeccapurple' => '#663399', 'red' => '#ff0000', 'rosybrown' => '#bc8f8f',
        'royalblue' => '#4169e1', 'saddlebrown' => '#8b4513', 'salmon' => '#fa8072',
        'sandybrown' => '#f4a460', 'seagreen' => '#2e8b57', 'seashell' => '#fff5ee',
        'sienna' => '#a0522d', 'silver' => '#c0c0c0', 'skyblue' => '#87ceeb',
        'slateblue' => '#6a5acd', 'slategray' => '#708090', 'slategrey' => '#708090',
        'snow' => '#fffafa', 'springgreen' => '#00ff7f', 'steelblue' => '#4682b4',
        'tan' => '#d2b48c', 'teal' => '#008080', 'thistle' => '#d8bfd8', 'tomato' => '#ff6347',
        'turquoise' => '#40e0d0', 'violet' => '#ee82ee', 'wheat' => '#f5deb3', 'white' => '#ffffff',
        'whitesmoke' => '#f5f5f5', 'yellow' => '#ffff00', 'yellowgreen' => '#9acd32',
    }.freeze

    FAMILIES = %w[white grey black red orange brown yellow green cyan blue purple pink].freeze
    GERMAN = {
        'white' => 'Weiß', 'grey' => 'Grau', 'black' => 'Schwarz', 'red' => 'Rot',
        'orange' => 'Orange', 'brown' => 'Braun', 'yellow' => 'Gelb', 'green' => 'Grün',
        'cyan' => 'Türkis', 'blue' => 'Blau', 'purple' => 'Lila', 'pink' => 'Pink',
    }.freeze

    # [r, g, b] with 0..255, or nil (no fill, or not a colour the server knows).
    # value: { 'rgb' => [0..1, …] }, { 'css' => text } or 'off', as dumped.
    def self.rgb(value)
        return nil unless value.is_a?(Hash)

        if value['rgb'].is_a?(Array) && value['rgb'].size == 3
            return value['rgb'].map { |v| (v.to_f.clamp(0, 1) * 255).round }
        end
        css(value['css'].to_s)
    end

    def self.css(text)
        text = text.strip.downcase
        text = CSS_NAMES[text] if CSS_NAMES.key?(text)
        case text
        when /\A#([0-9a-f])([0-9a-f])([0-9a-f])([0-9a-f])?\z/
            [$1, $2, $3].map { |h| (h * 2).to_i(16) }
        when /\A#([0-9a-f]{2})([0-9a-f]{2})([0-9a-f]{2})([0-9a-f]{2})?\z/
            [$1, $2, $3].map { |h| h.to_i(16) }
        when /\Argba?\(\s*(\d+(?:\.\d+)?)\s*[, ]\s*(\d+(?:\.\d+)?)\s*[, ]\s*(\d+(?:\.\d+)?)/
            [$1, $2, $3].map { |v| v.to_f.clamp(0, 255).round }
        end
    end

    # Hue in degrees, saturation and lightness 0..1 (HSL).
    def self.hsl(rgb)
        r, g, b = rgb.map { |v| v / 255.0 }
        max, min = [r, g, b].max, [r, g, b].min
        l = (max + min) / 2
        d = max - min
        return [0.0, 0.0, l] if d < 1e-9

        s = d / (1 - (2 * l - 1).abs)
        h = case max
            when r then ((g - b) / d) % 6
            when g then (b - r) / d + 2
            else (r - g) / d + 4
        end
        [h * 60, s, l]
    end

    # Which family a colour belongs to. Pale or dull colours count as white,
    # grey or black; dark orange is brown.
    def self.family(rgb)
        return nil if rgb.nil?

        h, s, l = hsl(rgb)
        if s < 0.2 || l > 0.93 || l < 0.08
            return 'white' if l >= 0.85
            return 'black' if l <= 0.15

            return 'grey'
        end
        return 'pink' if h >= 315 && h < 345 || (h >= 345 || h < 15) && l > 0.75
        if h >= 345 || h < 15
            # CSS brown (#a52a2a) is a dull dark red; darkred is not brown.
            return 'brown' if l < 0.42 && s < 0.7

            return 'red'
        end
        if h < 45
            return 'brown' if l < 0.4

            return 'orange'
        end
        return 'yellow' if h < 70
        return 'green' if h < 165
        return 'cyan' if h < 195
        return 'blue' if h < 255

        'purple'
    end

    def self.hex(rgb)
        rgb && format('#%02x%02x%02x', *rgb)
    end

    # How the person wrote the colour, and what it is: »yellow (#ffff00, Gelb)«.
    def self.describe(value)
        return 'keine Füllung' if value == 'off'

        rgb = rgb(value)
        written = value.is_a?(Hash) && value['css'] ? value['css'] : nil
        return "#{written} (unbekannte Farbe)" if rgb.nil?

        family = GERMAN[family(rgb)]
        return "#{hex(rgb)} (#{family})" if written.nil? || written == hex(rgb)

        "#{written} (#{hex(rgb)}, #{family})"
    end

    # »Gelb oder Orange«
    def self.german_list(families)
        names = Array(families).map { |f| GERMAN[f] || f }
        names.size > 1 ? "#{names[0..-2].join(', ')} oder #{names[-1]}" : names.first.to_s
    end
end
