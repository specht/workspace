require 'cgi'
require 'digest'

# Icons from the bundled Tabler sprite (src/static/include/tabler/tabler.svg,
# list in icons.txt). Only icons in the sprite can be used.
module Icons
    # The address carries the sprite's version: nginx lets browsers keep
    # images forever, and a sprite with new icons must be fetched again.
    SPRITE_FILE = File.expand_path('../../static/include/tabler/tabler.svg', __dir__)
    SPRITE = "/include/tabler/tabler.svg?v=#{File.exist?(SPRITE_FILE) ? Digest::SHA256.file(SPRITE_FILE).hexdigest[0, 10] : '0'}"

    # <svg> that shows one icon in the current text colour. With label, it is
    # announced to screen readers; without, it is decoration.
    def self.tabler(id, label: nil, klass: nil)
        a11y = label ? "role='img' aria-label='#{CGI.escapeHTML(label)}'" : "aria-hidden='true'"
        "<svg class='ti ti-#{CGI.escapeHTML(id)}#{klass ? " #{CGI.escapeHTML(klass)}" : ''}' #{a11y}><use href='#{SPRITE}##{CGI.escapeHTML(id)}'/></svg>"
    end
end
