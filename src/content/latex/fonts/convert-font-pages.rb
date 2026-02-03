#!/usr/bin/env ruby

fonts = []
fonts << 'latin-modern-math'
fonts << 'xcharter-math'
fonts << 'stix-two-math'
fonts << 'tex-gyre-pagella-math'
fonts << 'tex-gyre-termes-math'
fonts << 'tex-gyre-schola-math'
fonts << 'tex-gyre-bonum-math'
fonts << 'latin-modern-roman'
fonts << 'latin-modern-sans'
fonts << 'latin-modern-mono'
fonts << 'cmu-serif'
fonts << 'cmu-sans-serif'
fonts << 'cmu-typewriter-text'
fonts << 'nimbus-roman'
fonts << 'vollkorn'
fonts << 'inter'
fonts << 'ibm-plex-sans'
fonts << 'ubuntu'
fonts << 'liberation-serif'
fonts << 'liberation-sans'
# fonts << 'caladea'
fonts << 'carlito'
fonts << 'jetbrains-mono'
fonts << 'anonymous-pro'
fonts << 'ibm-plex-mono'
fonts << 'atkinson-hyperlegible'
fonts << 'comic-neue'
fonts << 'montserrat'
fonts << 'comfortaa'
fonts << 'gfs-bodoni'
fonts << 'gfs-didot'
fonts << 'gfs-neohellenic'

fonts.each.with_index do |font, index|
    page = index + 1
    system("ruby pdf_page_to_svg.rb fonts.pdf #{page} #{font}.svg")
    system("magick -background white -density 300 #{font}.svg -quality 90 #{font}.webp")
end