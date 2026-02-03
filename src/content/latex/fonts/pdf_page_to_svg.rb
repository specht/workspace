#!/usr/bin/env ruby
# Usage: ruby pdf_page_to_svg.rb input.pdf page_number output.svg

require "tmpdir"

input_pdf  = ARGV[0] or abort "Missing input PDF"
page       = (ARGV[1] || "1").to_i
output_svg = ARGV[2] or abort "Missing output SVG"

# 1 mm in bp (pdfcrop units)
SHAVE_BP = 2.8

Dir.mktmpdir do |dir|
  page_pdf    = File.join(dir, "page.pdf")
  cropped_pdf = File.join(dir, "cropped.pdf")
  shaved_pdf  = File.join(dir, "shaved.pdf")

  # 1) Extract single page
  system(
    "qpdf",
    input_pdf,
    "--pages", input_pdf, page.to_s, "--",
    page_pdf
  ) or abort "qpdf page extraction failed"

  # 2) Crop to content
  system(
    "pdfcrop",
    page_pdf,
    cropped_pdf
  ) or abort "pdfcrop failed (content crop)"

  # 3) Shave off 1 mm top and bottom
  # margins: left top right bottom (in bp)
  system(
    "pdfcrop",
    "--margins", "0 -#{SHAVE_BP} 0 -#{SHAVE_BP}",
    cropped_pdf,
    shaved_pdf
  ) or abort "pdfcrop failed (shave)"

  # 4) Convert to SVG
  system(
    "pdf2svg",
    shaved_pdf,
    output_svg,
    "1"
  ) or abort "pdf2svg failed"
end

puts "✔ SVG written to #{output_svg}"
