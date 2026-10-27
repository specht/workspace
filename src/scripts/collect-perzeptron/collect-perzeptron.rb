#!/usr/bin/env ruby
# frozen_string_literal: true

# Builds the data of »Ein Neuron lernt« (src/content/perzeptron), served
# from src/static/perzeptron:
#
# - penguins.csv: the Palmer penguins (K. B. Gorman, T. D. Williams,
#   W. R. Fraser, PLOS ONE 2014), from the package palmerpenguins under CC0;
#   species, island, the bill's length and depth, the flipper's length and
#   the body mass, without the two penguins whose bill was not measured.
# - digits-train.csv and digits-test.csv: »Optical Recognition of
#   Handwritten Digits« (E. Alpaydin, C. Kaynak, 1998), UCI Machine Learning
#   Repository, CC BY 4.0; optdigits.tra and optdigits.tes as they are, a
#   digit a line: 64 values from 0 to 16, then the digit.
#
#   ruby collect-perzeptron.rb

require 'csv'
require 'net/http'
require 'uri'

TARGET = File.expand_path('../../static/perzeptron', __dir__)
PENGUINS = 'https://raw.githubusercontent.com/allisonhorst/palmerpenguins/main/inst/extdata/penguins.csv'
DIGITS = 'https://archive.ics.uci.edu/ml/machine-learning-databases/optdigits/'

def download(url, limit = 5)
    raise 'too many redirects' if limit.zero?

    response = Net::HTTP.get_response(URI(url))
    return download(response['location'], limit - 1) if response.is_a?(Net::HTTPRedirection)
    raise "#{url}: #{response.code}" unless response.is_a?(Net::HTTPSuccess)

    response.body.force_encoding('UTF-8')
end

if $PROGRAM_NAME == __FILE__
    rows = CSV.parse(download(PENGUINS), :headers => true)
    columns = %w[species island bill_length_mm bill_depth_mm flipper_length_mm body_mass_g]
    measured = rows.reject { |r| r['bill_length_mm'] == 'NA' || r['bill_depth_mm'] == 'NA' }
    File.write(File.join(TARGET, 'penguins.csv'), ([columns] + measured.map { |r| r.fields(*columns) }).map(&:to_csv).join)
    puts "penguins.csv: #{measured.size} of #{rows.size} penguins"

    { 'optdigits.tra' => 'digits-train.csv', 'optdigits.tes' => 'digits-test.csv' }.each do |source, target|
        text = download(DIGITS + source)
        File.write(File.join(TARGET, target), text)
        puts "#{target}: #{text.lines.size} digits"
    end
end
