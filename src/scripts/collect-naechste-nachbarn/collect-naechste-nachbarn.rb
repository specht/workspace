#!/usr/bin/env ruby
# frozen_string_literal: true

# Builds the data of »Nächste Nachbarn: Wer entscheidet hier?«
# (src/content/naechste-nachbarn), served from src/static/naechste-nachbarn:
#
# - penguins.csv: the Palmer penguins, the same file as in »Ein Neuron
#   lernt« (src/scripts/collect-perzeptron), copied.
# - customers.csv: 600 customers of a bike rental in a made-up town, made up
#   by this script with a fixed seed, so that it is known what is in them.
#   A river runs through the town at y = 3 km; south of it lies Sandberg,
#   north of it Lindenhof (west), Altstadt (middle, with the shop at 4, 5)
#   and Bahnhofsviertel (east). Every customer is as reliable in every
#   district: seven in ten bring a bike back on time 95 times in 100, two in
#   ten 80 times, one in ten 50 times. The staff asked for a deposit from new
#   customers (under half a year) and from those with two late returns or
#   more in the last year; at Sandberg, from six in ten of the others too.
#   Columns: district, x and y (km east and north of the town's south-west
#   corner), distance (km to the shop), years (as a customer), late (late
#   returns in the last year, at most 4), deposit (the staff's decision, 1:
#   a deposit), on_time (1: the next bike came back on time).
#
#   ruby collect-naechste-nachbarn.rb

require 'fileutils'

TARGET = File.expand_path('../../static/naechste-nachbarn', __dir__)
PENGUINS = File.expand_path('../../static/perzeptron/penguins.csv', __dir__)
SHOP = [4.0, 5.0].freeze
CUSTOMERS = 600
SEED = 7

def district(x, y)
    return 'Sandberg' if y < 3
    return 'Lindenhof' if x < 3
    return 'Bahnhofsviertel' if x >= 5

    'Altstadt'
end

def customers(random)
    CUSTOMERS.times.map do
        if random.rand < 0.3
            x, y = random.rand * 8, random.rand * 2.6 + 0.2
        else
            x, y = random.rand * 8, 3.4 + random.rand * 4.4
        end
        x, y = x.round(2), y.round(2)
        reliability = [0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.8, 0.8, 0.5][random.rand(10)]
        years = (random.rand * 6).round(2)
        rentals = [(years * 3).round, 12].min
        late = [rentals.times.count { random.rand > reliability }, 4].min
        on_time = random.rand < reliability ? 1 : 0
        deposit = years < 0.5 || late >= 2 ? 1 : 0
        deposit = 1 if deposit == 0 && district(x, y) == 'Sandberg' && random.rand < 0.6
        distance = Math.sqrt((x - SHOP[0])**2 + (y - SHOP[1])**2).round(2)
        [district(x, y), x, y, distance, years, late, deposit, on_time]
    end
end

if $PROGRAM_NAME == __FILE__
    FileUtils.mkdir_p(TARGET)
    FileUtils.cp(PENGUINS, File.join(TARGET, 'penguins.csv'))
    rows = customers(Random.new(SEED))
    header = 'district,x,y,distance,years,late,deposit,on_time'
    File.write(File.join(TARGET, 'customers.csv'), ([header] + rows.map { |r| r.join(',') }).join("\n") + "\n")
    puts "customers.csv: #{rows.size} customers, #{rows.count { |r| r[0] == 'Sandberg' }} at Sandberg"
end
