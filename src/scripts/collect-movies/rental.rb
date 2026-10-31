# frozen_string_literal: true

require 'date'

# An invented video shop for the last chapter of the Filmdatenbank: copies
# of some of the films, customers and their loans. Nothing here comes from
# Wikidata or IMDb; it is made up from the films with a fixed seed, so a
# rebuild with the same films gives the same rows.
#
#   copy(id, movie_id → movie, format)
#   customer(id, name)
#   loan(customer_id → customer, copy_id → copy, loan_date, return_date)
#
# return_date is NULL while a copy is out. The loans run from FIRST_DAY to
# LAST_DAY; the shop is closed on Sundays. Some copies are never lent (for
# LEFT JOIN, among them the Blu-ray of Back to the Future), and a few
# loans run for weeks.
module Rental
  SEED = 1985
  FIRST_DAY = Date.new(2026, 1, 2)
  LAST_DAY = Date.new(2026, 10, 16)
  CUSTOMERS = 120

  FIRST_NAMES = %w[
    Anna Ben Can Clara Defne Elias Elif Emil Emma Finn Greta Hannah Ilyas Ida Jonas
    Jule Karim Lara Leon Lina Luca Maja Mats Mia Milan Mira Noah Nora Ole Paula
    Rania Samuel Sara Selin Tim Tom Yara Yusuf Zoe Malik Amira Jakob Lea Theo Nele
    Aylin Bruno Dana Frieda Hamza Ivo Janne Kim Lotta Mika Nils Olga Pia Rosa Timo
  ].freeze
  LAST_NAMES = %w[
    Albrecht Arslan Bauer Becker Berger Braun Demir Dietrich Engel Fischer Franke Frey
    Graf Haas Hartmann Hoffmann Horn Jansen Kaiser Kaya Keller Klein Koch Kowalski Krause
    Krüger Kuhn Lange Lehmann Lorenz Ludwig Maier Marx Meyer Möller Nguyen Neumann Novak
    Otto Park Peters Petrović Richter Roth Sander Schmitt Schneider Scholz Schulz Schwarz
    Seidel Sommer Stein Vogel Wagner Walter Weber Winter Wolf Yılmaz Ziegler Zimmermann
  ].freeze

  # { copies: [[id, movie_id, format]], customers: [[id, name]],
  #   loans: [[customer_id, copy_id, loan_date, return_date]] } with dates as
  # 'YYYY-MM-DD' (MySQL reads them into DATE columns)
  def self.build(movies)
    rng = Random.new(SEED)
    copies = copies(movies, rng)
    customers = customers(rng)
    loans = loans(copies, customers, rng)
    {
      copies: copies.map { |c| [c[:id], c[:movie_id], c[:format]] },
      customers: customers.map { |c| [c[:id], c[:name]] },
      loans: loans
    }
  end

  # The films the Filmdatenbank page asks about, always on the shelf: title,
  # year and the formats of their copies; a format in NEVER_LENT is never
  # lent (the page's example for LEFT JOIN)
  STOCKED = { ['Back to the Future', 1985] => %w[DVD Blu-ray], ['Inception', 2010] => %w[DVD DVD Blu-ray] }.freeze
  NEVER_LENT = { ['Back to the Future', 1985] => 'Blu-ray' }.freeze

  # Newer films are more likely on the shelf, some in two or three copies.
  # Blu-ray mostly for films from 2006 on, the year it came out, and as a
  # re-release for some older ones.
  def self.copies(movies, rng)
    copies = []
    movies.sort_by { |m| m[:id] }.each do |m|
      chance = m[:year] >= 2015 ? 0.45 : m[:year] >= 2000 ? 0.3 : m[:year] >= 1980 ? 0.18 : 0.08
      on_shelf = rng.rand < chance
      formats = STOCKED[[m[:title], m[:year]]]
      next unless on_shelf || formats

      count = (r = rng.rand) < 0.7 ? 1 : r < 0.92 ? 2 : 3
      formats ||= Array.new(count) { (m[:year] >= 2006 ? rng.rand < 0.5 : rng.rand < 0.2) ? 'Blu-ray' : 'DVD' }
      appeal = rng.rand(0.3..1.0) * (m[:year] >= 2015 ? 1.5 : 1.0)
      formats.each do |format|
        # about one copy in fifteen stays on the shelf, never lent
        never = rng.rand < 0.065 || NEVER_LENT[[m[:title], m[:year]]] == format
        copies << { id: copies.size + 1, movie_id: m[:id], format: format, weight: never ? 0.0 : appeal }
      end
    end
    copies
  end

  def self.customers(rng)
    names = FIRST_NAMES.product(LAST_NAMES).shuffle(random: rng).first(CUSTOMERS)
    names.each_with_index.map do |(first, last), i|
      { id: i + 1, name: "#{first} #{last}", weight: rng.rand(0.2..1.0)**3 }
    end
  end

  # Day by day: a few loans on weekdays, more on Friday and Saturday, none on
  # Sunday. A copy is back on the shelf on its return day; one that is not
  # back by LAST_DAY has no return_date.
  def self.loans(copies, customers, rng)
    free_from = copies.to_h { |c| [c[:id], FIRST_DAY] }
    loans = []
    (FIRST_DAY..LAST_DAY).each do |day|
      next if day.sunday?

      count = (day.friday? || day.saturday? ? 22 : 12) + rng.rand(-4..4)
      count.times do
        available = copies.select { |c| c[:weight].positive? && free_from[c[:id]] && free_from[c[:id]] <= day }
        copy = pick(available, rng)
        customer = pick(customers, rng)
        break unless copy && customer

        days = (r = rng.rand) < 0.6 ? rng.rand(1..3) : r < 0.9 ? rng.rand(4..7) : r < 0.97 ? rng.rand(8..14) : rng.rand(15..45)
        back = day + days
        if back > LAST_DAY
          loans << [customer[:id], copy[:id], day.iso8601, nil]
          free_from[copy[:id]] = nil
        else
          loans << [customer[:id], copy[:id], day.iso8601, back.iso8601]
          free_from[copy[:id]] = back
        end
      end
    end
    loans.sort_by { |customer_id, copy_id, date, _| [date, copy_id, customer_id] }
  end

  def self.pick(items, rng)
    total = items.sum { |i| i[:weight] }
    return nil unless total.positive?

    at = rng.rand * total
    items.each do |item|
      at -= item[:weight]
      return item if at <= 0
    end
    items.last
  end
end
