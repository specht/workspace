# Offline test of the rules of the country collector: canned Wikidata
# answers, no network.
#   ruby collect_countries_test.rb
require 'minitest/autorun'
require 'tmpdir'
load File.join(__dir__, 'collect-countries.rb')

class CollectCountriesTest < Minitest::Test
    E = 'http://www.wikidata.org/entity/'

    # Answers by what the query asks for.
    class Wikidata
        SETTLEMENTS = %w[Q486972 Q515 Q1549591 Q1637706 Q200250 Q174844 Q133442 Q1221156 Q1907114 Q4286337].freeze
        CITIES = %w[Q515 Q1549591 Q1637706 Q200250 Q174844 Q133442].freeze

        # city => [country, types, people, coordinates, lies in]
        PLACES = {
            'Q64' => ['Q183', %w[Q1221156 Q1637706 Q133442], 3_900_000, [13.38, 52.52], []],   # Berlin: a federated state too
            'Q1209' => ['Q183', %w[Q1221156 Q133442], 680_000, [8.8, 53.08], []],              # the state of Bremen
            'Q24879' => ['Q183', %w[Q1549591], 580_000, [8.81, 53.08], ['Q1209']],             # the city of Bremen
            'Q2833' => ['Q183', %w[Q1549591], 300_000, [13.32, 52.45], ['Q64']],               # a borough of Berlin
            'Q90001' => ['Q183', %w[Q4286337], 200_000, [13.4, 52.5], []],                     # a city district
            'Q90002' => ['Q183', %w[Q1907114 Q174844], 6_000_000, [13.4, 52.5], []],           # a metropolitan area
            'Q90003' => ['Q183', %w[Q1549591], 50_000, [10.0, 50.0], []],                      # too small
            'Q1117116' => ['Q142', %w[Q200250], 800_000, [-0.57, 44.84], []],                  # the union of Bordeaux
            'Q1479' => ['Q142', %w[Q515], 260_000, [-0.58, 44.84], ['Q1117116']],              # Bordeaux
            'Q90004' => ['Q142', %w[Q486972], 120_000, [2.0, 47.0], []],                       # no more than a settlement
            'Q4166179' => ['Q142', %w[Q486972], 1_500_000, [3.0, 48.0], []],                   # left out by hand
        }.freeze

        def select(query)
            e = CollectCountriesTest::E
            if query.include?('wdt:P279*')
                root = query[/wd:(Q\d+) \./, 1]
                list = { 'Q486972' => SETTLEMENTS, 'Q515' => CITIES }.fetch(root, [])
                list.map { |q| { 'class' => "#{e}#{q}" } }
            elsif query.include?('?city wdt:P1082 ?pop')
                PLACES.flat_map do |q, (country, types, people, (lon, lat))|
                    next [] if people < WikidataCountryBuilder::MIN_PEOPLE

                    types.map { |t| { 'city' => "#{e}#{q}", 'country' => "#{e}#{country}", 'type' => "#{e}#{t}", 'coord' => "Point(#{lon} #{lat})" } }
                end
            elsif query.include?('p:P131')
                asked(query).flat_map { |q| PLACES[q][4].map { |v| row(q, "#{e}#{v}") } }
            elsif query.include?('p:P1082')
                asked(query).map { |q| row(q, PLACES[q][2].to_s) }
            else
                raise "unexpected query: #{query}"
            end
        end

        def asked(query) = query.scan(/wd:(Q\d+)/).flatten.select { |q| PLACES.key?(q) }

        def row(item, value) = { 'item' => "#{E}#{item}", 'value' => value, 'rank' => 'http://wikiba.se/ontology#NormalRank' }
    end

    # The statements of one item, as the query service gives them
    class Statements
        def select(_query)
            r = ->(value, rank, date) { { 'item' => "#{E}Q2211", 'value' => value, 'rank' => "http://wikiba.se/ontology##{rank}", 'date' => date } }
            [r.('12456', 'NormalRank', '2023-12-31'), r.('339316', 'PreferredRank', '2023-12-31'), r.('0', 'NormalRank', '2023-12-31'),
             r.('324002', 'NormalRank', '2020-12-31')]
        end
    end

    def builder
        WikidataCountryBuilder.new(output_dir: Dir.mktmpdir, wikidata: Wikidata.new)
    end

    def test_which_settlements_are_cities
        countries = [{ wikidata: 'Q183', code: 'DEU' }, { wikidata: 'Q142', code: 'FRA' }]
        cities = builder.load_cities(countries).to_h { |c| [c[:wikidata], c] }
        # Berlin is a federated state, but a million city too; the state of
        # Bremen is a city-state, which alone makes no city
        assert_equal %w[Q1479 Q24879 Q64 Q90004], cities.keys.sort
        assert_equal 'DEU', cities['Q64'][:country]
        assert_equal 3_900_000, cities['Q64'][:population]
        assert_equal [52.52, 13.38], [cities['Q64'][:latitude], cities['Q64'][:longitude]]
    end

    def test_the_latest_value
        b = builder
        assert_equal 'Q2', b.latest([{ value: 'Q1', rank: 'NormalRank' }, { value: 'Q2', rank: 'PreferredRank' }])
        # the latest date first: an estimate of 2025 over a census of 2013 with preferred rank
        assert_equal '11000000', b.latest([{ value: '8300000', rank: 'PreferredRank', date: '2013' }, { value: '11000000', rank: 'NormalRank', date: '2025' }])
        assert_equal '7', b.latest([{ value: '5', rank: 'NormalRank', date: '2020' }, { value: '7', rank: 'NormalRank', date: '2024' },
                                    { value: '1', rank: 'NormalRank', date: '2025', end: '2025' }])
        # several of the same rank and date: the middle one, the lower of two
        assert_equal '51187', b.latest(%w[10 51209.2 51187].map { |v| { value: v, rank: 'NormalRank' } })
        assert_equal '37378', b.latest(%w[41543 37378].map { |v| { value: v, rank: 'NormalRank' } })
        # a value for a part only (P518) counts if there is no other
        assert_equal '136988', b.latest([{ value: '23889', rank: 'NormalRank', date: '2024', part: "#{E}Q1" },
                                         { value: '136988', rank: 'NormalRank', date: '2023' }])
        assert_equal '23889', b.latest([{ value: '23889', rank: 'NormalRank', date: '2024', part: "#{E}Q1" }])
        assert_nil b.latest([])
    end

    def test_a_value_with_preferred_rank_wins
        b = WikidataCountryBuilder.new(output_dir: Dir.mktmpdir, wikidata: Statements.new)
        values = b.statements(['Q2211'], 'P1082')['Q2211']
        assert_equal 'PreferredRank', values[1][:rank]
        assert_equal '339316', b.latest(values)
    end

    def test_the_plain_name_of_a_city
        assert_equal 'Madrid', WikidataCountryBuilder.plain_name('Stadt Madrid')
        assert_equal 'Valencia', WikidataCountryBuilder.plain_name('City of Valencia')
        assert_equal 'Valladolid', WikidataCountryBuilder.plain_name('Valladolid city')
        assert_equal 'Jaén', WikidataCountryBuilder.plain_name('Jaén (Stadt)')
        assert_equal 'Stadt des 10. Ramadan', WikidataCountryBuilder.plain_name('Stadt des 10. Ramadan')
        assert_equal 'Kansas City', WikidataCountryBuilder.plain_name('Kansas City')
    end

    def test_cities_sort_by_name_without_accents
        names = %w[Zürich Ägypten Aachen Österreich Berlin]
        assert_equal %w[Aachen Ägypten Berlin Österreich Zürich], names.sort_by { |n| WikidataCountryBuilder.sort_key(n) }
    end

    def test_the_same_place_counts_once
        city = ->(q, name, lat, people) { { wikidata: q, country: 'ESP', name: name, latitude: lat, longitude: -3.7, population: people } }
        kept = builder.merge_same_places([city.('Q1', 'Stadt Madrid', 40.42, 3_300_000), city.('Q2', 'Madrid', 40.41, 3_200_000),
                                          city.('Q3', 'Mérida', 38.9, 60_000), city.('Q4', 'Mérida', 41.0, 60_000)], Set.new)
        # the item with the plain name wins over a bigger count; places far apart are two
        assert_equal [%w[Q2 Madrid], %w[Q3 Mérida], %w[Q4 Mérida]], kept.map { |c| [c[:wikidata], c[:name]] }.sort
    end
end
