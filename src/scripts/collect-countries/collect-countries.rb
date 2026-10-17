#!/usr/bin/env ruby
# frozen_string_literal: true

# Builds the classroom country dataset from Wikidata: the continents, the
# countries of the world, their cities from 100,000 inhabitants (and every
# capital) and which countries share a border.
#
# Which countries: the members of the United Nations, its two observer
# states (Vatican City, Palestine), Kosovo and Taiwan. Names are the German
# labels of Wikidata, so that a query can say WHERE name = 'Deutschland';
# tables and columns are English, as in the movie dataset. Everything comes
# from Wikidata, which is CC0.

require 'json'
require 'optparse'
require 'set'
require 'time'

require_relative '../lib/classroom_dataset'
require_relative '../lib/wikidata_client'

class WikidataCountryBuilder
  USER_AGENT = 'hackschule-workspace-country-dataset/1.0 (https://github.com/specht/workspace; classroom dataset)'

  # The UN member stands for a kingdom of several parts; the part in Europe
  # is the country people mean (and the one with the ISO code DNK).
  INSTEAD = { 'Q756617' => 'Q35', 'Q29999' => 'Q55' }.freeze
  # Not UN members, but countries a class will look for
  EXTRA = { 'Q237' => 'Vatikanstadt', 'Q219060' => 'Palästina', 'Q1246' => 'Kosovo', 'Q865' => 'Taiwan' }.freeze
  # Where the German label of Wikidata is not the name used in German
  NAMES = { 'NRU' => 'Nauru', 'CHN' => 'China', 'FSM' => 'Mikronesien' }.freeze

  # The seven continents, in this order (their id), and what else Wikidata
  # names as the continent of a country
  CONTINENTS = [
    ['Q15', 'Afrika'], ['Q51', 'Antarktika'], ['Q48', 'Asien'], ['Q46', 'Europa'],
    ['Q49', 'Nordamerika'], ['Q55643', 'Ozeanien'], ['Q18', 'Südamerika']
  ].freeze
  ALSO = { 'Q3960' => 'Q55643', 'Q538' => 'Q55643', 'Q27611' => 'Q49', 'Q664609' => 'Q49' }.freeze
  # One continent per country: for a country on several, the one its capital
  # lies on (as atlases and the UN statistics division count it)
  CONTINENT_OF = {
    'RUS' => 'Q46', 'TUR' => 'Q48', 'KAZ' => 'Q48', 'AZE' => 'Q48', 'GEO' => 'Q48', 'ARM' => 'Q48',
    'EGY' => 'Q15', 'CYP' => 'Q48', 'IDN' => 'Q48', 'PNG' => 'Q55643', 'TLS' => 'Q48', 'PAN' => 'Q49',
    'FRA' => 'Q46', 'ESP' => 'Q46', 'PRT' => 'Q46', 'ITA' => 'Q46', 'GRC' => 'Q46', 'NLD' => 'Q46',
    'DNK' => 'Q46', 'NOR' => 'Q46', 'USA' => 'Q49', 'CHL' => 'Q18', 'ECU' => 'Q18', 'COL' => 'Q18',
    'VEN' => 'Q18', 'YEM' => 'Q48', 'JPN' => 'Q48', 'NZL' => 'Q55643', 'AUS' => 'Q55643'
  }.freeze
  # A capital set by hand. For a country with several, the one atlases list:
  # Pretoria, not Cape Town or Bloemfontein; Porto-Novo, not Cotonou. Managua
  # as the municipality, which has the population; the capital item of
  # Nicaragua has none.
  CAPITAL_OF = { 'ZAF' => 'Q3926', 'BEN' => 'Q3799', 'NIC' => 'Q3274' }.freeze

  # Driving side (P1622)
  DRIVING = { 'Q14565199' => 'right', 'Q11920728' => 'left' }.freeze
  # A country with several current currencies in Wikidata: the one it calls
  # its own (Panama the balboa beside the US dollar, Namibia the dollar beside
  # the rand), France the euro (not the Pacific franc of its territories),
  # Zimbabwe its gold currency, Poland the złoty (not the old one of 1950)
  CURRENCY_OF = { 'FRA' => 'EUR', 'POL' => 'PLN', 'ZWE' => 'ZWG', 'PAN' => 'PAB', 'LSO' => 'LSL', 'NAM' => 'NAD' }.freeze

  # A language or a variety of one without an ISO 639-3 code of its own in
  # Wikidata: the code of the language (Greek, British and Australian
  # English, Taiwan's Mandarin, Hokkien and Hakka, North Korea's Korean)
  LANGUAGE_CODE = {
    'Q9129' => 'ell', 'Q7979' => 'eng', 'Q44679' => 'eng', 'Q56467' => 'arb', 'Q18784' => 'kor',
    'Q262828' => 'cmn', 'Q36778' => 'nan', 'Q2391532' => 'hak'
  }.freeze
  # A country with no official language in Wikidata: the language of the state
  LANGUAGES_OF = { 'AZE' => { 'aze' => 'Aserbaidschanisch' }, 'URY' => { 'spa' => 'Spanisch' } }.freeze

  MIN_PEOPLE = 100_000
  # Kinds of »human settlement« that are no city in the sense of the lesson:
  # an area of several cities, a region around one, a part of a city, a unit
  # of administration. Only these kinds themselves: in Wikidata, »city in the
  # United States« is a subclass of »urban area of the United States«.
  NOT_A_CITY = {
    'Q2779745' => 'urban district of the Netherlands', 'Q1907114' => 'metropolitan area', 'Q159313' => 'urban agglomeration', 'Q245260' => 'conurbation',
    'Q2342385' => 'urban area', 'Q3175539' => 'functional urban area', 'Q1029013' => 'megaregion of the United States',
    'Q5951278' => 'urban area of the United States', 'Q1768043' => 'metropolitan statistical area',
    'Q1132300' => 'metropolitan region in Germany', 'Q15110' => 'metropolitan city of Italy', 'Q3333855' => 'métropole',
    'Q28575' => 'county', 'Q180673' => 'ceremonial county of England', 'Q1221156' => 'federated state of Germany',
    'Q22923920' => 'territorial collectivity of France',
    'Q123705' => 'neighborhood', 'Q2983893' => 'quarter', 'Q188509' => 'suburb', 'Q253019' => 'Ortsteil',
    'Q4286337' => 'city district', 'Q137773' => 'ward of Japan', 'Q15725677' => 'pedanía',
    'Q87542664' => 'sector of Marseille', 'Q4389092' => 'district of Moscow', 'Q15195406' => 'city district in Russia',
    'Q18559008' => 'district of São Paulo', 'Q705296' => 'district of Taiwan', 'Q1147395' => 'district of Turkey',
    'Q620471' => 'upazila of Bangladesh', 'Q735428' => 'town of China',
    'Q4056979' => 'agglomeration of Russia', 'Q3270419' => 'micropolitan statistical area', 'Q702492' => 'urban area',
    'Q3931970' => 'metropolitan region in Brazil', 'Q2826814' => 'urban agglomeration in Québec',
    'Q11962746' => 'metropolitan region of Norway', 'Q15092344' => 'urban area in Norway',
    'Q525504' => 'municipio of Rome', 'Q108723311' => 'municipality of Bari', 'Q15921300' => 'sector of Bucharest',
    'Q3565075' => 'raion of city in Ukraine', 'Q856976' => 'city district of Stockholm', 'Q2661988' => 'urban settlement in Russia',
    'Q1530824' => 'metropolitan municipality in South Africa', 'Q783930' => 'municipalities and cities of Serbia',
    'Q648681' => 'district of Serbia', 'Q2989682' => 'municipality of Kosovo', 'Q838549' => 'municipality of Montenegro',
    'Q18670606' => 'tehsil of Pakistan', 'Q509686' => 'province of Peru', 'Q851830' => "region of Côte d'Ivoire",
    'Q13415368' => 'county of Virginia', 'Q18534049' => 'county of North Korea', 'Q952274' => 'department of Argentina',
    'Q5283558' => 'district of Zimbabwe', 'Q2424478' => 'arrondissement of Senegal', 'Q782614' => 'autonomous region',
    'Q3032114' => 'district of Madrid', 'Q12750426' => 'city municipality of Serbia', 'Q12813115' => 'urban area in Sweden'
  }.freeze
  # Areas that are no more than a »human settlement« in Wikidata, or are a
  # city by a type they should not have
  NOT_A_CITY_ITEM = {
    'Q3571645' => 'Pamplona metropolitan area', 'Q16897338' => 'Swansea Bay City Region', 'Q65061424' => 'Greater Malang',
    'Q4166179' => 'Greater Donetsk', 'Q116954165' => 'Kangemi Ward', 'Q794954' => 'Latina (a district of Madrid)',
    'Q506202' => 'Spotsylvania County', 'Q4169280' => 'Dublin City (the council area beside Dublin)',
    'Q3695586' => 'Kansai Science City'
  }.freeze
  # Cities whose label in German or English says »city« after the name
  CITY_NAMES = {
    'Q43167665' => 'Burgos', 'Q24008386' => 'Albacete', 'Q116442617' => 'Salamanca', 'Q31915930' => 'Cádiz'
  }.freeze
  # Wikidata gives many cities such a kind as well (Lagos and Melbourne are
  # metropolitan areas, Berlin a federated state, Munich an urban district):
  # an item stays if it is a kind of city too, with all subclasses, but not
  # just by one of the kinds after it, which areas have as well.
  CITY = { 'Q515' => 'city' }.freeze
  NOT_CITY_ENOUGH = {
    'Q200250' => 'metropolis', 'Q174844' => 'megacity', 'Q471822' => 'megalopolis', 'Q208511' => 'global city',
    'Q133442' => 'city-state', 'Q902814' => 'border city'
  }.freeze
  HUMAN_SETTLEMENT = 'Q486972'
  # Municipalities that are their city, but no »human settlement« in Wikidata:
  # Marseille, Nantes, Maceió, Sfax
  MUNICIPAL_CITY = {
    'Q484170' => 'commune of France', 'Q3184121' => 'municipality of Brazil', 'Q41067667' => 'municipality of Tunisia'
  }.freeze
  # Two items with the same name in the same country closer than this are the
  # same place (a municipality and its city); Kansas City in Missouri and
  # the one in Kansas are 10 km apart.
  SAME_PLACE_KM = 8

  def initialize(output_dir:, offline: false, pause: 1.0, wikidata: nil)
    @output_dir = output_dir
    @wikidata = wikidata || WikidataClient.new(cache_dir: File.join(output_dir, '.wikidata-cache'),
                                               pause: pause, offline: offline, user_agent: USER_AGENT)
    @notes = []
  end

  COUNTRIES = 197   # 193 UN members, 2 observers, Kosovo, Taiwan

  def run
    FileUtils.mkdir_p(@output_dir)
    items = country_items
    countries = load_countries(items)
    raise "#{countries.size} countries instead of #{COUNTRIES}: #{countries.map { |c| c[:code] }.join(' ')}" unless countries.size == COUNTRIES
    load_details(countries)
    cities = load_cities(countries)
    add_capitals(countries, cities)
    borders = load_borders(countries)
    continents = load_continents
    write(continents, countries, cities, borders)
    report(continents, countries, cities, borders)
  end

  def self.qid(uri) = uri.to_s.split('/').last

  def entity_list(qids) = qids.map { |q| "wd:#{q}" }.join(' ')

  # --- Which countries ----------------------------------------------------

  def country_items
    rows = @wikidata.select(<<~SPARQL)
      SELECT DISTINCT ?c WHERE {
        ?c p:P463 ?st . ?st ps:P463 wd:Q1065 .
        FILTER NOT EXISTS { ?st pq:P582 ?end }
        FILTER NOT EXISTS { ?c wdt:P576 ?gone }
      }
    SPARQL
    members = rows.map { |r| self.class.qid(r['c']) }.map { |q| INSTEAD.fetch(q, q) }
    (members + EXTRA.keys).uniq
  end

  # The latest value of a statement: none that has ended (an old ISO code),
  # the latest date (P585), of several with that date the one with preferred
  # rank. (Preferred rank first would take a census of 2013 over an estimate
  # of 2025.) A value for a part only (P518: the foreigners of Bern, the land
  # of Finland without its lakes) counts if there is no other. Several numbers
  # of the same date and rank: the middle one, the lower of two. Wikidata has
  # 10 km² for Bosnia next to 51,187 and 51,209.
  def latest(values)
    current = values.reject { |v| v[:end] }
    whole = current.reject { |v| v[:part] }
    current = whole unless whole.empty?
    return nil if current.empty?

    tied = current.group_by { |v| [v[:date].to_s, v[:rank] == 'PreferredRank' ? 1 : 0] }.max_by(&:first).last
    numbers = tied.map { |v| v[:value].to_s }
    return tied.first[:value] unless numbers.size > 1 && numbers.all? { |n| n.match?(/\A-?\d+(?:\.\d+)?(?:e[-+]?\d+)?\z/i) }

    numbers.sort_by(&:to_f)[(numbers.size - 1) / 2]
  end

  def statements(qids, property, value_path = "ps:#{property}")
    rows = qids.each_slice(100).flat_map do |batch|
      @wikidata.select(<<~SPARQL)
        SELECT ?item ?value ?date ?end ?rank ?part WHERE {
          VALUES ?item { #{entity_list(batch)} }
          ?item p:#{property} ?st . ?st #{value_path} ?value ; wikibase:rank ?rank .
          FILTER(?rank != wikibase:DeprecatedRank)
          OPTIONAL { ?st pq:P585 ?date }
          OPTIONAL { ?st pq:P582 ?end }
          OPTIONAL { ?st pq:P518 ?part }
        }
      SPARQL
    end
    rows.group_by { |r| self.class.qid(r['item']) }.transform_values do |list|
      list.map { |r| { value: r['value'], date: r['date'], end: r['end'], rank: r['rank'].to_s.split('#').last, part: r['part'] } }.uniq
    end
  end

  def labels(qids)
    rows = qids.each_slice(200).flat_map do |batch|
      @wikidata.select(<<~SPARQL)
        SELECT ?item ?de ?en ?mul WHERE {
          VALUES ?item { #{entity_list(batch)} }
          OPTIONAL { ?item rdfs:label ?de FILTER(lang(?de) = 'de') }
          OPTIONAL { ?item rdfs:label ?en FILTER(lang(?en) = 'en') }
          OPTIONAL { ?item rdfs:label ?mul FILTER(lang(?mul) = 'mul') }
        }
      SPARQL
    end
    rows.to_h { |r| [self.class.qid(r['item']), r['de'] || r['en'] || r['mul']] }
  end

  def load_countries(items)
    names = labels(items)
    codes = statements(items + INSTEAD.keys, 'P298')
    people = statements(items, 'P1082')
    # in square metres, whatever unit the statement uses
    areas = statements(items, 'P2046', 'psn:P2046/wikibase:quantityAmount')
    capitals = statements(items, 'P36')
    continents = statements(items, 'P30')
    items.filter_map do |q|
      # an item for a period of a country (Third Hellenic Republic) is a UN member too
      # the part in Europe may have no code of its own; the kingdom has it
      code = latest(codes[q] || []) || latest(codes[INSTEAD.key(q)] || [])
      unless code
        @notes << "#{q} (#{names[q]}) has no ISO code, left out"
        next
      end
      on = (continents[q] || []).map { |c| self.class.qid(c[:value]) }.map { |c| ALSO.fetch(c, c) }.uniq
      on &= CONTINENTS.map(&:first)
      continent = CONTINENT_OF[code] || (on.size == 1 ? on.first : nil)
      raise "#{code}: on #{on.inspect}, say which in CONTINENT_OF" unless continent
      # the capitals now: a statement with an end date is a capital of the past
      capital_list = (capitals[q] || []).reject { |c| c[:end] }.map { |c| self.class.qid(c[:value]) }.uniq
      capital = CAPITAL_OF[code] || self.class.qid(latest(capitals[q] || []))
      @notes << "#{code}: capitals #{capital_list.join(', ')}, took #{capital}" if capital_list.size > 1 && !CAPITAL_OF[code]
      {
        wikidata: q, code: code, name: NAMES[code] || names[q],
        population: latest(people[q] || [])&.to_f&.round,
        area: (area = latest(areas[q] || [])) && (area.to_f / 1_000_000).round(2),
        capital: capital, continent: continent
      }
    end.sort_by { |c| c[:code] }
  end

  # The current values of a statement, preferred rank first; values for a
  # part only (P518: the languages of Russia's republics) if there are no others
  def current(values)
    now = values.reject { |v| v[:end] }
    whole = now.reject { |v| v[:part] }
    (whole.empty? ? now : whole).sort_by { |v| v[:rank] == 'PreferredRank' ? 0 : 1 }
  end

  # Internet domain (P78), calling code (P474), driving side (P1622), the
  # currency (P38, by its ISO 4217 code) and the official languages (P37, by
  # their ISO 639-3 code). Sets @currencies and @languages, the rows of their
  # tables.
  def load_details(countries)
    items = countries.map { |c| c[:wikidata] }
    domains = statements(items, 'P78')
    calls = statements(items, 'P474')
    sides = statements(items, 'P1622')
    money = statements(items, 'P38')
    tongues = statements(items, 'P37')
    alpha2 = statements(items, 'P297')
    currency_items = money.values.flatten.map { |v| self.class.qid(v[:value]) }.uniq
    language_items = tongues.values.flatten.map { |v| self.class.qid(v[:value]) }.uniq
    domain_names = labels(domains.values.flatten.map { |v| self.class.qid(v[:value]) }.uniq)
    iso4217 = statements(currency_items, 'P498')
    iso639 = statements(language_items, 'P220')
    currency_names = labels(currency_items)
    language_names = labels(language_items)
    @currencies = {}
    @languages = {}
    countries.each do |c|
      q = c[:wikidata]
      # an ASCII domain of two letters; of several the one with preferred
      # rank, else the one of the country's two-letter code (.de, not .deutschland)
      own = ".#{latest(alpha2[q] || []).to_s.downcase}"
      tlds = current(domains[q] || []).filter_map do |v|
        name = domain_names[self.class.qid(v[:value])].to_s.downcase
        [name, v[:rank]] if name.match?(/\A\.[a-z]{2}\z/)
      end
      preferred = tlds.find { |_, rank| rank == 'PreferredRank' }
      c[:tld] = preferred&.first || (tlds.any? { |name, _| name == own } ? own : tlds.first&.first)
      code = current(calls[q] || []).map { |v| v[:value].to_s.strip }.first
      c[:calling_code] = code && (code.start_with?('+') ? code : "+#{code}")
      c[:driving_side] = current(sides[q] || []).map { |v| DRIVING[self.class.qid(v[:value])] }.compact.first
      choices = current(money[q] || []).filter_map do |v|
        item = self.class.qid(v[:value])
        iso = latest(iso4217[item] || []).to_s
        [iso, item, v[:rank]] if iso.match?(/\A[A-Z]{3}\z/)
      end.uniq { |iso, _, _| iso }
      best = choices.select { |_, _, rank| rank == 'PreferredRank' }
      best = choices if best.empty?
      pick = CURRENCY_OF[c[:code]] ? choices.find { |iso, _, _| iso == CURRENCY_OF[c[:code]] } : (best.first if best.size == 1)
      raise "#{c[:code]}: currencies #{choices.map(&:first).join(', ')}; say which in CURRENCY_OF" unless pick || choices.empty?
      if pick
        c[:currency] = pick[0]
        @currencies[pick[0]] ||= { code: pick[0], name: currency_names[pick[1]], wikidata: pick[1] }
      else
        @notes << "#{c[:code]}: no current currency with an ISO code"
      end
      c[:languages] = current(tongues[q] || []).filter_map do |v|
        item = self.class.qid(v[:value])
        iso = LANGUAGE_CODE[item] || latest(iso639[item] || []).to_s
        unless iso.match?(/\A[a-z]{3}\z/)
          @notes << "#{c[:code]}: language #{item} (#{language_names[item]}) has no ISO 639-3 code, left out"
          next
        end
        @languages[iso] ||= { code: iso, name: language_names[item], wikidata: item }
        iso
      end.uniq
      next unless c[:languages].empty? && LANGUAGES_OF[c[:code]]

      c[:languages] = LANGUAGES_OF[c[:code]].map do |iso, name|
        @languages[iso] ||= { code: iso, name: name }
        iso
      end
    end
  end

  # --- Cities ---------------------------------------------------------------

  # Every human settlement that had at least MIN_PEOPLE at its latest count,
  # with a place on the map. Asking country by country makes the query
  # service go through every item of China or India, and asking whether each
  # item is a settlement takes hours; so: all classes of settlements once,
  # one query for whatever has that many people by its current value, with
  # its classes (a province has some too), and then the latest count of the
  # settlements among them.
  # The classes with all their subclasses
  def subclasses(roots)
    roots.flat_map do |root|
      @wikidata.select(<<~SPARQL).map { |r| self.class.qid(r['class']) }
        SELECT ?class WHERE { ?class wdt:P279* wd:#{root} . }
      SPARQL
    end.to_set
  end

  def load_cities(countries)
    code_of = countries.to_h { |c| [c[:wikidata], c[:code]] }
    kinds = subclasses([HUMAN_SETTLEMENT]) | MUNICIPAL_CITY.keys
    no_city = NOT_A_CITY.keys.to_set
    city = subclasses(CITY.keys) - no_city - NOT_CITY_ENOUGH.keys
    candidates = {}
    @wikidata.select(<<~SPARQL).each do |r|
      SELECT DISTINCT ?city ?country ?type ?coord WHERE {
        ?city wdt:P1082 ?pop . FILTER(?pop >= #{MIN_PEOPLE})
        ?city wdt:P17 ?country ; wdt:P31 ?type ; wdt:P625 ?coord .
        FILTER NOT EXISTS { ?city wdt:P576 ?gone }
      }
    SPARQL
      code = code_of[self.class.qid(r['country'])]
      next unless code

      entry = (candidates[self.class.qid(r['city'])] ||= { countries: Set.new, coord: r['coord'], types: Set.new })
      entry[:countries] << code
      entry[:types] << self.class.qid(r['type'])
    end
    candidates.select! do |q, c|
      next false if NOT_A_CITY_ITEM.key?(q)

      c[:types].intersect?(kinds) && (!c[:types].intersect?(no_city) || c[:types].intersect?(city))
    end
    inside = statements(candidates.keys, 'P131').transform_values do |values|
      values.reject { |v| v[:end] }.map { |v| self.class.qid(v[:value]) }.select { |q| candidates.key?(q) }
    end
    # An item that is no more than a »human settlement« or a »metropolis« and
    # has other cities in it is an area, as the union of Bordeaux and its
    # neighbours: it goes, its cities stay.
    vague = Set[HUMAN_SETTLEMENT, *NOT_CITY_ENOUGH.keys]
    areas = inside.values.flatten.to_set.select { |q| (candidates[q][:types] & kinds).subset?(vague) }
    @notes << "#{areas.size} areas with cities in them left out" unless areas.empty?
    areas.each { |q| candidates.delete(q) }
    # A part of a city that is a city of the dataset itself is none of its own:
    # a borough of Berlin or Vienna, which lies in (P131) Berlin or Vienna
    parts = candidates.keys.select { |q| (inside[q] || []).any? { |container| candidates.key?(container) } }
    @notes << "#{parts.size} parts of other cities left out" unless parts.empty?
    parts.each { |q| candidates.delete(q) }
    people = statements(candidates.keys, 'P1082')
    candidates.filter_map do |q, candidate|
      population = latest(people[q] || []).to_f.round
      next if population < MIN_PEOPLE

      longitude, latitude = candidate[:coord].scan(/-?\d+(?:\.\d+)?/).map(&:to_f)
      { wikidata: q, country: candidate[:countries].min, countries: candidate[:countries].to_a,
        population: population, latitude: latitude.round(4), longitude: longitude.round(4) }
    end
  end

  def distance_km(a, b)
    rad = Math::PI / 180
    dlat = (b[:latitude] - a[:latitude]) * rad
    dlon = (b[:longitude] - a[:longitude]) * rad
    h = Math.sin(dlat / 2)**2 + Math.cos(a[:latitude] * rad) * Math.cos(b[:latitude] * rad) * Math.sin(dlon / 2)**2
    2 * 6371 * Math.asin(Math.sqrt(h))
  end

  # The name of a place without what tells a city from its municipality:
  # »Stadt Madrid«, »City of Valencia«, »Valladolid city«, »Jaén (Stadt)«
  def self.plain_name(name)
    name.sub(/\A(?:Stadt|City of) (?=\p{Lu})/, '').sub(/ (?:city|\((?:Stadt|city|Tettsted)\))\z/, '')
  end

  def self.sort_key(name) = name.to_s.unicode_normalize(:nfd).gsub(/\p{Mn}/, '').downcase

  # The same place as two items: same name, same country, close together.
  # A capital wins, then the item with the plain name, then the bigger count;
  # it keeps the plain name.
  def merge_same_places(cities, capitals)
    kept = []
    cities.group_by { |c| [c[:country], self.class.plain_name(c[:name])] }.each do |(_, name), group|
      group.sort_by { |c| [capitals.include?(c[:wikidata]) ? 0 : 1, c[:name] == name ? 0 : 1, -(c[:population] || 0)] }.each do |city|
        if kept.any? { |k| k[:country] == city[:country] && self.class.plain_name(k[:name]) == name && distance_km(k, city) < SAME_PLACE_KM }
          @notes << "#{city[:name]} (#{city[:country]}): two items, kept one" if @notes.size < 60
          next
        end
        kept << city.merge(name: name)
      end
    end
    kept
  end

  # A capital is a city of the dataset, however small
  def add_capitals(countries, cities)
    known = cities.to_h { |c| [c[:wikidata], c] }
    missing = countries.map { |c| c[:capital] }.compact.reject { |q| known[q] }
    unless missing.empty?
      people = statements(missing, 'P1082')
      coords = statements(missing, 'P625')
      by_capital = countries.to_h { |c| [c[:capital], c] }
      missing.each do |q|
        coord = latest(coords[q] || []) or next @notes << "capital #{q} of #{by_capital[q][:code]} has no coordinates"
        longitude, latitude = coord.scan(/-?\d+(?:\.\d+)?/).map(&:to_f)
        cities << { wikidata: q, country: by_capital[q][:code], population: latest(people[q] || [])&.to_f&.round,
                    latitude: latitude.round(4), longitude: longitude.round(4) }
      end
    end
    # A city that Wikidata places in two countries of the dataset (Crimea in
    # Russia and Ukraine, Prizren in Serbia and Kosovo) goes with the country
    # whose capital is nearest.
    at = cities.to_h { |c| [c[:wikidata], c] }
    capital_of = countries.to_h { |c| [c[:code], at[c[:capital]]] }
    cities.each do |c|
      choices = c.delete(:countries) || []
      next if choices.size < 2

      c[:country] = choices.min_by { |code| capital_of[code] ? distance_km(c, capital_of[code]) : Float::INFINITY }
    end
    names = labels(cities.map { |c| c[:wikidata] }.uniq)
    cities.each { |c| c[:name] = CITY_NAMES[c[:wikidata]] || names[c[:wikidata]] }
    unnamed = cities.reject { |c| c[:name] }
    @notes << "#{unnamed.size} cities without a name in German, English or mul, left out" unless unnamed.empty?
    cities.replace(merge_same_places(cities - unnamed, countries.map { |c| c[:capital] }.to_set))
    # ids by country and name, Ä as A: not by size, so that a query without
    # ORDER BY does not happen to give the biggest cities first
    cities.sort_by! { |c| [c[:country], self.class.sort_key(c[:name]), -(c[:population] || 0)] }
    cities.each_with_index { |c, i| c[:id] = i + 1 }
    capitals = countries.map { |c| [c[:capital], c[:code]] }.to_set
    cities.each { |c| c[:is_capital] = capitals.include?([c[:wikidata], c[:country]]) }
  end

  # --- Borders and continents ------------------------------------------------

  # Land borders between two countries of the dataset, in both directions.
  # Not »shares border with« (P47): it counts sea borders and borders through
  # an overseas territory too (Japan and the USA, the UK and Cuba). Wikidata
  # has an item for each border, though, and a land border is an instance of
  # »land boundary« (Q15104814), with the two countries (P17).
  # Hans Island: Canada and Greenland, which is not part of this Denmark;
  # France and the United Kingdom: a border in the Channel Tunnel and at sea
  NOT_A_BORDER = [%w[CAN DNK], %w[FRA GBR]].freeze

  def load_borders(countries)
    by_item = countries.to_h { |c| [c[:wikidata], c[:code]] }
    INSTEAD.each { |kingdom, part| by_item[kingdom] = by_item[part] if by_item[part] }
    pairs = Set.new
    rows = @wikidata.select(<<~SPARQL)
      SELECT ?border ?country WHERE {
        ?border wdt:P31 wd:Q15104814 ; wdt:P17 ?country .
        FILTER NOT EXISTS { ?border wdt:P576 ?gone }
      }
    SPARQL
    rows.group_by { |r| r['border'] }.each_value do |list|
      codes = list.map { |r| by_item[self.class.qid(r['country'])] }.compact.uniq
      next unless codes.size == 2
      next if NOT_A_BORDER.include?(codes.sort)

      pairs << codes << codes.reverse
    end
    pairs.to_a.sort
  end

  def load_continents
    areas = statements(CONTINENTS.map(&:first), 'P2046', 'psn:P2046/wikibase:quantityAmount')
    CONTINENTS.map do |q, name|
      { wikidata: q, name: name, area: (area = latest(areas[q] || [])) && (area.to_f / 1_000_000).round }
    end
  end

  # --- Output -----------------------------------------------------------------

  def write(continents, countries, cities, borders)
    continent_name = continents.to_h { |c| [c[:wikidata], c[:name]] }
    countries.each { |c| c[:continent_name] = continent_name.fetch(c[:continent]) }
    neighbors = borders.group_by(&:first).transform_values { |list| list.map(&:last) }
    write_jsonl('continents.txt', continents.map { |c| c.slice(:name, :area, :wikidata) })
    write_jsonl('countries.txt', countries.map do |c|
      c.slice(:code, :name, :population, :area, :wikidata)
       .merge(continent: c[:continent_name], currency: c[:currency], tld: c[:tld], calling_code: c[:calling_code],
              driving_side: c[:driving_side], languages: c[:languages], neighbors: neighbors[c[:code]] || [])
    end)
    write_jsonl('cities.txt', cities.map { |c| c.slice(:id, :name, :country, :population, :latitude, :longitude, :is_capital, :wikidata) })
    write_jsonl('currencies.txt', currencies.map(&:compact))
    write_jsonl('languages.txt', languages.map(&:compact))
    tables = mysql_tables(continents, countries, cities, borders)
    mysql_writer('countries', FULL_TABLES, tables, parts: {
      'cities' => %w[continent currency country city], 'borders' => %w[border], 'languages' => %w[language country_language]
    }).write
    mysql_writer('countries-basic', BASIC_TABLES, basic(tables)).write(only_main: true)
    write_neo4j(continents, countries, cities, borders)
    write_readme(continents, countries, cities, borders)
  end

  def currencies = @currencies.values.sort_by { |c| c[:code] }
  def languages = @languages.values.sort_by { |l| l[:code] }

  def write_jsonl(name, rows)
    File.write(File.join(@output_dir, name), rows.map { |row| JSON.generate(row) }.join("\n") + "\n")
  end

  TABLE_SQL = {
    'continent' => <<~SQL,
      CREATE TABLE `continent` (
        `name` VARCHAR(50) NOT NULL,
        `area` INT NULL,
        PRIMARY KEY (`name`)
      ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
    SQL
    'currency' => <<~SQL,
      CREATE TABLE `currency` (
        `code` CHAR(3) NOT NULL,
        `name` VARCHAR(100) NOT NULL,
        PRIMARY KEY (`code`)
      ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
    SQL
    'language' => <<~SQL,
      CREATE TABLE `language` (
        `code` CHAR(3) NOT NULL,
        `name` VARCHAR(100) NOT NULL,
        PRIMARY KEY (`code`)
      ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
    SQL
    'country-basic' => <<~SQL,
      CREATE TABLE `country` (
        `code` CHAR(3) NOT NULL,
        `name` VARCHAR(100) NOT NULL,
        `population` INT NULL,
        `area` DECIMAL(12,2) NULL,
        `continent` VARCHAR(50) NOT NULL,
        PRIMARY KEY (`code`),
        UNIQUE KEY `uq_country_name` (`name`),
        KEY `idx_country_continent` (`continent`),
        CONSTRAINT `fk_country_continent`
          FOREIGN KEY (`continent`) REFERENCES `continent` (`name`)
      ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
    SQL
    'country' => <<~SQL,
      CREATE TABLE `country` (
        `code` CHAR(3) NOT NULL,
        `name` VARCHAR(100) NOT NULL,
        `population` INT NULL,
        `area` DECIMAL(12,2) NULL,
        `continent` VARCHAR(50) NOT NULL,
        `currency_code` CHAR(3) NULL,
        `tld` VARCHAR(10) NULL,
        `calling_code` VARCHAR(10) NULL,
        `driving_side` ENUM('left', 'right') NULL,
        PRIMARY KEY (`code`),
        UNIQUE KEY `uq_country_name` (`name`),
        KEY `idx_country_continent` (`continent`),
        KEY `idx_country_currency` (`currency_code`),
        CONSTRAINT `fk_country_continent`
          FOREIGN KEY (`continent`) REFERENCES `continent` (`name`),
        CONSTRAINT `fk_country_currency`
          FOREIGN KEY (`currency_code`) REFERENCES `currency` (`code`)
      ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
    SQL
    'city' => <<~SQL,
      CREATE TABLE `city` (
        `id` INT NOT NULL,
        `name` VARCHAR(100) NOT NULL,
        `country_code` CHAR(3) NOT NULL,
        `population` INT NULL,
        `latitude` DECIMAL(8,4) NOT NULL,
        `longitude` DECIMAL(8,4) NOT NULL,
        `is_capital` BOOLEAN NOT NULL,
        PRIMARY KEY (`id`),
        KEY `idx_city_country` (`country_code`),
        CONSTRAINT `fk_city_country`
          FOREIGN KEY (`country_code`) REFERENCES `country` (`code`)
      ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
    SQL
    'border' => <<~SQL,
      CREATE TABLE `border` (
        `country_code` CHAR(3) NOT NULL,
        `neighbor_code` CHAR(3) NOT NULL,
        PRIMARY KEY (`country_code`, `neighbor_code`),
        KEY `idx_border_neighbor` (`neighbor_code`),
        CONSTRAINT `fk_border_country`
          FOREIGN KEY (`country_code`) REFERENCES `country` (`code`),
        CONSTRAINT `fk_border_neighbor`
          FOREIGN KEY (`neighbor_code`) REFERENCES `country` (`code`)
      ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
    SQL
    'country_language' => <<~SQL
      CREATE TABLE `country_language` (
        `country_code` CHAR(3) NOT NULL,
        `language_code` CHAR(3) NOT NULL,
        PRIMARY KEY (`country_code`, `language_code`),
        KEY `idx_country_language_language` (`language_code`),
        CONSTRAINT `fk_country_language_country`
          FOREIGN KEY (`country_code`) REFERENCES `country` (`code`),
        CONSTRAINT `fk_country_language_language`
          FOREIGN KEY (`language_code`) REFERENCES `language` (`code`)
      ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
    SQL
  }.freeze
  # In an order that satisfies the foreign keys. The basic version for the
  # Einführung in SQL has the countries with their first columns, the
  # continents and the cities; the whole one drops and replaces its tables.
  FULL_TABLES = %w[continent currency language country city border country_language].freeze
  BASIC_TABLES = %w[continent country-basic city].freeze
  BASIC_COUNTRY_COLUMNS = %w[code name population area continent].freeze

  def mysql_tables(continents, countries, cities, borders)
    [
      ClassroomDataset::Table.new(name: 'continent', columns: %w[name area], primary_key: %w[name],
                                  rows: continents.map { |c| [c[:name], c[:area]] }),
      ClassroomDataset::Table.new(name: 'currency', columns: %w[code name], primary_key: %w[code],
                                  rows: currencies.map { |c| [c[:code], c[:name]] }),
      ClassroomDataset::Table.new(name: 'language', columns: %w[code name], primary_key: %w[code],
                                  rows: languages.map { |l| [l[:code], l[:name]] }),
      ClassroomDataset::Table.new(name: 'country', columns: BASIC_COUNTRY_COLUMNS + %w[currency_code tld calling_code driving_side],
                                  primary_key: %w[code],
                                  rows: countries.map do |c|
                                    [c[:code], c[:name], c[:population], c[:area], c[:continent_name],
                                     c[:currency], c[:tld], c[:calling_code], c[:driving_side]]
                                  end),
      ClassroomDataset::Table.new(name: 'city', columns: %w[id name country_code population latitude longitude is_capital], primary_key: %w[id],
                                  rows: cities.map { |c| [c[:id], c[:name], c[:country], c[:population], c[:latitude], c[:longitude], c[:is_capital] ? 1 : 0] }),
      ClassroomDataset::Table.new(name: 'border', columns: %w[country_code neighbor_code], primary_key: %w[country_code neighbor_code],
                                  rows: borders),
      ClassroomDataset::Table.new(name: 'country_language', columns: %w[country_code language_code], primary_key: %w[country_code language_code],
                                  rows: countries.flat_map { |c| c[:languages].map { |l| [c[:code], l] } })
    ]
  end

  # The tables of the basic version: country with its first columns only
  def basic(tables)
    tables.filter_map do |t|
      next unless %w[continent country city].include?(t.name)
      next t unless t.name == 'country'

      keep = BASIC_COUNTRY_COLUMNS.map { |column| t.columns.index(column) }
      ClassroomDataset::Table.new(name: t.name, columns: BASIC_COUNTRY_COLUMNS, primary_key: t.primary_key,
                                  rows: t.rows.map { |row| row.values_at(*keep) })
    end
  end

  def mysql_writer(name, schema, tables, parts: {})
    ClassroomDataset::MysqlWriter.new(
      name: name,
      output_dir: @output_dir,
      header: ['Country dataset of the Hackschule Workspace (https://github.com/specht/workspace),',
               "built from Wikidata (CC0 1.0) by src/scripts/collect-countries on #{Time.now.utc.strftime('%Y-%m-%d')}.",
               name == 'countries' ? 'See README.md for what each column means.' :
                                     'The basic version for the Einführung in SQL; countries.sql has everything. See README.md.'],
      schema: schema.map { |table| TABLE_SQL.fetch(table) }.join("\n"), tables: tables, parts: parts
    )
  end

  def write_neo4j(continents, countries, cities, borders)
    ClassroomDataset.write_neo4j(File.join(@output_dir, ClassroomDataset.neo4j_file('countries'))) do |neo|
      continent_node = continents.to_h { |c| [c[:name], neo.node(['Continent'], { name: c[:name], area: c[:area] }.compact)] }
      currency_node = currencies.to_h { |c| [c[:code], neo.node(['Currency'], { code: c[:code], name: c[:name] })] }
      language_node = languages.to_h { |l| [l[:code], neo.node(['Language'], { code: l[:code], name: l[:name] })] }
      country_node = countries.to_h do |c|
        [c[:code], neo.node(['Country'], { code: c[:code], name: c[:name], population: c[:population], area: c[:area],
                                           tld: c[:tld], calling_code: c[:calling_code], driving_side: c[:driving_side] }.compact)]
      end
      city_node = cities.to_h do |c|
        [c[:id], neo.node(['City'], { name: c[:name], population: c[:population], latitude: c[:latitude], longitude: c[:longitude] }.compact)]
      end
      countries.each do |c|
        neo.relationship(country_node[c[:code]], continent_node[c[:continent_name]], 'ON')
        neo.relationship(country_node[c[:code]], currency_node[c[:currency]], 'PAYS_WITH') if c[:currency]
        c[:languages].each { |l| neo.relationship(country_node[c[:code]], language_node[l], 'SPEAKS') }
      end
      cities.each do |c|
        neo.relationship(city_node[c[:id]], country_node[c[:country]], 'IN')
        neo.relationship(country_node[c[:country]], city_node[c[:id]], 'CAPITAL') if c[:is_capital]
      end
      # once per pair; a query follows it either way: (a)-[:BORDERS]-(b)
      borders.each { |a, b| neo.relationship(country_node[a], country_node[b], 'BORDERS') if a < b }
    end
  end

  def report(continents, countries, cities, borders)
    germany = countries.find { |c| c[:code] == 'DEU' }
    ClassroomDataset.report('Country dataset', {
      continents: continents.size, countries: countries.size, cities: cities.size,
      'border pairs' => borders.size / 2, currencies: currencies.size, languages: languages.size,
      'official languages (country, language)' => countries.sum { |c| c[:languages].size },
      'countries without population' => countries.count { |c| c[:population].nil? },
      'countries without capital' => countries.count { |c| cities.none? { |city| city[:is_capital] && city[:country] == c[:code] } },
      'countries without neighbours' => countries.count { |c| borders.none? { |a, _| a == c[:code] } },
      'Germany' => germany && "#{germany[:population]} people, #{germany[:area]} km², capital #{cities.find { |c| c[:is_capital] && c[:country] == 'DEU' }&.dig(:name)}, " \
                              "#{borders.count { |a, _| a == 'DEU' }} neighbours"
    }, @notes)
  end

  def write_readme(continents, countries, cities, borders)
    File.write(File.join(@output_dir, 'README.md'), <<~README)
      # Country dataset (Wikidata)

      The continents, the countries of the world, their cities, which countries share
      a border, their currencies and official languages, for the *Einführung in SQL*
      of the Hackschule Workspace and what comes after it. Built with
      `src/scripts/collect-countries` from https://github.com/specht/workspace.

      Two versions: `countries-basic.sql` has only `continent`, `country` (with its
      first five columns) and `city`, for the first steps; `countries.sql` has
      everything. Importing `countries.sql` later replaces the basic tables.

      Generated: #{Time.now.utc.iso8601}

      ## Source and licence

      All data comes from [Wikidata](https://www.wikidata.org/), which is available
      under [CC0 1.0](https://creativecommons.org/publicdomain/zero/1.0/): it may be
      copied, changed and shared without asking. Every continent, country and city in
      the `.txt` files carries its Wikidata item ID (`wikidata`).

      Which countries: the #{countries.size - 4} members of the United Nations, its two
      observer states (Vatican City, Palestine), Kosovo and Taiwan. For Denmark and the
      Netherlands, the country in Europe (without Greenland, the Faroe Islands and the
      Caribbean islands), not the kingdom as a whole.

      - `name`: the German label; #{NAMES.map { |code, name| "#{code} is called #{name}" }.join(', ')}
        instead of #{NAMES.keys.size == 1 ? 'its label' : 'their labels'}
      - `population` (P1082) and `area` (P2046, in km²): the value with the latest
        date, not a value for a part only (P518), of several with that date the one
        with preferred rank, of several with the same rank as well the middle one, the
        lower of two; may be empty
      - `continent`: the continent (P30), by its name; a country on several
        continents counts where its capital lies, as in atlases
        (#{CONTINENT_OF.keys.join(', ')} are set by hand)
      - `tld` (P78): the country's internet domain of two letters; of several the one
        with preferred rank, else the one of its two-letter code (`.de`, not
        `.deutschland`); `calling_code` (P474) with a `+`; `driving_side` (P1622),
        `left` or `right`
      - `currency_code` (P38): the current currency by its ISO 4217 code, of several the
        one with preferred rank; set by hand for #{CURRENCY_OF.map { |k, v| "#{k} (#{v})" }.join(', ')}.
        `currency.name` is its German label
      - `country_language` (P37): the official languages of the whole country (not of
        a region, as the languages of Russia's republics), by their ISO 639-3 code
        (P220); a variety without a code of its own counts as its language
        (#{LANGUAGE_CODE.size} items set by hand), a language without a code is left out.
        #{LANGUAGES_OF.keys.join(', ')} have no official language in Wikidata and get the
        language of the state
      - `is_capital`: the capital of its country (P36); with several in Wikidata,
        the one with preferred rank; set by hand for #{CAPITAL_OF.keys.join(', ')}
      - cities: every human settlement (P31, a subclass of Q486972, or a
        #{MUNICIPAL_CITY.values.join(', ')}) in a country that
        had at least #{MIN_PEOPLE} inhabitants at its latest count, and every capital,
        however small; population as Wikidata counts it, by the city's own borders;
        `latitude` and `longitude` from P625. Left out:
        - an area, a part of a city or a unit of administration: an item of one of
          #{NOT_A_CITY.size} kinds (#{NOT_A_CITY.values.first(6).join(', ')}, …, listed
          in the script as `NOT_A_CITY`), unless it is a kind of city (a subclass of
          city, Q515) too, other than #{NOT_CITY_ENOUGH.values.join(', ')}: Wikidata
          calls Berlin a federated state and Lagos a metropolitan area as well
        - an item that is no more than a human settlement or a metropolis and has other
          cities of the dataset in it (P131), as the union of Bordeaux and its neighbours
        - a place that lies in (P131) another city of the dataset, such as a borough of Berlin
        - single items: #{NOT_A_CITY_ITEM.values.join('; ')}
      - the same city twice: two items with the same name in the same country less
        than #{SAME_PLACE_KM} km apart (a municipality and its city, »Stadt Madrid« and
        Madrid) count once
      - a city that Wikidata places in two countries of the dataset goes with the
        country whose capital is nearer: Crimea and Donetsk with Ukraine, Prizren with
        Kosovo
      - borders: land borders only, from the border items of Wikidata that are an
        instance of land boundary (Q15104814), with the two countries (P17); not
        shares border with (P47), which also counts sea borders. A border of an
        overseas part counts where Wikidata records it as a land border (France and
        Brazil, through French Guiana; Spain and the United Kingdom, through
        Gibraltar; Cyprus and the United Kingdom, through its bases on the island);
        Canada and Denmark (Hans Island, Greenland) does not, since Greenland is not
        part of this Denmark, nor France and the United Kingdom (in the Channel
        Tunnel)

      Wikidata is edited by many people. A wrong number or a missing border is best
      fixed there, and the next build picks it up.

      ## Files

      - `continents.txt` — #{continents.size} continents, one JSON object per line
      - `countries.txt` — #{countries.size} countries, one JSON object per line, with their languages and neighbours
      - `cities.txt` — #{cities.size} cities, one JSON object per line
      - `currencies.txt` — #{currencies.size} currencies, `languages.txt` — #{languages.size} languages
      - `countries-basic.sql` — the basic version: `continent`, `country`, `city`, schema and data
      - `countries.sql` — everything, schema and data
      - `countries-schema.sql` — schema only
      - `countries-data.sql` — data only, for a schema built in class
      - `countries-data-cities.sql` — data only: `continent`, `currency`, `country`, `city`
      - `countries-data-borders.sql` — data only: `border`
      - `countries-data-languages.sql` — data only: `language`, `country_language`
      - `countries-neo4j.dump` — logical Neo4jBolt dump for `neo4j_bolt load`

      Every classroom dataset names its files this way, after the dataset (`countries`, `movies`, `music`).

      ## MySQL

      ```
      continent(name, area)
      currency(code, name)
      language(code, name)
      country(code, name, population, area, continent → continent, currency_code → currency, tld, calling_code, driving_side)
      city(id, name, country_code → country, population, latitude, longitude, is_capital)
      border(country_code → country, neighbor_code → country)
      country_language(country_code → country, language_code → language)
      ```

      The basic version: `continent`, `country(code, name, population, area,
      continent → continent)` and `city` as above.

      A continent is identified by its name, a country by its ISO 3166-1 alpha-3 code
      (`DEU`, `FRA`; Kosovo, which has none, by `XKS`), a city by its `id`, numbered
      by country and name; city names are not unique. Every country has
      exactly one city with `is_capital` = 1 (TRUE). `border` has a row in both directions for
      each pair of neighbours: Germany and France are `('DEU', 'FRA')` and
      `('FRA', 'DEU')`: a deliberate redundancy, so that a plain `JOIN` finds the
      neighbours of a country. A currency is identified by its ISO 4217 code (`EUR`), a
      language by its ISO 639-3 code (`deu`, `fra`). A continent has no population of its own; it is the sum of its
      countries (`GROUP BY`). Antarctica has no countries.

      ```sh
      mysql < countries.sql
      ```

      ## Neo4j

      `Continent`, `Country`, `City`, `Currency` and `Language` nodes;
      `(Country)-[:ON]->(Continent)`, `(City)-[:IN]->(Country)`,
      `(Country)-[:CAPITAL]->(City)`, `(Country)-[:PAYS_WITH]->(Currency)`,
      `(Country)-[:SPEAKS]->(Language)` and
      `(Country)-[:BORDERS]->(Country)` once per pair, to be followed either way:
      `(a:Country)-[:BORDERS]-(b:Country)`.

      ```sh
      neo4j_bolt load countries-neo4j.dump
      ```
    README
  end
end

options = { output_dir: 'country-archive', offline: false, pause: 1.0 }

parser = OptionParser.new do |opts|
  opts.banner = 'Usage: collect-countries.rb [options]'
  opts.on('-o', '--output DIR', "Output directory (default: #{options[:output_dir]})") { |dir| options[:output_dir] = dir }
  opts.on('--offline', 'Use only cached Wikidata answers') { options[:offline] = true }
  opts.on('--pause SECONDS', Float, "Pause between Wikidata requests (default: #{options[:pause]})") { |s| options[:pause] = s }
end

if $PROGRAM_NAME == __FILE__
  parser.parse!
  WikidataCountryBuilder.new(**options).run
end
