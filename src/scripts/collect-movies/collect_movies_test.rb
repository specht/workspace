# Offline test of the movie collector: canned Wikidata answers, no network.
#   ruby collect_movies_test.rb
require 'minitest/autorun'
require 'tmpdir'
load File.join(__dir__, 'collect-movies.rb')

class CollectMoviesTest < Minitest::Test
    E = 'http://www.wikidata.org/entity/'

    class Selection
        def ids = %w[tt0133093 tt0114709 tt9999999 tt0000001]
    end

    # Answers by what the query asks for.
    class Wikidata
        attr_reader :requests, :queries

        def initialize
            @requests = 0
            @queries = []
        end

        def select(query)
            @queries << query
            e = CollectMoviesTest::E
            if query.include?('wdt:P345')
                [
                    { 'imdb' => 'tt0133093', 'film' => "#{e}Q83495", 'en' => 'The Matrix', 'de' => 'Matrix', 'original' => 'The Matrix',
                      'date' => '1999-03-31T00:00:00Z', 'amount' => '+136', 'unit' => "#{e}Q7727" },
                    { 'imdb' => 'tt0133093', 'film' => "#{e}Q83495", 'en' => 'The Matrix', 'de' => 'Matrix',
                      'date' => '1999-06-17T00:00:00Z', 'amount' => '+136', 'unit' => "#{e}Q7727" },
                    { 'imdb' => 'tt0114709', 'film' => "#{e}Q171048", 'en' => 'Toy Story', 'de' => 'Toy Story',
                      'date' => '1995-11-22T00:00:00Z', 'amount' => '+1.35', 'unit' => "#{e}Q25235" },
                    # no runtime: left out
                    { 'imdb' => 'tt0000001', 'film' => "#{e}Q1", 'en' => 'Short', 'date' => '1894-01-01T00:00:00Z' },
                ]
            elsif query.include?('wdt:P136')
                [
                    { 'film' => "#{e}Q83495", 'label' => 'action film' },
                    { 'film' => "#{e}Q83495", 'label' => 'science fiction film' },
                    { 'film' => "#{e}Q83495", 'label' => 'cyberpunk' },
                    { 'film' => "#{e}Q171048", 'label' => 'computer-animated film' },
                    { 'film' => "#{e}Q171048", 'label' => 'comedy film' },
                ]
            elsif query.include?('wdt:P161')
                [
                    { 'film' => "#{e}Q83495", 'prop' => 'P57', 'person' => "#{e}Q9545711", 'links' => '40' },
                    { 'film' => "#{e}Q83495", 'prop' => 'P57', 'person' => "#{e}Q9544977", 'links' => '42' },
                    { 'film' => "#{e}Q83495", 'prop' => 'P58', 'person' => "#{e}Q9545711", 'links' => '40' },
                    { 'film' => "#{e}Q83495", 'prop' => 'P161', 'person' => "#{e}Q43416", 'links' => '100' },
                    { 'film' => "#{e}Q171048", 'prop' => 'P161', 'person' => "#{e}Q2263", 'links' => '90' },
                    { 'film' => "#{e}Q171048", 'prop' => 'P86', 'person' => "#{e}Q500", 'links' => '5' },
                ]
            elsif query.include?('wdt:P569')
                [
                    { 'person' => "#{e}Q9545711", 'en' => 'Lana Wachowski', 'birth' => '1965-06-21T00:00:00Z' },
                    { 'person' => "#{e}Q9544977", 'en' => 'Lilly Wachowski', 'birth' => '1967-12-29T00:00:00Z' },
                    { 'person' => "#{e}Q43416", 'en' => 'Keanu Reeves', 'birth' => '1964-09-02T00:00:00Z' },
                    { 'person' => "#{e}Q2263", 'en' => 'Tom Hanks', 'birth' => '1956-07-09T00:00:00Z' },
                    # only a German label, impossible death year
                    { 'person' => "#{e}Q500", 'de' => 'Randy Newman', 'birth' => '1943-11-28T00:00:00Z', 'death' => '1900-01-01T00:00:00Z' },
                ]
            else
                raise "unexpected query #{query}"
            end
        end
    end

    def build
        Dir.mktmpdir do |dir|
            wikidata = Wikidata.new
            builder = WikidataMovieBuilder.new(output_dir: dir, selection: Selection.new, wikidata: wikidata)
            capture_io { builder.run }
            files = Dir.children(dir).to_h { |f| [f, File.read(File.join(dir, f))] }
            yield files, wikidata
        end
    end

    # The query service gives up on credit queries with more than one film:
    # the builder splits the batch and still gets everything.
    class SlowWikidata < Wikidata
        def select(query)
            if query.include?('wdt:P161') && query.scan(/wd:Q\d+/).uniq.size > 2
                @queries << query
                raise WikidataClient::Failed, 'query timed out on the server'
            end
            super
        end
    end

    def test_slow_batches_are_split
        Dir.mktmpdir do |dir|
            wikidata = SlowWikidata.new
            capture_io { WikidataMovieBuilder.new(output_dir: dir, selection: Selection.new, wikidata: wikidata).run }
            movies = File.readlines(File.join(dir, 'movies.txt')).map { |l| JSON.parse(l) }
            assert_equal ['Lilly Wachowski', 'Lana Wachowski'].size, movies.last['crew']['director'].size
            assert(wikidata.queries.count { |q| q.include?('wdt:P161') } >= 3, 'tried the whole batch, then halves')
        end
    end

    def test_helpers
        assert_equal 1999, WikidataMovieBuilder.year('1999-03-31T00:00:00Z')
        assert_equal(-500, WikidataMovieBuilder.year('-0500-01-01T00:00:00Z'))
        assert_equal 81, WikidataMovieBuilder.minutes('+1.35', WikidataMovieBuilder::HOUR)
        assert_equal 2, WikidataMovieBuilder.minutes('120', WikidataMovieBuilder::SECOND)
        assert_equal %w[Action Sci-Fi], WikidataMovieBuilder.genres_for('superhero science fiction film').sort
        assert_equal ['War'], WikidataMovieBuilder.genres_for('war film')
        assert_equal [], WikidataMovieBuilder.genres_for('award-winning film')
        assert_equal ['Animation'], WikidataMovieBuilder.genres_for('computer-animated film')
    end

    def test_build
        build do |files, wikidata|
            movies = files['movies.txt'].lines.map { |l| JSON.parse(l) }
            assert_equal ['Toy Story', 'The Matrix'], movies.map { |m| m['title'] }, 'sorted by year, the film without runtime left out'
            toy, matrix = movies
            assert_equal 81, toy['runtime']
            assert_nil toy['german_title'], 'same as the title'
            assert_equal 'Matrix', matrix['german_title']
            assert_nil matrix['original_title']
            assert_equal 1999, matrix['year']
            assert_equal 'Q83495', matrix['wikidata']
            genres = files['genres.txt'].lines.map { |l| JSON.parse(l)['name'] }
            assert_equal %w[Action Animation Comedy Sci-Fi], genres

            data = files['movies-data-films.sql']
            assert_includes data, 'INSERT INTO `movie` (`id`, `title`, `original_title`, `german_title`, `year`, `runtime`) VALUES'
            refute_includes files['movies.sql'], 'rating'
            crew_sql = files['movies-data-crew.sql']
            assert_includes crew_sql, "'Randy Newman', 1943, NULL"
            assert_includes crew_sql, "(1, 'actor')"
            refute_includes files['movies.sql'], 'imdb', 'no IMDb data in the output'
            assert_includes files['README.md'], 'CC0'
            assert_includes files['movies-neo4j.dump'], 'DIRECTOR'

            crew = files['crew.txt'].lines.map { |l| JSON.parse(l) }
            lana = crew.find { |p| p['name'] == 'Lana Wachowski' }
            matrix_jobs = matrix['crew'].transform_values { |ids| ids.map { |id| crew.find { |p| p['id'] == id }['name'] } }
            assert_equal ['Lilly Wachowski', 'Lana Wachowski'], matrix_jobs['director'], 'best known first'
            assert_equal ['Lana Wachowski'], matrix_jobs['writer']
            assert_equal 1, crew.count { |p| p['id'] == lana['id'] }, 'one person, two jobs'
            assert(wikidata.queries.all? { |q| q.include?('SELECT') })
            assert_includes files['movies-data-rental.sql'], 'part "rental" (copy, customer, loan)'
            assert_includes files['movies-schema.sql'], 'PRIMARY KEY (`customer_id`, `copy_id`, `loan_date`)'
        end
    end

    # The invented video shop: the same rows for the same films, a loan
    # without return_date only at the end, the page's examples on the shelf
    def test_rental
        movies = (1..400).map { |i| { id: i, title: "Film #{i}", year: 1950 + i % 76 } }
        movies << { id: 401, title: 'Back to the Future', year: 1985 }
        rental = Rental.build(movies)
        assert_equal rental, Rental.build(movies)
        copies, customers, loans = rental.values_at(:copies, :customers, :loans)
        assert_equal Rental::CUSTOMERS, customers.map(&:last).uniq.size
        assert_equal loans.size, loans.map { |l| l.first(3) }.uniq.size, 'the primary key is unique'
        future = copies.select { |c| c[1] == 401 }
        assert_equal %w[DVD Blu-ray], future.map(&:last)
        blu_ray = future.last.first
        assert loans.none? { |l| l[1] == blu_ray }, 'the Blu-ray of Back to the Future is never lent'
        assert loans.any? { |l| l[3].nil? }
        loans.group_by { |l| l[1] }.each_value do |list|
            list.sort_by { |l| l[2] }.each_cons(2) do |a, b|
                refute_nil a[3], 'only the last loan of a copy may be open'
                assert a[3] <= b[2], 'a copy is back before it is lent again'
            end
        end
    end
end
