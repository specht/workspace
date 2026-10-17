require 'minitest/autorun'
require_relative '../include/schemas'

# The tables of a dataset as schema lines (_schema(name)), from the JSON of
# the Query Stepper.
class SchemasTest < Minitest::Test
    def test_a_line_underlines_the_key_and_marks_foreign_keys
        table = { 'columns' => %w[movie_id genre_id], 'key' => %w[movie_id genre_id],
                  'references' => { 'movie_id' => 'movie.id', 'genre_id' => 'genre.id' } }
        line = Schemas.line('movie_genre', table)
        assert_equal "<div><span class='schema-table'>movie_genre</span>(" \
            "<u><span class='schema-fk' title='verweist auf movie.id'><span class='schema-arrow'>↑</span>movie_id</span></u>, " \
            "<u><span class='schema-fk' title='verweist auf genre.id'><span class='schema-arrow'>↑</span>genre_id</span></u>)</div>", line
        assert_equal "<div><span class='schema-table'>job</span>(<u>id</u>, title)</div>",
            Schemas.line('job', { 'columns' => %w[id title], 'key' => %w[id] })
    end

    def test_the_datasets_of_the_sql_zeitlupe
        countries = Schemas.html('countries')
        %w[continent country city border].each { |table| assert_includes countries, ">#{table}</span>(" }
        assert_includes countries, "<span class='schema-table'>city</span>(<u>id</u>, name, " \
            "<span class='schema-fk' title='verweist auf country.code'><span class='schema-arrow'>↑</span>country_code</span>, population,"
        movies = Schemas.html('movies')
        %w[movie genre movie_genre crew job movie_crew].each { |table| assert_includes movies, ">#{table}</span>(" }
        assert_includes movies, "<u><span class='schema-fk' title='verweist auf job.id'><span class='schema-arrow'>↑</span>job_id</span></u>)"
    end

    def test_expand_a_line_of_its_own_and_report_an_unknown_dataset
        warnings = []
        out = Schemas.expand("Vorher\n\n_schema(countries)\n\n_schema(nothing)\n`_schema(countries)` im Text\n", warn: ->(m) { warnings << m })
        assert_includes out, "<div class='schema-lines'"
        assert_includes out, "_schema(nothing)"
        assert_includes out, "`_schema(countries)` im Text"
        assert_equal ['_schema(nothing): no such dataset'], warnings
    end
end
