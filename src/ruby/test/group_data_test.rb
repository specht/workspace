require 'minitest/autorun'
require_relative '../include/group_data'

# What hangs on a group's name (GroupSetting, Pin) goes with the group:
# when it is deleted, when it is gone from the roster, and before a new
# group of that name is created (GroupData).
class GroupDataTest < Minitest::Test
    class FakeDb
        attr_reader :queries

        def initialize
            @queries = []
        end

        def neo4j_query(query, params = {})
            @queries << [query, params]
            []
        end
    end

    def test_groups_gone_from_the_roster
        assert_equal ['10b'], GroupData.vanished(['10a', '10b', 'Testgruppe (A)'], ['10a', 'Testgruppe (A)', '11'])
        assert_equal [], GroupData.vanished(['10a'], ['10a', '11'])
        # The first reading has nothing before it.
        assert_equal [], GroupData.vanished([], ['10a'])
        # A roster without any group was not read; it did not lose them all.
        assert_equal [], GroupData.vanished(['10a', '10b'], [])
        # A rename is a group that is gone; its settings moved before (the
        # rename's transaction), so dropping the old name finds nothing.
        assert_equal ['10b'], GroupData.vanished(['10b'], ['10b Informatik'])
    end

    def test_drop_takes_settings_and_pins_of_those_names
        db = FakeDb.new
        GroupData.drop!(db, ['10b', '10b', '', nil])
        # first the folders of the group's Git repositories, then the nodes
        assert_equal 2, db.queries.size
        assert_match(/g\.git_id/, db.queries.first[0])
        query, params = db.queries.last
        assert_match(/n:GroupSetting OR n:Pin/, query)
        assert_match(/n\.group IN \$names/, query)
        assert_equal({ :names => ['10b'] }, params)
        GroupData.drop!(db, [])
        assert_equal 2, db.queries.size
    end

    def test_import_keeps_the_imported_and_the_other_known_groups
        db = FakeDb.new
        GroupData.drop_all_but!(db, ['10a', '10b', 'Testgruppe (A)', '10a'])
        query, params = db.queries.last
        assert_match(/n:GroupSetting OR n:Pin/, query)
        assert_match(/NOT n\.group IN \$keep/, query)
        assert_equal({ :keep => ['10a', '10b', 'Testgruppe (A)'] }, params)
        # Never everything at once: without any group to keep, nothing goes.
        GroupData.drop_all_but!(db, [])
        assert_equal 2, db.queries.size
    end

    # The places that create, delete, import and reread groups use it.
    def test_wired_where_groups_come_and_go
        root = File.expand_path('..', __dir__)
        groups_page = File.read(File.join(root, 'include', 'groups_page.rb'))
        create = groups_page[/post '\/api\/groups\/create' do.*?\n    end\n/m]
        delete = groups_page[/post '\/api\/groups\/delete' do.*?\n    end\n/m]
        assert_operator create.index('GroupData.drop!'), :<, create.index('CREATE (g:Group'), 'leftovers go before the group is created'
        assert_includes delete, 'GroupData.drop!($neo4j, [name])'
        assert_includes File.read(File.join(root, 'include', 'roster_store.rb')), 'GroupData.drop_all_but!'
        assert_includes File.read(File.join(root, 'main.rb')), 'GroupData.vanished('
    end
end
