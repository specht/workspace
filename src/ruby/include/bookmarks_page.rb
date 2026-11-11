# The routes of the bookmarks in the table of contents (Bookmarks,
# include/bookmarks.rb): list a page's, set or remove one. Any logged-in
# person, only their own.
class Main < Sinatra::Base
    # The person's bookmarks on a page: { anchors: [...] }.
    post '/api/bookmarks/list' do
        assert(user_logged_in?)
        data = parse_request_data(:required_keys => [:page], :max_value_lengths => { :page => 220 })
        page = Bookmarks.page(data[:page])
        halt 400, { :error => 'page' }.to_json unless page
        anchors = neo4j_query(<<~END_OF_QUERY, :email => my_email, :page => page).map { |row| row['anchor'] }
            MATCH (b:Bookmark {page: $page})-[:BY]->(:User {email: $email})
            RETURN b.anchor AS anchor
            ORDER BY b.created_at;
        END_OF_QUERY
        respond(:anchors => anchors)
    end

    # Sets (on: 'true') or removes (on: 'false') a bookmark.
    post '/api/bookmarks/set' do
        assert(user_logged_in?)
        data = parse_request_data(:required_keys => [:page, :anchor, :on],
            :max_value_lengths => { :page => 220, :anchor => 220, :on => 5 })
        page = Bookmarks.page(data[:page])
        anchor = Bookmarks.anchor(data[:anchor])
        halt 400, { :error => 'page' }.to_json unless page && anchor
        if data[:on].to_s == 'true'
            count = neo4j_query_expect_one(<<~END_OF_QUERY, :email => my_email, :page => page)['n']
                MATCH (b:Bookmark {page: $page})-[:BY]->(:User {email: $email})
                RETURN count(b) AS n;
            END_OF_QUERY
            halt 400, { :error => 'too_many' }.to_json if count >= Bookmarks::MAX_PER_PAGE
            neo4j_query(<<~END_OF_QUERY, :email => my_email, :page => page, :anchor => anchor, :now => Time.now.to_i)
                MATCH (u:User {email: $email})
                MERGE (b:Bookmark {page: $page, anchor: $anchor})-[:BY]->(u)
                SET b.created_at = coalesce(b.created_at, $now);
            END_OF_QUERY
        else
            neo4j_query(<<~END_OF_QUERY, :email => my_email, :page => page, :anchor => anchor)
                MATCH (b:Bookmark {page: $page, anchor: $anchor})-[:BY]->(:User {email: $email})
                DETACH DELETE b;
            END_OF_QUERY
        end
        respond(:ok => true)
    end
end
