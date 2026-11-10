# Bookmarks in the table of contents on the left (»Auf dieser Seite«): a
# logged-in person marks headings of a page, as many as they like, and finds
# them again on any computer. A bookmark is a page (its path) and the anchor
# of a heading; nothing else. Stored as Bookmark nodes with [:BY] to the
# User; they go with the account (account_deletion.rb, account_export.rb,
# privacy.rb). bookmarks.js draws them; the routes are in bookmarks_page.rb.
# Plain Ruby, so that it can be tested without Sinatra.
module Bookmarks
    # at most this many on one page, so that a script cannot fill the store
    MAX_PER_PAGE = 60
    PAGE = %r{\A/[a-z0-9_\-/]{0,200}\z}
    ANCHOR = /\A[\p{L}\p{N}_\-]{1,200}\z/u

    # The path of a page as bookmarks keep it, or nil: lower case letters,
    # digits, - _ and /, without a slash at the end.
    def self.page(path)
        path = path.to_s.strip
        path = path.chomp('/') while path.length > 1 && path.end_with?('/')
        path.match?(PAGE) ? path : nil
    end

    # The anchor of a heading (the id the table of contents gives it), or nil.
    def self.anchor(text)
        text = text.to_s.strip
        text.match?(ANCHOR) ? text : nil
    end
end
