require 'net/http'

# Wo die Website läuft, die die Suchmaschine durchsucht
SITE = 'http://localhost:8000'

# Eine Seite der Website als String, oder nil, wenn es sie nicht gibt
def fetch_page(path)
  response = Net::HTTP.get_response(URI(SITE + path))
  response.is_a?(Net::HTTPSuccess) ? response.body.force_encoding('UTF-8') : nil
end

# Die Ziele der Links einer Seite, die auf derselben Website bleiben
def links(html)
  html.scan(/<a href="(\/[^"#]*)"/).flatten
end

# Die Wörter einer Seite, klein geschrieben, ohne die Tags
def words(html)
  html.gsub(/<[^>]*>/, ' ').downcase.scan(/\p{L}+/)
end

# Alle Seiten, die von start aus über Links erreichbar sind, als Hash von
# Pfad zu HTML; fetch holt eine Seite
def crawl(start, fetch)
  pages = {}
  queue = [start]
  seen = [start]
  until queue.empty?
    path = queue.shift
    html = fetch.call(path)
    next if html.nil?

    pages[path] = html
    links(html).each do |link|
      next if seen.include?(link)

      seen << link
      queue << link
    end
  end
  pages
end
