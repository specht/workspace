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

# Für jedes Wort die Seiten, auf denen es steht
def build_index(pages)
  index = {}
  pages.each do |path, html|
    words(html).uniq.each do |word|
      index[word] ||= []
      index[word] << path
    end
  end
  index
end

# Die Seiten, auf denen jedes Wort der Anfrage steht, nach dem Alphabet
def search(index, query)
  results = nil
  words(query).each do |word|
    found = index[word] || []
    results = results.nil? ? found : results & found
  end
  (results || []).sort
end

# Die Treffer, die Seite mit dem Wort am häufigsten zuerst
def rank_by_count(results, pages, word)
  results.sort_by { |path| [-words(pages[path]).count(word), path] }
end
