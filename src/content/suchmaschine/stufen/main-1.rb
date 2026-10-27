require_relative 'search'

pages = crawl('/index.html', method(:fetch_page))
puts "#{pages.size} Seiten gefunden: #{pages.keys.join(', ')}"
