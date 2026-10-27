require_relative 'webcrawler'

start = ARGV[0] || 'http://localhost:8000/index.html'
pages = polite_crawl(start)
puts "#{pages.size} Seiten geholt"
