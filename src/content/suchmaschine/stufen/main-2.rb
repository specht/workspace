require_relative 'search'

pages = crawl('/index.html', method(:fetch_page))
puts "#{pages.size} Seiten gefunden: #{pages.keys.join(', ')}"
index = build_index(pages)

loop do
  print 'Suche: '
  query = gets
  break if query.nil? || query.strip.empty?

  results = search(index, query)
  puts '  Nichts gefunden.' if results.empty?
  results.each { |path| puts "  #{path}" }
end
