#!/usr/bin/env ruby

Dir['previews/**/*.png'].each do |path|
   next unless true || path.include?('commercial'); p = path.split('.').first; command = "convert #{p}.png -resize 400% -fuzz 0% -transparent white -resize 25% #{p}.webp"; system(command)
end
Dir['previews/**/*.png'].each do |path|
   next unless true || path.include?('commercial'); command = "rm \"#{path}\""; system(command)
end
