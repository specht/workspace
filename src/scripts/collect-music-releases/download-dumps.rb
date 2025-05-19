#!/usr/bin/env ruby

URL = "https://discogs-data-dumps.s3-us-west-2.amazonaws.com/data/2025/discogs_20250501"

['artists', 'masters', 'labels', 'releases'].each do |key|
    url = "#{URL}_#{key}.xml.gz"
    system("curl -L #{url} -o #{key}.xml.gz")
end
