#!/usr/bin/env ruby

URL = "https://data.discogs.com/?download=data%2F2026%2Fdiscogs_20260201"

['artists', 'masters', 'labels', 'releases'].each do |key|
    url = "#{URL}_#{key}.xml.gz"
    system("curl -C - -L #{url} -o #{key}.xml.gz")
end
