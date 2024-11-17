#!/usr/bin/env ruby

require 'json'
require 'mysql2'

client = Mysql2::Client.new(
    host: ENV['MYSQL_HOST'],
    username: ENV['MYSQL_USER'],
    password: ENV['MYSQL_PASSWORD'],
    database: ENV['MYSQL_DATABASE']
)

job_id = {}
File.open('movies.txt') do |f|
    f.each_line do |line|
        data = JSON.parse(line)
        data['crew'].keys.each do |job|
            job_id[job] ||= job_id.size + 1
        end
    end
end

query = client.prepare("INSERT IGNORE INTO job (id, title) VALUES (?, ?)")
job_id.each_pair do |job, id|
    puts "Importing job: #{job}"
    query.execute(id, job)
end

File.open('genres.txt') do |f|
    query = client.prepare("INSERT IGNORE INTO genre (id, name) VALUES (?, ?)")
    f.each_line do |line|
        data = JSON.parse(line)
        puts "Importing genre: #{data['name']}"
        query.execute(data['id'], data['name'])
    end
end

File.open('crew.txt') do |f|
    query = client.prepare("INSERT IGNORE INTO crew (id, name, birth_year, death_year) VALUES (?, ?, ?, ?)")
    f.each_line do |line|
        data = JSON.parse(line)
        puts "Importing crew: #{data['birth_year']} - #{data['name']}"
        query.execute(data['id'], data['name'], data['birth_year'], data['death_year'])
    end
end

File.open('movies.txt') do |f|
    query = client.prepare("INSERT IGNORE INTO movie (id, title, german_title, year, runtime, rating) VALUES (?, ?, ?, ?, ?, ?)")
    query2 = client.prepare("INSERT IGNORE INTO movie_genre (movie_id, genre_id) VALUES (?, ?)")
    query3 = client.prepare("INSERT IGNORE INTO movie_crew (movie_id, crew_id, job_id) VALUES (?, ?, ?)")
    f.each_line do |line|
        data = JSON.parse(line)
        puts "Importing movie: #{data['year']} - #{data['title']}"
        query.execute(data['id'], data['title'], data['german_title'], data['year'], data['runtime'], data['rating'])
        data['genres'].each do |genre|
            query2.execute(data['id'], genre)
        end
        data['crew'].each_pair do |job, crew_ids|
            crew_ids.each do |crew_id|
                query3.execute(data['id'], crew_id, job_id[job])
            end
        end
    end
end
