#!/usr/bin/env ruby

require 'json'
require 'open3'

class BufferedReader
    def initialize(io)
        @io = io
        @buffer = ''
        @offset = 0
    end

    def read(size)
        while @buffer.size - @offset < size
            @buffer += @io.read(1024 * 1024)
        end
        result = @buffer[@offset, size]
        @offset += size
        result
    end

    def eof?
        @io.eof? && @buffer.size == @offset
    end
end

%w(artist label release master).each do |key|
    ks = key.size + 3
    next unless key == 'master'
    File.open("index-#{key}s.txt", 'w') do |f|
        info = {}
        count = 0
        Open3.popen2("pigz -cd #{key}s.xml.gz") do |stdin, stdout, wait_thr|
            xml = ''
            stdout.readline
            stdout.each_line do |line|
                xml += line
                if line[line.size - ks - 1, ks] == "</#{key}>"
                    begin
                        if key == 'master'
                            id = xml.match(/<master id="(\d+)">/)[1]
                            name = xml.match(/<title>([^<]+)<\/title>/)[1]
                        else
                            id = xml.match(/<id>(\d+)<\/id>/)[1]
                            if key == 'release'
                                name = xml.match(/<title>([^<]+)<\/title>/)[1]
                            else
                                name = xml.match(/<name>([^<]+)<\/name>/)[1]
                            end
                        end
                        name.gsub!(/\(\d+\)$/, '')
                        # STDERR.puts "#{id} #{name}"
                        f.puts "#{id} #{name}"
                        # info[id] = name
                    rescue
                        STDERR.puts xml
                    end
                    xml = ''
                end
            end
        end
    end
end
