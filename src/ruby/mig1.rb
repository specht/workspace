#!/usr/bin/env ruby
require 'neo4j_bolt'
require './credentials.rb'
require 'digest'

Neo4jBolt.bolt_host = 'neo4japp'
Neo4jBolt.bolt_port = 7687

CONSTRAINTS_LIST = [
    'Code/sha1',
    'Database/name',
    'File/sha1',
    'Language/name',
    'LoginRequest/tag',
    'Session/sid',
    'Solved/id',
    'Submission/id',
    'Task/name',
    'Test/tag',
    'TIC80Dir/path',
    'TIC80File/path',
    'User/email',
    'User/server_tag',
    'User/share_tag',
]
INDEX_LIST = [
    'Submission/success',
    'Test/running',
]

class Migration
    include Neo4jBolt

    def assert(message = nil, &block)
        unless yield
            STDERR.puts message
            raise 'assertion failed.'
        end
    end

    def run
        transaction do
            submissions = []
            neo4j_query("MATCH (u:User)<-[:BY]-(s:Submission)-[:FOR]->(t:Task) RETURN s, t, u").each do |row|
                STDERR.puts "Migrating #{row['s']}..."
                t = row['t']
                u = row['u']
                submission = {}
                [:sha1, :line_count, :size, :success, :lang, :ts].each do |key|
                    submission[key] = row['s'][key]
                end
                submission[:email] = u[:email]
                submission[:task] = t[:name]
                submissions << submission
            end
            STDERR.puts "Migrating #{submissions.size} submissions..."
            neo4j_query("MATCH (s:Submission) DETACH DELETE s")
            submissions.each do |submission|
                sha1 = submission[:sha1]
                STDERR.puts "Migrating submission #{sha1}..."
                line_count = submission[:line_count]
                size = submission[:size]
                success = submission[:success]
                lang = submission[:lang]
                ts = submission[:ts]
                email = submission[:email]
                task = submission[:task]
                submission_id = Digest::SHA1.hexdigest("#{task}-#{lang}-#{sha1}")
                neo4j_query(<<~END_OF_STRING, {:email => email, :task => task, :lang => lang, :sha1 => sha1, :line_count => line_count, :size => size, :ts => ts, :success => success, :submission_id => submission_id})
                    MERGE (u:User {email: $email})
                    MERGE (t:Task {name: $task})
                    MERGE (l:Language {name: $lang})
                    MERGE (c:Code {sha1: $sha1})
                    ON CREATE SET c.line_count = $line_count, c.size = $size

                    WITH u,t,l,c

                    MERGE (s:Submission {id: $submission_id})
                    ON CREATE SET s.success = $success
                    ON MATCH  SET s.success = $success

                    MERGE (s)-[r:BY]->(u)
                    ON CREATE SET r.ts = $ts
                    MERGE (s)-[:FOR]->(t)
                    MERGE (s)-[:IN]->(l)
                    MERGE (s)-[:WITH]->(c);
                END_OF_STRING
                solved_id = Digest::SHA1.hexdigest("#{email}-#{task}-#{lang}")
                if success
                    neo4j_query(<<~END_OF_STRING, {:email => email, :task => task, :lang => lang, :solved_id => solved_id})
                       MERGE (u:User {email: $email})
                       MERGE (t:Task {name: $task})
                       MERGE (l:Language {name: $lang})
                       MERGE (s:Solved {id: $solved_id})
                       MERGE (s)-[:BY]->(u)
                       MERGE (s)-[:FOR]->(t)
                       MERGE (s)-[:IN]->(l)
                    END_OF_STRING
                end
            end
            # raise 'nope!'
        end
        setup_constraints_and_indexes(CONSTRAINTS_LIST, INDEX_LIST)
    end
end

dump = Migration.new
dump.run
