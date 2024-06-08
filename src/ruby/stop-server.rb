#!/usr/bin/env ruby

container_name = ARGV[0]
system("docker kill #{container_name}")
system("docker rm #{container_name}")