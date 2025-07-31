#!/usr/bin/env ruby
require 'yaml'

def print_progress_bar(percentage, width = 40)
    percentage = [[percentage, 0].max, 100].min  # Clamp to [0, 100]

    # Calculate the number of blocks to print
    full_blocks = (percentage * width / 100.0).floor
    empty_blocks = width - full_blocks

    # ANSI escape sequences
    fg_yellow = "\e[96m"
    bg_yellow = "\e[90m"
    reset = "\e[0m"

    # Unicode full block character
    block = '━'
    space = '━'

    bar = fg_yellow + #bg_yellow +
          (block * full_blocks) +
          reset + bg_yellow +
          (space * empty_blocks) +
          reset

    "[#{bar}]"
end

def bytes_to_str(ai_Size)
    if ai_Size < 1024
        return "#{ai_Size} B"
    elsif ai_Size < 1024 * 1024
        return "#{sprintf('%1.1f', ai_Size.to_f / 1024.0)} kB"
    elsif ai_Size < 1024 * 1024 * 1024
        return "#{sprintf('%1.1f', ai_Size.to_f / 1024.0 / 1024.0)} MB"
    elsif ai_Size < 1024 * 1024 * 1024 * 1024
        return "#{sprintf('%1.1f', ai_Size.to_f / 1024.0 / 1024.0 / 1024.0)} GB"
    end
    return "#{sprintf('%1.1f', ai_Size.to_f / 1024.0 / 1024.0 / 1024.0 / 1024.0)} TB"
end

def dump(total, used, path)
    ts = bytes_to_str(total * 1024)
    us = bytes_to_str(used * 1024)
    b = print_progress_bar(used * 100.0 / total, 27)
    puts sprintf(" %8s of %8s used (%4.1f%%) %s %s", us, ts, used * 100.0 / total, b, path)
end

`df / /mnt/hackschule`.each_line do |line|
    parts = line.split(/\s+/)
    if parts[2].to_i > 0
        path = parts[5]
        total = parts[1].to_i
        used = parts[2].to_i
        dump(total, used, path)
    end
end

`sudo vdostats`.each_line do |line|
    parts = line.split(/\s+/)
    if parts.first == 'vdo--vg-vpool0-vpool'
        total = parts[1].to_i
        used = parts[2].to_i
        dump(total, used, '(physical vdo)');
    end
end
