#!/usr/bin/env ruby

require 'digest'
require 'json'
require 'yaml'
require_relative 'credentials'

INVITATIONS_DIR = ENV.fetch('HS_INVITATIONS_DIR', '/invitations')
USER_DIR = ENV.fetch('HS_USER_DIR', '/user')
SECTIONS_PATH = ENV.fetch(
    'HS_KEYBOARD_TUTORIAL_SECTIONS',
    '/src/vscode-extensions/keyboard-tutorial/tutorial/sections.yaml',
)

def fs_tag_for_email(email)
    Digest::SHA2.hexdigest(email).to_i(16).to_s(36)[0, 16]
end

def load_users
    invitations = {}
    user_groups = {}
    user_group_order = []

    current_group = 'Administrator'
    ADMIN_USERS.each do |raw_email|
        email = raw_email.to_s.downcase.strip
        next if email.empty?

        user_group_order << current_group unless user_group_order.include?(current_group)
        invitations[email] = { :group => current_group, :name => email }
        user_groups[current_group] ||= []
        user_groups[current_group] << email
    end

    current_group = '(keine Gruppe)'
    Dir[File.join(INVITATIONS_DIR, '*.txt')].sort.each do |path|
        next if File.basename(path) == '_template.txt'

        File.foreach(path) do |line|
            next if line.strip.empty?
            next if line.strip[0] == '#'

            if line[0] == '>'
                current_group = line[1, line.size - 1].strip
                user_groups[current_group] ||= []
                user_group_order << current_group unless user_group_order.include?(current_group)
            elsif line[0] == '+'
                next
            else
                parts = line.strip.split(' ')
                email = parts.last.delete_prefix('<').delete_suffix('>').downcase
                unless invitations[email]
                    user_groups[current_group] ||= []
                    user_groups[current_group] << email
                    user_group_order << current_group unless user_group_order.include?(current_group)
                    invitations[email] = { :group => current_group }
                end
                invitations[email][:name] = if parts.size > 1
                    parts[0, parts.size - 1].join(' ')
                else
                    email
                end
            end
        end
    end

    [invitations, user_groups, user_group_order]
end

def tutorial_step_keys
    sections = YAML.load_file(SECTIONS_PATH).fetch('sections')
    sections.flat_map do |section|
        section.fetch('steps', []).map { |step| step.fetch('key') }
    end
end

def progress_for(email, step_keys)
    state_path = File.join(
        USER_DIR,
        fs_tag_for_email(email),
        'workspace',
        '.hs-kbd-tutorial',
        '.state.json',
    )
    return nil unless File.file?(state_path)

    state = JSON.parse(File.read(state_path))
    solved = step_keys.count { |key| state[key] == true }
    return nil if solved.zero?

    {
        :solved => solved,
        :total => step_keys.size,
        :percent => (100.0 * solved / step_keys.size).round,
    }
rescue JSON::ParserError => e
    warn "Could not read #{state_path}: #{e.message}"
    nil
end

step_keys = tutorial_step_keys
invitations, user_groups, user_group_order = load_users
rows_by_group = {}

user_group_order.each do |group|
    rows = (user_groups[group] || []).filter_map do |email|
        progress = progress_for(email, step_keys)
        next unless progress

        {
            :name => invitations.fetch(email)[:name],
            :email => email,
            :progress => progress,
        }
    end
    rows_by_group[group] = rows unless rows.empty?
end

puts "Tastatur-Tutorial: #{step_keys.size} Schritte"

if rows_by_group.empty?
    puts 'Noch kein Fortschritt gespeichert.'
    exit
end

rows_by_group.each_pair do |group, rows|
    puts
    puts group
    name_width = rows.map { |row| row[:name].length }.max
    rows.each do |row|
        progress = row[:progress]
        puts format(
            "  %-#{name_width}s  %2d / %2d  %3d %%",
            row[:name],
            progress[:solved],
            progress[:total],
            progress[:percent],
        )
    end
end
