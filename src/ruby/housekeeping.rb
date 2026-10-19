#!/usr/bin/env ruby

require 'json'
require 'shellwords'
require_relative 'include/workspace_runtime'
require_relative 'include/workspace_activity'

runtime = WorkspaceRuntime::DirectDocker.new(
    :capture => lambda { |command, **_options| `#{command}` },
    :ok => lambda { |command, **_options| system(command) },
)

LIVE_APP_PIN_PATH = '/internal/live_app_pins.json'
LIVE_APP_PIN_MAX_AGE = 60

def active_live_app_fs_tags
    return [] unless File.exist?(LIVE_APP_PIN_PATH)
    return [] if Time.now.to_i - File.mtime(LIVE_APP_PIN_PATH).to_i > LIVE_APP_PIN_MAX_AGE

    data = JSON.parse(File.read(LIVE_APP_PIN_PATH))
    data['fs_tags'] || []
rescue => e
    STDERR.puts "Housekeeping: Could not read live-app pins: #{e.class}: #{e.message}"
    []
end

running_servers = runtime.running_workspaces.keys

now = Time.now.to_i

live_app_fs_tags = active_live_app_fs_tags
STDERR.puts "Housekeeping: Checking #{running_servers.size} running servers (#{live_app_fs_tags.size} pinned by live apps)..."

running_servers.each do |fs_tag|
    if live_app_fs_tags.include?(fs_tag)
        STDERR.puts "Keeping #{fs_tag} alive because it has an active shared app"
        next
    end

    # Background jobs (e.g. an APK decompiler) may write to /workspace
    # indefinitely without anyone having the editor open. Only code-server's
    # browser-connection heartbeat and the launch grace period count here.
    last_activity = WorkspaceActivity.last_connected_at(fs_tag, :now => now)
    unless last_activity
        # Fail safe for an unexpected/legacy workspace without either marker.
        STDERR.puts "Housekeeping: Keeping #{fs_tag}; no launch marker or browser heartbeat"
        next
    end

    age = now - last_activity
    if age > WorkspaceActivity::IDLE_TIMEOUT
        STDERR.puts "Killing #{fs_tag} => #{age} seconds since browser activity/start"
        runtime.stop_workspace(fs_tag)
    end
end

du_for_fs_tag = {}

Dir["/user/*"].each do |path|
    fs_tag = File.basename(path)
    du = `du -d 0 /user/#{fs_tag}`.split(/\s/).first.to_i
    du_for_fs_tag[fs_tag] = du
end

File.open('/internal/du_for_fs_tag.json', 'w') do |f|
    f.write(du_for_fs_tag.to_json)
end

# Evidence of use for the administration (»Zuletzt da« on /personen,
# Main#activity_by_email): when a browser last had the Workspace open
# (code-server's heartbeat, the launch marker; every hour) and when a file in
# it last changed (once a day: it reads every file's time). A date in the
# future (a broken clock, a file dated on purpose) does not count. Copies
# that lost their times only make someone look more recent, never less.
ACTIVITY_PATH = '/internal/activity_for_fs_tag.json'
previous = (JSON.parse(File.read(ACTIVITY_PATH)) rescue {})
files_due = previous['files_measured_at'].to_i < now - 20 * 3600
activity = {
    'workspace' => {},
    'files' => files_due ? {} : (previous['files'] || {}),
    'files_measured_at' => files_due ? now : previous['files_measured_at'],
    'measured_at' => now,
}
Dir['/user/*'].each do |path|
    fs_tag = File.basename(path)
    next unless fs_tag.match?(/\A[0-9a-z]{16}\z/)

    seen = WorkspaceActivity.last_connected_at(fs_tag, :now => now)
    activity['workspace'][fs_tag] = seen if seen
    next unless files_due

    newest = `find #{Shellwords.escape(path)} -xdev -type f ! -newermt @#{now + 60} -printf '%T@\\n' 2>/dev/null | sort -n | tail -1`.to_f
    activity['files'][fs_tag] = newest.to_i if newest > 0
end
File.write("#{ACTIVITY_PATH}.tmp", activity.to_json)
File.rename("#{ACTIVITY_PATH}.tmp", ACTIVITY_PATH)