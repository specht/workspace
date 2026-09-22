# Tracks browser connections, not arbitrary filesystem writes by code running
# inside a student container. code-server maintains this heartbeat in /workspace
# while a browser is connected (normally once a minute).
module WorkspaceActivity
    IDLE_TIMEOUT = 3 * 60 * 60
    CLOCK_SKEW = 60

    def self.last_connected_at(fs_tag, root: '/user', now: Time.now.to_i)
        workspace = File.join(root, fs_tag, 'workspace')
        paths = [
            File.join(workspace, '.hackschule'), # rewritten on every launch
            File.join(workspace, '.local/share/code-server/heartbeat'),
        ]

        paths.filter_map do |path|
            next if File.symlink?(path)

            modified_at = File.mtime(path).to_i
            # A broken clock or a file deliberately dated far into the future
            # must not keep a container running indefinitely.
            modified_at if modified_at <= now + CLOCK_SKEW
        rescue Errno::ENOENT
            nil # heartbeat may not exist yet (or may disappear during shutdown)
        end.max
    end
end
