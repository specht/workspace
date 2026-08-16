require 'json'
require 'shellwords'

# Semantic boundary for operations on student Workspace containers.
# DirectDocker intentionally preserves the existing Docker CLI behavior; a
# different implementation can be tested behind the same interface later.
module WorkspaceRuntime
    class DirectDocker
        WORKSPACE_NETWORK = 'workspace_user'
        WORKSPACE_IMAGE = 'hs_code_server'

        def initialize(capture:, ok:, each_line: nil)
            @capture = capture
            @ok = ok
            @each_line = each_line || lambda do |command, &block|
                IO.popen(command, &block)
            end
        end

        def running_workspaces(timeout: nil)
            inspect = JSON.parse(@capture.call(
                "docker network inspect #{WORKSPACE_NETWORK}",
                :timeout => timeout,
            ))
            result = {}
            inspect.first['Containers'].values.each do |container|
                name = container['Name'].to_s
                next unless name.start_with?('hs_code_')

                result[name.sub('hs_code_', '')] = {
                    :ip => container['IPv4Address'].to_s.split('/').first,
                }
            end
            result
        end

        def workspace_state(fs_tag, timeout: nil)
            result = {
                :tag => fs_tag,
                :running => false,
            }
            inspect_json = @capture.call(
                "docker inspect hs_code_#{fs_tag}",
                :timeout => timeout,
                :allow_failure => true,
            )
            inspect = inspect_json.strip.empty? ? [] : JSON.parse(inspect_json)
            unless inspect.empty?
                result[:running] = true
                result[:ip] = inspect.first['NetworkSettings']['Networks'][WORKSPACE_NETWORK]['IPAddress']
            end
            result
        end

        def start_workspace(fs_tag:, workspace_login:, database_environment:,
            host_data_path:, test_mode:, timeout: nil)
            command = "docker run --cpus=4 --memory=4g --memory-swap=4g --pids-limit=256 -d --rm --hostname workspace -e PUID=1000 -e PGID=1000 -e TZ=Europe/Berlin -e WORKSPACE_USER=#{Shellwords.escape(workspace_login)} -e PWA_APPNAME=\"Workspace\" -e DEFAULT_WORKSPACE=/workspace -e MYSQL_HOST=\"#{database_environment['MYSQL_HOST']}\" -e MYSQL_USER=\"#{database_environment['MYSQL_USER']}\" -e MYSQL_PASSWORD=\"#{database_environment['MYSQL_PASSWORD']}\" -e MYSQL_DATABASE=\"#{database_environment['MYSQL_DATABASE']}\" -e NEO4J_URI=\"#{database_environment['NEO4J_URI']}\" -e NEO4J_USERNAME=\"#{database_environment['NEO4J_USERNAME']}\" -e NEO4J_PASSWORD=\"#{database_environment['NEO4J_PASSWORD']}\" -e NEO4J_DATABASE=\"#{database_environment['NEO4J_DATABASE']}\" -v #{host_data_path}/user/#{fs_tag}/config:/config -v #{host_data_path}/user/#{fs_tag}/workspace:/workspace --network #{WORKSPACE_NETWORK} #{test_mode ? '-v /dev/null:/etc/resolv.conf:ro' : ''} --name hs_code_#{fs_tag} #{WORKSPACE_IMAGE}"
            @ok.call(command, :timeout => timeout)
        end

        def stop_workspace(fs_tag, timeout: nil)
            @ok.call(
                "docker kill hs_code_#{fs_tag}",
                :timeout => timeout,
                :allow_failure => true,
            )
        end

        def workspace_network_info(timeout: nil)
            inspect_json = @capture.call(
                'docker inspect workspace',
                :timeout => timeout,
                :allow_failure => true,
            )
            result = {}
            unless inspect_json.strip.empty?
                JSON.parse(inspect_json).each do |entry|
                    entry['Containers'].each_value do |container|
                        name = container['Name'].to_s
                        next unless name.start_with?('hs_code_')

                        result[name.sub('hs_code_', '')] = {
                            :ip => container['IPv4Address'],
                        }
                    end
                end
            end
            result
        end

        def workspace_stats
            result = {}
            command = "docker stats --no-stream --format \"{{ json . }}\""
            @each_line.call(command) do |io|
                io.each_line do |line|
                    line.strip!
                    next if line.empty?

                    begin
                        stat_line = JSON.parse(line)
                    rescue JSON::ParserError => e
                        STDERR.puts "Could not parse docker stats line: #{line.inspect}: #{e.message}"
                        next
                    end

                    name = stat_line['Name'].to_s
                    next unless name.start_with?('hs_code_')

                    result[name.sub('hs_code_', '')] = stat_line
                end
            end
            result
        end

        def live_app_sockets(fs_tag, uid:, timeout: nil)
            output = @capture.call(
                "docker exec hs_code_#{fs_tag} sh -c 'cat /proc/net/tcp /proc/net/tcp6 2>/dev/null'",
                :timeout => timeout,
                :allow_failure => true,
                :log_command => false,
            )
            sockets = []
            output.each_line do |line|
                parts = line.strip.split(/\s+/)
                next unless parts.size >= 10
                next unless parts[3] == '0A'
                next unless parts[7].to_i == uid

                local_address = parts[1]
                next unless local_address && local_address.include?(':')

                sockets << {
                    :port => local_address.split(':').last.to_i(16),
                    :inode => parts[9],
                }
            end
            sockets
        end

        def live_app_processes(fs_tag, uid:, timeout: nil)
            script = <<~'SH'
                for p in /proc/[0-9]*; do
                    pid=${p#/proc/}
                    [ -r "$p/comm" ] || continue
                    comm=$(tr '\t\n' '  ' < "$p/comm" 2>/dev/null)
                    cmd=$(tr '\000' ' ' < "$p/cmdline" 2>/dev/null | tr '\t\n' '  ')
                    for fd in "$p"/fd/*; do
                        link=$(readlink "$fd" 2>/dev/null) || continue
                        case "$link" in
                            socket:\[*\])
                                inode=${link#socket:\[}
                                inode=${inode%\]}
                                printf '%s\t%s\t%s\t%s\n' "$inode" "$pid" "$comm" "$cmd"
                                ;;
                        esac
                    done
                done
            SH
            command = "docker exec -u #{uid} hs_code_#{fs_tag} sh -c #{Shellwords.escape(script)}"
            output = @capture.call(
                command,
                :timeout => timeout,
                :allow_failure => true,
                :log_command => false,
            )
            processes = {}
            output.each_line do |line|
                inode, pid, process, command = line.chomp.split("\t", 4)
                next if inode.to_s.empty?

                processes[inode] ||= {
                    :pid => pid.to_i,
                    :process => process.to_s.strip,
                    :command => command.to_s.strip,
                }
            end
            processes
        end
    end
end
