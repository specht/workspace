require 'json'
require 'uri'

# What the backend may ask Docker for (docker/docker-guard/guard.rb applies
# it). The backend holds no Docker socket of its own: whoever took over the
# Ruby process could otherwise start a privileged container and own the
# host. Through the guard it can do what the Workspace needs and nothing
# more:
#
# - start containers named hs_code_* from the Workspace image, with folders
#   only under the user data, no privileges, no host modes, no extra
#   capabilities, no published ports, and limits no larger than a
#   Workspace's (CPUs, memory without swap, processes), so that it cannot
#   start containers that eat the server either;
# - exec into hs_code_* and the database containers, never privileged;
# - read (ps, inspect, stats, network inspect, image inspect), kill and wait
#   for hs_code_*, and HUP to nginx (reload).
#
# Plain Ruby, tested on its own (test/docker_policy_test.rb). Every rule
# that is not listed is a refusal.
module DockerPolicy
    # images: the Workspace image(s); bind_prefix: where Workspace folders
    # live on the host (PATH_TO_HOST_DATA/user/); networks: what a Workspace
    # may join; exec_containers: the database containers; hup_containers:
    # those that may get a HUP (nginx).
    # max_cpus, max_memory (bytes), max_pids: the largest limits a container
    # may ask for; nil means a Workspace's (workspace_runtime.rb).
    Config = Struct.new(:images, :bind_prefix, :networks, :exec_containers, :hup_containers, :name_prefix,
        :max_cpus, :max_memory, :max_pids, keyword_init: true)
    MAX_CPUS = 4
    MAX_MEMORY = 4 * 1024**3
    MAX_PIDS = 256

    def self.config_from_env(env = ENV)
        list = ->(key) { env[key].to_s.split(',').map(&:strip).reject(&:empty?) }
        Config.new(
            :images => list.call('GUARD_IMAGES'),
            :bind_prefix => env['GUARD_BIND_PREFIX'].to_s,
            :networks => list.call('GUARD_NETWORKS'),
            :exec_containers => list.call('GUARD_EXEC_CONTAINERS'),
            :hup_containers => list.call('GUARD_HUP_CONTAINERS'),
            :name_prefix => env['GUARD_NAME_PREFIX'].to_s.empty? ? 'hs_code_' : env['GUARD_NAME_PREFIX'],
            :max_cpus => env['GUARD_MAX_CPUS'].to_s.empty? ? nil : env['GUARD_MAX_CPUS'].to_f,
            :max_memory => env['GUARD_MAX_MEMORY'].to_s.empty? ? nil : env['GUARD_MAX_MEMORY'].to_i,
            :max_pids => env['GUARD_MAX_PIDS'].to_s.empty? ? nil : env['GUARD_MAX_PIDS'].to_i
        )
    end

    # The path without the API version: /v1.47/containers/json -> /containers/json
    def self.plain_path(path)
        path.to_s.split('?').first.to_s.sub(%r{\A/v\d+(\.\d+)?(?=/)}, '')
    end

    # What a request is about: [:kind, container or nil]. container is a name
    # or ID the guard has to resolve before deciding (container_name).
    def self.classify(method, path)
        p = plain_path(path)
        # /events: docker stats follows it to notice new containers; /volumes:
        # docker inspect tries every kind of object for a name it cannot find.
        return [:read, nil] if %w[GET HEAD].include?(method) && (p == '/_ping' || p == '/version' || p == '/info' || p == '/events' ||
            p == '/containers/json' || p == '/networks' || p.match?(%r{\A/networks/[^/]+\z}) || p == '/volumes' || p.match?(%r{\A/volumes/[^/]+\z}) ||
            p.match?(%r{\A/(plugins|secrets|configs|services|nodes|tasks)/[^/]+(/json)?\z}) ||
            p.match?(%r{\A/images/.+/json\z}) || p.match?(%r{\A/exec/[^/]+/json\z}))
        return [:exec_session, nil] if method == 'POST' && p.match?(%r{\A/exec/[^/]+/(start|resize)\z})
        return [:create, nil] if method == 'POST' && p == '/containers/create'
        if (m = p.match(%r{\A/containers/([^/]+)(/[a-z]+)?\z}))
            action = (m[2] || '').delete_prefix('/')
            kind = case [method, action]
                when ['GET', 'json'], ['GET', 'stats'], ['GET', 'top'] then :container_read
                when ['POST', 'start'], ['POST', 'wait'], ['POST', 'attach'], ['POST', 'resize'], ['POST', 'stop'] then :container_own
                when ['POST', 'kill'] then :kill
                when ['POST', 'exec'] then :exec_create
                when ['DELETE', ''] then :container_own
            end
            return [kind, m[1]] if kind
        end
        [:unknown, nil]
    end

    # Decides; name: the resolved container name (without the leading /) for
    # kinds that need one. Returns [true] or [false, reason].
    def self.decide(method, path, query, body, config, name: nil)
        kind, = classify(method, path)
        case kind
        when :read, :exec_session, :container_read
            [true]
        when :create
            create_allowed?(query, body, config)
        when :container_own
            own?(name, config) ? [true] : [false, "not a Workspace container: #{name}"]
        when :kill
            signal = query.to_s[/(?:\A|&)signal=([^&]*)/, 1].to_s.upcase
            return [true] if own?(name, config)
            return [true] if config.hup_containers.include?(name) && %w[HUP SIGHUP 1].include?(signal)

            [false, "kill not allowed for #{name} (#{signal.empty? ? 'KILL' : signal})"]
        when :exec_create
            return [false, "exec not allowed in #{name}"] unless own?(name, config) || config.exec_containers.include?(name)
            return [false, 'privileged exec'] if json(body)['Privileged'] == true

            [true]
        else
            [false, "#{method} #{plain_path(path)} is not needed by the Workspace"]
        end
    end

    def self.own?(name, config)
        !name.to_s.empty? && name.to_s.start_with?(config.name_prefix)
    end

    def self.json(body)
        body.to_s.empty? ? {} : JSON.parse(body)
    rescue JSON::ParserError
        {}
    end

    UNSAFE_SECURITY = /unconfined|label[:=]disable|systempaths/i

    def self.create_allowed?(query, body, config)
        name = query.to_s[/(?:\A|&)name=([^&]*)/, 1].to_s
        name = URI.decode_www_form_component(name) rescue name
        return [false, "container name must start with #{config.name_prefix}: #{name}"] unless own?(name, config)

        spec = json(body)
        return [false, "image not allowed: #{spec['Image']}"] unless config.images.include?(spec['Image'].to_s)

        host = spec['HostConfig'] || {}
        return [false, 'privileged'] if host['Privileged'] == true
        return [false, "capabilities added: #{host['CapAdd']}"] unless Array(host['CapAdd']).empty?
        return [false, 'devices'] unless Array(host['Devices']).empty? && Array(host['DeviceRequests']).empty?
        %w[PidMode IpcMode UTSMode UsernsMode CgroupnsMode].each do |mode|
            return [false, "#{mode}=host"] if host[mode].to_s == 'host'
        end
        return [false, "network not allowed: #{host['NetworkMode']}"] unless config.networks.include?(host['NetworkMode'].to_s)
        return [false, 'security options'] if Array(host['SecurityOpt']).any? { |o| o.to_s.match?(UNSAFE_SECURITY) }
        return [false, 'published ports'] unless (host['PortBindings'] || {}).empty? && host['PublishAllPorts'] != true
        return [false, 'volumes from other containers'] unless Array(host['VolumesFrom']).empty?
        return [false, 'sysctls'] unless (host['Sysctls'] || {}).empty?
        return [false, 'cgroup parent'] unless host['CgroupParent'].to_s.empty?
        return [false, "runtime #{host['Runtime']}"] unless ['', 'runc'].include?(host['Runtime'].to_s)
        problem = limits_problem(host, config)
        return [false, problem] if problem

        sources = Array(host['Binds']).map { |b| b.to_s.split(':').first.to_s } +
            Array(host['Mounts']).map do |m|
                return [false, "mount type #{m['Type']}"] unless %w[bind tmpfs].include?(m['Type'].to_s)

                m['Type'] == 'bind' ? m['Source'].to_s : nil
            end.compact
        bad = sources.reject { |source| bind_allowed?(source, config) }
        return [false, "folders not allowed: #{bad.join(', ')}"] unless bad.empty?

        [true]
    end

    # Every limit set, none larger than a Workspace's; memory without swap
    # (MemorySwap, memory plus swap, equal to Memory). nil when all is well.
    def self.limits_problem(host, config)
        max_cpus = config.max_cpus || MAX_CPUS
        max_memory = config.max_memory || MAX_MEMORY
        max_pids = config.max_pids || MAX_PIDS
        cpus = host['NanoCpus'].to_i
        return "CPU limit missing or above #{max_cpus}" unless cpus > 0 && cpus <= max_cpus * 1_000_000_000
        memory = host['Memory'].to_i
        return "memory limit missing or above #{max_memory} bytes" unless memory > 0 && memory <= max_memory
        return 'swap' unless host['MemorySwap'].to_i == memory
        pids = host['PidsLimit'].to_i
        return "process limit missing or above #{max_pids}" unless pids > 0 && pids <= max_pids

        nil
    end

    # Under the user data, or /dev/null (an exam Workspace has no DNS).
    def self.bind_allowed?(source, config)
        return true if source == '/dev/null'
        return false if config.bind_prefix.empty? || source.include?('..') || !source.start_with?('/')

        source.start_with?(config.bind_prefix.end_with?('/') ? config.bind_prefix : "#{config.bind_prefix}/")
    end
end
