require 'minitest/autorun'
require 'uri'
require_relative '../include/docker_policy'

class DockerPolicyTest < Minitest::Test
    CONFIG = DockerPolicy::Config.new(
        :images => ['hs_code_server'], :bind_prefix => '/srv/data/user/', :networks => %w[workspace_user none],
        :exec_containers => %w[workspace_mysql_1 workspace_neo4j_1], :hup_containers => ['workspace_nginx_1'], :name_prefix => 'hs_code_'
    )

    # What workspace_runtime.rb sends for a Workspace (docker run -d …).
    def workspace(overrides = {}, host_overrides = {})
        {
            'Image' => 'hs_code_server',
            'HostConfig' => {
                'Binds' => ['/srv/data/user/abc/config:/config', '/srv/data/user/abc/workspace:/workspace'],
                'NetworkMode' => 'workspace_user', 'AutoRemove' => true, 'NanoCpus' => 4_000_000_000, 'Memory' => 4 * 1024**3,
                'MemorySwap' => 4 * 1024**3, 'PidsLimit' => 256,
            }.merge(host_overrides),
        }.merge(overrides).to_json
    end

    def create(body, name: 'hs_code_abc')
        DockerPolicy.decide('POST', '/v1.47/containers/create', "name=#{name}", body, CONFIG)
    end

    def test_a_workspace_and_the_sandbox_may_start
        assert_equal [true], create(workspace)
        exam = workspace({}, 'Binds' => ['/srv/data/user/abc/workspace:/workspace', '/dev/null:/etc/resolv.conf:ro'])
        assert_equal [true], create(exam)
        hardened = workspace({}, 'CapDrop' => %w[NET_RAW MKNOD AUDIT_WRITE SYS_CHROOT SETFCAP SETPCAP NET_BIND_SERVICE FSETID],
            'SecurityOpt' => ['no-new-privileges'])
        assert_equal [true], create(hardened)
        sandbox = workspace({}, 'Binds' => ['/srv/data/user/abc/workspace:/workspace:ro'], 'NetworkMode' => 'none',
            'CapDrop' => ['ALL'], 'SecurityOpt' => ['no-new-privileges'], 'ReadonlyRootfs' => true,
            'NanoCpus' => 1_000_000_000, 'Memory' => 256 * 1024**2, 'MemorySwap' => 256 * 1024**2, 'PidsLimit' => 64)
        assert_equal [true], create(sandbox, :name => 'hs_code_offline_1a2b')
    end

    def test_what_would_own_the_host_is_refused
        refute create(workspace('Image' => 'alpine')).first
        refute create(workspace, :name => 'evil').first
        refute create(workspace({}, 'Privileged' => true)).first
        refute create(workspace({}, 'CapAdd' => ['SYS_ADMIN'])).first
        refute create(workspace({}, 'Binds' => ['/:/host'])).first
        refute create(workspace({}, 'Binds' => ['/var/run/docker.sock:/var/run/docker.sock'])).first
        refute create(workspace({}, 'Binds' => ['/srv/data/user/../../etc:/x'])).first
        refute create(workspace({}, 'Binds' => ['/srv/data/userx/a:/x'])).first
        refute create(workspace({}, 'Mounts' => [{ 'Type' => 'bind', 'Source' => '/etc', 'Target' => '/x' }])).first
        refute create(workspace({}, 'Mounts' => [{ 'Type' => 'volume', 'Source' => 'workspace_mysql', 'Target' => '/x' }])).first
        refute create(workspace({}, 'NetworkMode' => 'host')).first
        refute create(workspace({}, 'NetworkMode' => 'workspace_internal')).first
        refute create(workspace({}, 'PidMode' => 'host')).first
        refute create(workspace({}, 'SecurityOpt' => ['seccomp=unconfined'])).first
        refute create(workspace({}, 'Devices' => [{ 'PathOnHost' => '/dev/sda' }])).first
        refute create(workspace({}, 'PortBindings' => { '22/tcp' => [{ 'HostPort' => '2222' }] })).first
    end

    # A backend taken over must not start containers that eat the server
    def test_every_container_has_limits_no_larger_than_a_workspace
        refute create(workspace({}, 'NanoCpus' => nil)).first
        refute create(workspace({}, 'NanoCpus' => 8_000_000_000)).first
        refute create(workspace({}, 'Memory' => nil, 'MemorySwap' => nil)).first
        refute create(workspace({}, 'Memory' => 16 * 1024**3, 'MemorySwap' => 16 * 1024**3)).first
        refute create(workspace({}, 'MemorySwap' => -1)).first
        refute create(workspace({}, 'MemorySwap' => 8 * 1024**3)).first
        refute create(workspace({}, 'PidsLimit' => nil)).first
        refute create(workspace({}, 'PidsLimit' => -1)).first
        refute create(workspace({}, 'PidsLimit' => 100_000)).first
        roomy = CONFIG.dup.tap { |c| c.max_pids = 512 }
        assert_equal [true], DockerPolicy.decide('POST', '/containers/create', 'name=hs_code_a', workspace({}, 'PidsLimit' => 512), roomy)
    end

    def test_exec_only_into_workspaces_and_the_databases_never_privileged
        assert_equal [true], DockerPolicy.decide('POST', '/v1.47/containers/abc123/exec', '', '{"Cmd":["mysql"]}', CONFIG, :name => 'workspace_mysql_1')
        assert_equal [true], DockerPolicy.decide('POST', '/containers/x/exec', '', '{"User":"1000:1000"}', CONFIG, :name => 'hs_code_abc')
        refute DockerPolicy.decide('POST', '/containers/x/exec', '', '{}', CONFIG, :name => 'workspace_ruby_1').first
        refute DockerPolicy.decide('POST', '/containers/x/exec', '', '{"Privileged":true}', CONFIG, :name => 'hs_code_abc').first
        assert_equal [true], DockerPolicy.decide('POST', '/v1.47/exec/123abc/start', '', '', CONFIG)
    end

    def test_kill_workspaces_and_reload_nginx_only
        assert_equal [true], DockerPolicy.decide('POST', '/containers/x/kill', '', '', CONFIG, :name => 'hs_code_abc')
        assert_equal [true], DockerPolicy.decide('POST', '/containers/x/kill', 'signal=HUP', '', CONFIG, :name => 'workspace_nginx_1')
        refute DockerPolicy.decide('POST', '/containers/x/kill', 'signal=KILL', '', CONFIG, :name => 'workspace_nginx_1').first
        refute DockerPolicy.decide('POST', '/containers/x/kill', '', '', CONFIG, :name => 'workspace_mysql_1').first
        refute DockerPolicy.decide('DELETE', '/containers/x', '', '', CONFIG, :name => 'workspace_mysql_1').first
    end

    def test_reading_is_allowed_everything_unlisted_is_not
        %w[/_ping /v1.47/version /v1.47/containers/json /v1.47/networks/workspace_user /v1.47/images/hs_code_server/json /v1.47/events /v1.47/volumes/x].each do |path|
            assert_equal [true], DockerPolicy.decide('GET', path, '', '', CONFIG), path
        end
        assert_equal [true], DockerPolicy.decide('GET', '/v1.47/containers/abc/stats', 'stream=0', '', CONFIG, :name => 'workspace_mysql_1')
        refute DockerPolicy.decide('POST', '/v1.47/images/create', 'fromImage=alpine', '', CONFIG).first
        refute DockerPolicy.decide('POST', '/v1.47/networks/create', '', '{}', CONFIG).first
        refute DockerPolicy.decide('POST', '/v1.47/volumes/create', '', '{}', CONFIG).first
        refute DockerPolicy.decide('POST', '/v1.47/build', '', '', CONFIG).first
        refute DockerPolicy.decide('POST', '/v1.47/containers/x/update', '', '{}', CONFIG, :name => 'hs_code_abc').first
        refute DockerPolicy.decide('POST', '/v1.47/swarm/init', '', '{}', CONFIG).first
    end

    # The guard of those who only watch (the peer firewall, Traefik): what
    # they read passes, everything else is refused, also what the backend's
    # guard allows
    READONLY = DockerPolicy::Config.new(:images => [], :bind_prefix => '', :networks => [], :exec_containers => [],
        :hup_containers => [], :name_prefix => 'hs_code_', :readonly => true)

    def test_a_watcher_reads
        ['/_ping', '/version', '/v1.47/version', '/v1.47/info', '/v1.47/events', '/v1.47/containers/json',
         '/v1.47/containers/abc123/json', '/v1.47/networks', '/v1.47/networks/workspace_user'].each do |path|
            assert_equal [true], DockerPolicy.decide('GET', path, '', '', READONLY), path
        end
        assert_equal [true], DockerPolicy.decide('HEAD', '/_ping', '', '', READONLY)
    end

    def test_a_watcher_changes_nothing
        [['POST', '/v1.47/containers/create'], ['POST', '/v1.47/containers/hs_code_abc/start'],
         ['POST', '/v1.47/containers/workspace_mysql_1/exec'], ['POST', '/v1.47/exec/1/start'],
         ['DELETE', '/v1.47/containers/hs_code_abc'], ['POST', '/v1.47/containers/x/kill'],
         ['GET', '/v1.47/containers/x/logs'], ['GET', '/v1.47/containers/x/stats'], ['GET', '/v1.47/containers/x/archive'],
         ['GET', '/v1.47/images/json'], ['POST', '/v1.47/images/create'], ['GET', '/v1.47/volumes'],
         ['POST', '/v1.47/networks/create'], ['POST', '/v1.47/networks/x/connect'], ['GET', '/v1.47/secrets'],
         ['POST', '/v1.47/build'], ['GET', '/v1.47/containers/x/export']].each do |method, path|
            allowed, reason = DockerPolicy.decide(method, path, '', '{}', READONLY)
            refute allowed, "#{method} #{path}"
            assert_match(/not needed to watch/, reason)
        end
    end

    def test_the_mode_comes_from_the_environment
        assert DockerPolicy.config_from_env('GUARD_MODE' => 'readonly').readonly
        refute DockerPolicy.config_from_env({}).readonly
    end
end
