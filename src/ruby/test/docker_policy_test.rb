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
            'CapDrop' => ['ALL'], 'SecurityOpt' => ['no-new-privileges'], 'ReadonlyRootfs' => true)
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
end
