require 'minitest/autorun'
require 'fileutils'
require 'tmpdir'
require_relative '../include/workspace_activity'

class WorkspaceActivityTest < Minitest::Test
    TAG = 'abcdefghijklmnop'

    def setup
        @now = Time.now.to_i
        @tmpdir = Dir.mktmpdir('workspace-activity-')
        @workspace = File.join(@tmpdir, TAG, 'workspace')
        FileUtils.mkdir_p(@workspace)
    end

    def teardown
        FileUtils.remove_entry(@tmpdir)
    end

    def touch(relative_path, age:)
        path = File.join(@workspace, relative_path)
        FileUtils.mkdir_p(File.dirname(path))
        File.write(path, '')
        at = Time.at(@now - age)
        File.utime(at, at, path)
        path
    end

    def last_activity
        WorkspaceActivity.last_connected_at(TAG, :root => @tmpdir, :now => @now)
    end

    def test_fresh_launch_without_heartbeat_has_three_hour_grace
        touch('.hackschule', :age => 60)
        assert_equal @now - 60, last_activity
        assert_operator @now - last_activity, :<, WorkspaceActivity::IDLE_TIMEOUT
    end

    def test_recent_browser_heartbeat_takes_precedence_over_old_launch
        touch('.hackschule', :age => 7 * 24 * 3600)
        touch('.local/share/code-server/heartbeat', :age => 45)
        assert_equal @now - 45, last_activity
    end

    def test_background_file_writes_do_not_reset_browser_idle_time
        touch('.hackschule', :age => 4 * 3600)
        touch('.local/share/code-server/heartbeat', :age => 4 * 3600)
        touch('ct/re/JEB-5.42/.jebc_ts', :age => 0)
        touch('.cache/opencode/models.json', :age => 0)
        assert_operator @now - last_activity, :>, WorkspaceActivity::IDLE_TIMEOUT
    end

    def test_old_heartbeat_does_not_cancel_recent_launch_grace
        touch('.hackschule', :age => 120)
        touch('.local/share/code-server/heartbeat', :age => 6 * 3600)
        assert_equal @now - 120, last_activity
    end

    def test_missing_markers_fail_safe_instead_of_crashing_or_killing
        assert_nil last_activity
    end

    def test_future_dated_heartbeat_cannot_keep_container_alive
        touch('.hackschule', :age => 4 * 3600)
        touch('.local/share/code-server/heartbeat', :age => -24 * 3600)
        assert_equal @now - 4 * 3600, last_activity
    end

    def test_ignores_symlinked_heartbeat
        touch('.hackschule', :age => 4 * 3600)
        target = touch('arbitrary-activity', :age => 0)
        heartbeat = File.join(@workspace, '.local/share/code-server/heartbeat')
        FileUtils.mkdir_p(File.dirname(heartbeat))
        File.symlink(target, heartbeat)
        assert_equal @now - 4 * 3600, last_activity
    end
end
