require 'minitest/autorun'
require 'tmpdir'
require 'zlib'
require 'rubygems/package'
require_relative '../include/account_export'

class AccountExportTest < Minitest::Test
    NOW = Time.utc(2026, 10, 18, 12, 0, 0)

    def test_keys_that_open_the_workspace_stay_out
        rows = {
            :user => { :email => 'lena@example.org', :name => 'Lena', :brightness => 'dark',
                       :server_sid => 'secret', :share_tag => 'secret', :server_tag => 'secret' },
            :groups => ['LK Informatik'],
            :task_states => [{ :task => 'sql/1', :status => 'correct', :last_answer => 'SELECT 1', :key => 'hash' }],
            :attempts => [{ :task => 'sql/1', :status => 'wrong', :ts => 1 }],
            :look_ins => [{ :ts => 2, :kind => 'workspace', :reason => 'Abgabe', :by_name => 'Frau Berger', :by_email => 'b@example.org' }],
            :live_apps => [{ :port => 3000, :active => true, :tag => 'secret' }],
            :sessions => [{ :sid => 'secret', :created_on => '2026-10-03', :last_used_on => '2026-10-17', :device => 'Firefox auf Windows' }],
        }
        json = AccountExport.account_json(rows, :now => NOW)
        refute_includes json, 'secret'
        refute_includes json, 'b@example.org'
        data = JSON.parse(json)
        assert_equal({ 'email' => 'lena@example.org', 'name' => 'Lena', 'brightness' => 'dark' }, data['konto'])
        assert_equal 'SELECT 1', data['aufgaben'].first['last_answer']
        assert_equal 'Frau Berger', data['einsicht'].first['by_name']
        assert_equal [{ 'created_on' => '2026-10-03', 'last_used_on' => '2026-10-17', 'device' => 'Firefox auf Windows' }], data['anmeldungen']
        assert_equal '2026-10-18T12:00:00Z', data['exportiert']
    end

    # A teacher's feedback on activities is part of what is stored about them.
    def test_feedback_of_a_teacher_is_in_the_export
        rows = { :user => { :email => 'lehrer@example.org' },
                 :feedback => [{ :group => '9b', :activity => 'crypto', :overall => 'good', :languages => ['python'], :contact => true, :created_at => 5 }] }
        data = JSON.parse(AccountExport.account_json(rows, :now => NOW))
        assert_equal [{ 'group' => '9b', 'activity' => 'crypto', 'overall' => 'good', 'languages' => ['python'], 'contact' => true, 'created_at' => 5 }],
            data['rueckmeldungen']
        assert_equal [], JSON.parse(AccountExport.account_json({ :user => {} }, :now => NOW))['rueckmeldungen']
    end

    def test_empty_account
        data = JSON.parse(AccountExport.account_json({}, :now => NOW))
        assert_equal [], data['aufgaben']
        assert_equal({}, data['konto'])
    end

    def test_hand_ins_are_unpacked_and_stay_inside
        Dir.mktmpdir do |dir|
            archive = File.join(dir, 'abgabe.tar.gz')
            File.open(archive, 'wb') do |f|
                Zlib::GzipWriter.wrap(f) do |gz|
                    Gem::Package::TarWriter.new(gz) do |tar|
                        tar.add_file_simple('index.html', 0o644, 5) { |io| io.write('hallo') }
                        tar.add_file_simple('css/style.css', 0o644, 4) { |io| io.write('body') }
                        tar.add_file_simple('../../outside.txt', 0o644, 4) { |io| io.write('evil') }
                    end
                end
            end
            target = File.join(dir, 'out', AccountExport.folder_for('website/abgabe-1'))
            assert_equal 2, AccountExport.unpack_submission(archive, target)
            assert_equal 'hallo', File.read(File.join(dir, 'out', 'website-abgabe-1', 'index.html'))
            assert File.file?(File.join(target, 'css', 'style.css'))
            refute File.exist?(File.join(dir, 'outside.txt'))
            refute File.exist?(File.join(dir, 'out', 'outside.txt'))
        end
    end

    def test_filename
        assert_equal 'workspace-lena.mueller-2026-10-18.zip', AccountExport.filename('lena.mueller@example.org', :now => NOW)
        assert_equal 'workspace-xrm-2026-10-18.zip', AccountExport.filename("x\"; rm@example.org", :now => NOW)
    end
end
