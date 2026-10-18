require 'minitest/autorun'
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
        }
        json = AccountExport.account_json(rows, :now => NOW)
        refute_includes json, 'secret'
        refute_includes json, 'b@example.org'
        data = JSON.parse(json)
        assert_equal({ 'email' => 'lena@example.org', 'name' => 'Lena', 'brightness' => 'dark' }, data['konto'])
        assert_equal 'SELECT 1', data['aufgaben'].first['last_answer']
        assert_equal 'Frau Berger', data['einsicht'].first['by_name']
        assert_equal '2026-10-18T12:00:00Z', data['exportiert']
    end

    def test_empty_account
        data = JSON.parse(AccountExport.account_json({}, :now => NOW))
        assert_equal [], data['aufgaben']
        assert_equal({}, data['konto'])
    end

    def test_filename
        assert_equal 'workspace-lena.mueller-2026-10-18.tar.gz', AccountExport.filename('lena.mueller@example.org', :now => NOW)
        assert_equal 'workspace-xrm-2026-10-18.tar.gz', AccountExport.filename("x\"; rm@example.org", :now => NOW)
    end
end
