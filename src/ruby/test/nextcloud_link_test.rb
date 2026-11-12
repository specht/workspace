require 'minitest/autorun'
require 'tmpdir'
require_relative '../include/nextcloud_link'
require_relative '../include/submission_store'

# NextcloudLink without a network: which addresses a teacher may name, which
# links of students count, which addresses are never fetched from, the name
# of a downloaded file; and the link kept beside the document.
class NextcloudLinkTest < Minitest::Test
    def refused(&block)
        assert_raises(NextcloudLink::Refused, &block).message
    end

    def test_server_from_any_page_of_it
        assert_equal 'https://cloud.schule.de', NextcloudLink.server('cloud.schule.de')
        assert_equal 'https://cloud.schule.de', NextcloudLink.server(' https://Cloud.Schule.de/apps/files/?dir=/ ')
        assert_equal 'https://schule.de/nextcloud', NextcloudLink.server('https://schule.de/nextcloud/index.php/apps/dashboard/')
        assert_equal 'https://schule.de/nextcloud', NextcloudLink.server('https://schule.de/nextcloud/login')
    end

    def test_server_refuses
        assert_match(/https/, refused { NextcloudLink.server('http://cloud.schule.de') })
        assert_match(/443/, refused { NextcloudLink.server('https://cloud.schule.de:8443') })
        assert_match(/IP/, refused { NextcloudLink.server('https://10.0.0.5') })
        assert_match(/IP/, refused { NextcloudLink.server('https://[::1]') })
        assert_match(/Passwort/, refused { NextcloudLink.server('https://a:b@cloud.schule.de') })
        assert_match(/vollständig/, refused { NextcloudLink.server('https://localhost') })
    end

    def test_token
        server = 'https://cloud.schule.de'
        assert_equal 'Ab12Cd34Ef56Gh7', NextcloudLink.token(server, 'https://cloud.schule.de/s/Ab12Cd34Ef56Gh7')
        assert_equal 'Ab12Cd34Ef56Gh7', NextcloudLink.token(server, 'cloud.schule.de/index.php/s/Ab12Cd34Ef56Gh7/download')
        assert_equal 'Ab12Cd34Ef56Gh7', NextcloudLink.token('https://schule.de/nc', 'https://schule.de/nc/s/Ab12Cd34Ef56Gh7')
        assert_match(/eurer Nextcloud/, refused { NextcloudLink.token(server, 'https://andere.de/s/Ab12Cd34Ef56Gh7') })
        assert_match(/eurer Nextcloud/, refused { NextcloudLink.token(server, 'http://cloud.schule.de/s/Ab12Cd34Ef56Gh7') })
        assert_match(/Teilen/, refused { NextcloudLink.token(server, 'https://cloud.schule.de/f/123') })
        assert_match(/Teilen/, refused { NextcloudLink.token(server, 'https://cloud.schule.de/s/../status.php') })
        assert_match(/Teilen/, refused { NextcloudLink.token('https://schule.de/nc', 'https://schule.de/s/Ab12Cd34Ef56Gh7') })
        assert_match(/einfügen|ein:/, refused { NextcloudLink.token(server, '  ') })
    end

    def test_only_public_addresses
        %w[10.1.2.3 172.16.0.1 192.168.0.1 127.0.0.1 169.254.169.254 100.64.0.1 0.0.0.0 224.0.0.1
           ::1 fd12::1 fe80::1 ::ffff:192.168.0.1 ::ffff:127.0.0.1].each do |ip|
            refute NextcloudLink.public_ip?(ip), ip
        end
        %w[8.8.8.8 141.35.1.1 2a00:1450:4001::1].each { |ip| assert NextcloudLink.public_ip?(ip), ip }
    end

    def test_filename
        assert_equal 'Einladung Elternabend.docx',
            NextcloudLink.filename("attachment; filename*=UTF-8''Einladung%20Elternabend.docx; filename=\"Einladung Elternabend.docx\"")
        assert_equal 'Plan.odt', NextcloudLink.filename('attachment; filename="../../Plan.odt"')
        assert_equal 'Büro.ods', NextcloudLink.filename("attachment; filename*=UTF-8''B%C3%BCro.ods")
        assert_nil NextcloudLink.filename('inline')
    end

    def test_link_kept_beside_the_document
        Dir.mktmpdir do |dir|
            target = File.join(dir, 'x.tar.gz')
            SubmissionStore.write_text(target, 'Einladung.docx', 'PK-bytes', :extra => { NextcloudLink::LINK_FILE => 'https://cloud.schule.de/s/Ab12Cd34Ef56Gh7' })
            assert_equal ['Einladung.docx', 'PK-bytes'], SubmissionStore.read_file(target)
            assert_equal 'https://cloud.schule.de/s/Ab12Cd34Ef56Gh7', SubmissionStore.read_entry(target, NextcloudLink::LINK_FILE)
            SubmissionStore.write_text(target, 'Einladung.docx', 'PK-new')
            assert_nil SubmissionStore.read_entry(target, NextcloudLink::LINK_FILE)
        end
    end
end
