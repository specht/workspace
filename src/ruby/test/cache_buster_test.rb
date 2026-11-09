require 'minitest/autorun'

# Every address of a file under /include that the server writes into a page
# carries the cache buster in both parts: CACHE_BUSTER, fixed while the
# server runs (a new version in production gets a new address), and in
# development the time of the request, so an edited script or stylesheet
# arrives at once instead of the one the browser kept (nginx lets browsers
# keep /include/* forever):
#
#     /include/x.js?#{CACHE_BUSTER}#{DEVELOPMENT ? Time.now.to_i : nil}
#
# Scripts in the Markdown of a page get it from content_pipeline.rb.
class CacheBusterTest < Minitest::Test
    SRC = File.expand_path('../..', __dir__)
    # written once and compared on each refresh: a time in it would make the
    # configuration look changed at every refresh in development; it gets a
    # new address with each start of the server
    EXCEPTIONS = {
        'ruby/include/nginx_config.rb' => 'the code-server pages',
    }.freeze

    def test_every_cache_buster_is_fresh_in_development
        missing = []
        Dir.glob(File.join(SRC, '{ruby,static}', '**', '*.{rb,html}')).sort.each do |path|
            relative = path.delete_prefix("#{SRC}/")
            next if relative.start_with?('ruby/test/') || EXCEPTIONS.key?(relative)

            File.foreach(path).with_index(1) do |line, number|
                next unless line.include?('CACHE_BUSTER')
                next if line =~ /\bCACHE_BUSTER\s*=/            # where it is made
                next if line.include?('Time.now')

                missing << "#{relative}:#{number}: #{line.strip[0, 120]}"
            end
        end
        assert_empty missing, "a cache buster without the time of the request in development:\n#{missing.join("\n")}"
    end
end
