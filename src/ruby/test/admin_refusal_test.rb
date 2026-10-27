require 'minitest/autorun'

# A route for administrators refuses everyone else with 403
# (admins_only!, access.rb); an assert would answer 500, an error in the
# log instead of a refusal.
class AdminRefusalTest < Minitest::Test
    def test_no_route_asserts_the_admin
        root = File.join(__dir__, '..')
        sources = [File.join(root, 'main.rb')] + Dir[File.join(root, 'include', '*.rb')]
        sources.each do |path|
            refute_match(/assert\(\s*admin_logged_in\?/, File.read(path), "#{File.basename(path)}: use admins_only! instead")
        end
    end
end
