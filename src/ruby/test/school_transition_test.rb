require 'minitest/autorun'
require_relative '../include/school_transition'

# The transition overview (school_transition.rb): it checks what the
# administration enters, sorts the schools and fills in the templates, and
# it never decides about access, approval or consent.
class SchoolTransitionTest < Minitest::Test
    TODAY = Date.new(2026, 11, 20)
    DEADLINE = Date.new(2027, 4, 12)

    def school(extra = {})
        { 'key' => 'a', 'name' => 'Muster-Schule' }.merge(extra)
    end

    def test_clean_keeps_valid_values_and_removes_empty_ones
        values = SchoolTransition.clean(:transition_status => 'in_discussion', :next_follow_up_at => '2026-12-01',
            :transition_note => '  Gespräch mit der Schulleitung  ', :contacted_at => '')
        assert_equal({ 'transition_status' => 'in_discussion', 'next_follow_up_at' => '2026-12-01',
                       'transition_note' => 'Gespräch mit der Schulleitung', 'contacted_at' => nil }, values)
    end

    def test_clean_rejects_bad_status_dates_and_long_notes
        assert_equal 'invalid_status', assert_raises(SchoolTransition::Invalid) { SchoolTransition.clean(:transition_status => 'approved') }.code
        assert_equal 'invalid_date', assert_raises(SchoolTransition::Invalid) { SchoolTransition.clean(:last_contact_at => '1.12.2026') }.code
        assert_equal 'invalid_date', assert_raises(SchoolTransition::Invalid) { SchoolTransition.clean(:dpo_at => '2026-02-30') }.code
        assert_equal 'text_too_long', assert_raises(SchoolTransition::Invalid) { SchoolTransition.clean(:open_questions => 'x' * 1001) }.code
    end

    # The school's decision, the agreement and the approval are not progress:
    # they can only be set on their own route (school_status)
    def test_clean_ignores_the_legal_fields
        values = SchoolTransition.clean(:decision_at => '2026-11-01', :agreement_at => '2026-11-01', :approved_at => 1,
            :consent_at => '2026-11-01', :suspended => true, :transition_status => 'in_discussion')
        assert_equal ['transition_status'], values.keys
    end

    def test_no_status_means_approval
        refute SchoolTransition::STATES.keys.any? { |k| k =~ /approv|done|documented|consent/ }
        refute SchoolTransition::FIELDS.any? { |f| %w[decision_at agreement_at approved_at approved_by consent_at].include?(f) }
    end

    def test_categories
        assert_equal :documented, SchoolTransition.category(school('approved_at' => 1, 'transition_status' => 'follow_up'), TODAY)
        assert_equal :not_contacted, SchoolTransition.category(school, TODAY)
        assert_equal :not_contacted, SchoolTransition.category(school('transition_status' => 'not_started'), TODAY)
        assert_equal :in_progress, SchoolTransition.category(school('transition_status' => 'in_discussion'), TODAY)
        assert_equal :in_progress, SchoolTransition.category(school('contacted_at' => '2026-11-03'), TODAY)
        assert_equal :in_progress, SchoolTransition.category(school('transition_status' => 'in_discussion', 'next_follow_up_at' => '2026-12-01'), TODAY)
        assert_equal :follow_up, SchoolTransition.category(school('transition_status' => 'in_discussion', 'next_follow_up_at' => '2026-11-20'), TODAY)
        assert_equal :follow_up, SchoolTransition.category(school('transition_status' => 'follow_up'), TODAY)
    end

    # A decision alone, or progress marked as far along, is not approval
    def test_only_approved_at_counts_as_documented
        refute SchoolTransition.documented?(school('decision_at' => '2026-11-01', 'transition_status' => 'awaiting_documents'))
        assert SchoolTransition.documented?(school('approved_at' => 1))
    end

    def test_summary_leaves_out_the_guests
        schools = [school('approved_at' => 1), school('transition_status' => 'in_discussion'), school,
                   school('transition_status' => 'follow_up'), school('guests' => true, 'approved_at' => 1)]
        summary = SchoolTransition.summary(schools, TODAY, DEADLINE)
        assert_equal 4, summary[:total]
        assert_equal [1, 1, 1, 1], summary.values_at(:documented, :in_progress, :not_contacted, :follow_up)
        assert_equal 143, summary[:days_left]
        assert_equal '2027-04-12', summary[:deadline]
    end

    def test_phases
        assert_equal 'preparation', SchoolTransition.phase(Date.new(2026, 10, 25), DEADLINE)[:key]
        assert_equal 'information', SchoolTransition.phase(Date.new(2026, 11, 2), DEADLINE)[:key]
        assert_equal 'reminder', SchoolTransition.phase(Date.new(2027, 1, 15), DEADLINE)[:key]
        assert_equal 'follow_up', SchoolTransition.phase(Date.new(2027, 2, 10), DEADLINE)[:key]
        assert_equal 'notice', SchoolTransition.phase(Date.new(2027, 3, 1), DEADLINE)[:key]
        assert_equal 'after', SchoolTransition.phase(DEADLINE, DEADLINE)[:key]
        assert_equal 0, SchoolTransition.summary([], Date.new(2027, 5, 1), DEADLINE)[:days_left]
    end

    def test_next_step_and_templates
        assert_equal 'Schulleitung angesprochen', SchoolTransition.next_step(school)
        assert_equal 'Entscheidung der Schulleitung', SchoolTransition.next_step(school('contacted_at' => 'x', 'legal_basis_at' => 'x', 'dpo_at' => 'x'))
        assert_nil SchoolTransition.next_step(school('approved_at' => 1))
        text = SchoolTransition.fill('notice', school('contacted_at' => '2026-11-03'), DEADLINE, 'Max Muster')
        assert_includes text, 'Muster-Schule'
        assert_includes text, '12.04.2027'
        assert_includes text, 'Max Muster'
        assert_includes text, 'Entscheidung der Schulleitung'
        refute_includes text, 'Schulleitung angesprochen'
        %w[preparation information reminder follow_up notice after].each do |phase|
            assert SchoolTransition::TEMPLATES.key?(SchoolTransition.template_for(phase))
        end
    end

    # Neo4j gives a node's properties with symbol keys
    def test_symbol_keys_as_from_the_database
        row = { :name => 'Muster-Schule', :contacted_at => '2026-11-03', :approved_at => nil, :transition_status => 'in_discussion' }
        assert_equal :in_progress, SchoolTransition.category(row, TODAY)
        assert_equal 'Rechtsgrundlage mit der Schule geklärt', SchoolTransition.next_step(row)
        text = SchoolTransition.fill('reminder', row, DEADLINE, 'Max Muster')
        assert_includes text, 'Muster-Schule'
        refute_includes text, 'Schulleitung angesprochen'
        assert_equal :documented, SchoolTransition.category({ :approved_at => 5 }, TODAY)
        refute SchoolTransition.counts?({ :guests => true })
    end

    def test_deadline_can_be_set
        scope = Module.new
        assert_equal DEADLINE, SchoolTransition.deadline(scope)
        scope.const_set('TRANSITION_DEADLINE', '2027-05-03')
        assert_equal Date.new(2027, 5, 3), SchoolTransition.deadline(scope)
    end

    # The routes for the progress only touch the progress: no approval, no
    # consent, no access, no mail (people_page.rb)
    def test_the_routes_change_nothing_else
        source = File.read(File.join(__dir__, '../include/people_page.rb'))
        routes = source.scan(%r{post '/api/people/school_(?:transition|template)' do\n(.*?)\n    end\n}m).flatten
        assert_equal 2, routes.size
        routes.each do |route|
            assert_includes route, 'admin_only!'
            %w[deliver_mail suspend approved_at decision_at agreement_at consent_at note_roster_change!].each do |word|
                refute_includes route, word, "a transition route mentions #{word}"
            end
        end
    end
end
