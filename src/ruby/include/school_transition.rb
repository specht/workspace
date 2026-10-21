# The transition to documented approval at every school (docs/handoff-datenschutz.md,
# »Übergangszeit«): until the deadline (12 April 2027, TRANSITION_DEADLINE in
# credentials.rb) nobody is locked out over a school's paperwork; the
# administration keeps track of where each school stands and what is due.
#
# The progress is kept apart from the legal status on purpose: a school is
# approved (approved_at, people_page.rb) only when the Schulleitung's decision
# and the agreement on processing are recorded. Nothing here sets or clears
# approved_at, decision_at, agreement_at or a student's consent_at, and nothing
# here sends mail or changes access: it is an overview for the administration.
require 'date'

module SchoolTransition
    DEFAULT_DEADLINE = Date.new(2027, 4, 12)

    # How the talks stand, chosen by the administration (never a legal status)
    STATES = {
        'not_started' => 'noch nicht angesprochen',
        'contact_planned' => 'Gespräch geplant',
        'in_discussion' => 'im Gespräch',
        'awaiting_documents' => 'Unterlagen ausstehend',
        'follow_up' => 'Nachfassen nötig',
    }.freeze

    # The steps for every school, each a date when it was done. decision_at
    # and agreement_at are the school's own fields (people_page.rb, school_status);
    # they are shown here, but saved only through school_status.
    STEPS = [
        ['contacted_at', 'Schulleitung angesprochen'],
        ['legal_basis_at', 'Rechtsgrundlage mit der Schule geklärt'],
        ['dpo_at', 'Datenschutzbeauftragte Stelle beteiligt'],
        ['decision_at', 'Entscheidung der Schulleitung'],
        ['agreement_at', 'Vereinbarung zur Auftragsverarbeitung unterschrieben'],
    ].freeze
    OWN_STEPS = %w[contacted_at legal_basis_at dpo_at].freeze

    DATE_FIELDS = (%w[last_contact_at next_follow_up_at] + OWN_STEPS).freeze
    TEXT_FIELDS = { 'transition_note' => 1000, 'open_questions' => 1000 }.freeze
    FIELDS = (%w[transition_status] + DATE_FIELDS + TEXT_FIELDS.keys).freeze

    class Invalid < StandardError
        attr_reader :code

        def initialize(code)
            @code = code
            super(code)
        end
    end

    def self.deadline(scope = Object)
        value = scope.const_defined?('TRANSITION_DEADLINE') ? scope.const_get('TRANSITION_DEADLINE') : nil
        return DEFAULT_DEADLINE if value.nil?

        value.is_a?(Date) ? value : Date.iso8601(value.to_s)
    end

    # The fields of a request, checked: only the keys given are returned, an
    # empty value removes the field (nil). Raises Invalid with a code.
    def self.clean(data)
        values = {}
        data.each do |key, value|
            key = key.to_s
            next unless FIELDS.include?(key)

            text = value.to_s.strip
            values[key] =
                if text.empty? then nil
                elsif key == 'transition_status'
                    raise Invalid, 'invalid_status' unless STATES.key?(text)

                    text
                elsif DATE_FIELDS.include?(key)
                    begin
                        Date.iso8601(text).to_s
                    rescue ArgumentError
                        raise Invalid, 'invalid_date'
                    end
                else
                    raise Invalid, 'text_too_long' if text.size > TEXT_FIELDS[key]

                    text
                end
        end
        values
    end

    # A property of a school: from Neo4j with symbol keys, in tests with strings
    def self.get(school, key)
        school.key?(key.to_sym) ? school[key.to_sym] : school[key.to_s]
    end

    def self.documented?(school)
        !get(school, 'approved_at').nil?
    end

    # The guests need no school's decision (they agree to the Nutzungsordnung)
    def self.counts?(school)
        !get(school, 'guests')
    end

    def self.date(value)
        value.nil? || value.to_s.empty? ? nil : Date.iso8601(value.to_s)
    rescue ArgumentError
        nil
    end

    # documented: decision and agreement recorded; follow_up: a follow-up date
    # has come; not_contacted: nothing done yet; in_progress: everything else
    def self.category(school, today = Date.today)
        return :documented if documented?(school)

        status = get(school, 'transition_status')
        follow = date(get(school, 'next_follow_up_at'))
        return :follow_up if (follow && follow <= today) || status == 'follow_up'

        nothing = (status.nil? || status == 'not_started') &&
                  OWN_STEPS.all? { |step| get(school, step).nil? } && get(school, 'last_contact_at').nil?
        nothing ? :not_contacted : :in_progress
    end

    # What is next for a school, in a few words
    def self.next_step(school)
        return nil if documented?(school)

        step = STEPS.find { |key, _| get(school, key).nil? }
        step && step[1]
    end

    # The phases agreed with the maintainer; text: what the administration does now
    def self.phase(today = Date.today, deadline = DEFAULT_DEADLINE)
        year = deadline.year
        if today >= deadline
            { :key => 'after', :text => 'Der Stichtag ist erreicht. Was mit Schulen ohne vollständige Unterlagen geschieht, entscheidet der Betreiber; von selbst ändert sich nichts.' }
        elsif today >= Date.new(year, 3, 1)
            { :key => 'notice', :text => 'März: Schulen ohne vollständige Unterlagen rechtzeitig und deutlich auf den Stichtag hinweisen.' }
        elsif today >= Date.new(year, 2, 1)
            { :key => 'follow_up', :text => 'Februar: bei ausstehenden Entscheidungen und Vereinbarungen nachfassen.' }
        elsif today >= Date.new(year, 1, 1)
            { :key => 'reminder', :text => 'Januar: freundlich erinnern und den Stand zusammenfassen; Ende Januar der Zwischenstand.' }
        elsif today >= Date.new(year - 1, 11, 2)
            { :key => 'information', :text => 'November und Dezember: die Schulleitungen informieren und das Gespräch suchen.' }
        else
            { :key => 'preparation', :text => 'Bis zu den Herbstferien: die Unterlagen vorbereiten, noch kein Aufwand für die Schulen.' }
        end
    end

    def self.summary(schools, today = Date.today, deadline = DEFAULT_DEADLINE)
        counted = schools.select { |s| counts?(s) }
        groups = counted.group_by { |s| category(s, today) }
        {
            :deadline => deadline.to_s,
            :days_left => [(deadline - today).to_i, 0].max,
            :phase => phase(today, deadline),
            :total => counted.size,
            :documented => groups.fetch(:documented, []).size,
            :in_progress => groups.fetch(:in_progress, []).size,
            :not_contacted => groups.fetch(:not_contacted, []).size,
            :follow_up => groups.fetch(:follow_up, []).size,
        }
    end

    # Templates to copy into a mail of one's own; nothing is sent from here.
    # %{school}, %{deadline} and %{operator} are filled in by fill.
    TEMPLATES = {
        'information' => {
            :title => 'Erste Information an die Schulleitung',
            :text => <<~TEXT,
                Sehr geehrte Schulleitung der %{school},

                Lehrkräfte Ihrer Schule nutzen den Hackschule Workspace im Informatikunterricht. Damit die Nutzung auch formal geregelt ist, möchte ich mit Ihnen besprechen, auf welcher Grundlage Ihre Schule ihn einsetzt, und die nötigen Unterlagen vorbereiten: die Entscheidung der Schulleitung und die Vereinbarung zur Auftragsverarbeitung. Alle Unterlagen finden Sie im Workspace unter »Unterlagen für die Schule«.

                Bis zum %{deadline} läuft der Unterricht wie bisher weiter. Ich melde mich in den nächsten Wochen für ein kurzes Gespräch, gern auch mit Ihrer Datenschutzbeauftragten Stelle.

                Mit freundlichen Grüßen
                %{operator}
            TEXT
        },
        'reminder' => {
            :title => 'Freundliche Erinnerung',
            :text => <<~TEXT,
                Sehr geehrte Schulleitung der %{school},

                ich möchte freundlich an die Unterlagen zum Hackschule Workspace erinnern. Offen ist noch: %{open}. Wenn Fragen geblieben sind, beantworte ich sie gern.

                Mit freundlichen Grüßen
                %{operator}
            TEXT
        },
        'notice' => {
            :title => 'Hinweis auf den Stichtag',
            :text => <<~TEXT,
                Sehr geehrte Schulleitung der %{school},

                ab dem %{deadline} soll der Workspace im regulären Unterricht nur noch an Schulen genutzt werden, deren Grundlage dokumentiert ist. Für Ihre Schule fehlt noch: %{open}. Konten und Arbeiten der Schülerinnen und Schüler bleiben in jedem Fall erhalten; wie es danach weitergeht, kläre ich vorher mit Ihnen.

                Mit freundlichen Grüßen
                %{operator}
            TEXT
        },
    }.freeze

    def self.template_for(phase_key)
        case phase_key
        when 'preparation', 'information' then 'information'
        when 'reminder', 'follow_up' then 'reminder'
        else 'notice'
        end
    end

    def self.open_steps(school)
        STEPS.reject { |key, _| get(school, key) }.map(&:last)
    end

    def self.fill(template_key, school, deadline, operator)
        open = open_steps(school)
        TEMPLATES.fetch(template_key)[:text] % {
            :school => get(school, 'name'),
            :deadline => deadline.strftime('%d.%m.%Y'),
            :operator => operator.to_s.empty? ? '…' : operator,
            :open => open.empty? ? 'nichts' : open.join(', '),
        }
    end
end
