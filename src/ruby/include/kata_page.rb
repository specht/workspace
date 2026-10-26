require_relative 'kata_check'
require_relative 'kata_hand_ins'
require_relative 'submission_store'
require_relative 'kata_queue'
require_relative 'kata_runner'
require_relative 'pip_check'

# Katas (docs/plan-katas.md): the code typed into a card runs in a container
# of its own (KataRunner), through the one queue (KataQueue). A request does
# not wait for its run, which would hold a thread of the web server while a
# whole class waits: /api/kata/check and /api/kata/run hand back a ticket,
# and the card asks /api/kata/poll until the run is done, with its place in
# the queue meanwhile. A check's result is stored on that request, in the
# person's own session, once, as /api/task/check stores it.
class Main < Sinatra::Base
    # How many runs at a time: KATA_PARALLEL_RUNS in credentials.rb, measured
    # with src/scripts/measure-katas.rb.
    KATA_QUEUE = KataQueue.new(:parallel => defined?(KATA_PARALLEL_RUNS) ? KATA_PARALLEL_RUNS : 4)
    # A run that waited this long for a place is given up (»Gerade prüfen
    # sehr viele …«).
    KATA_MAX_WAIT = 120

    # The run of one ticket: a newer run of the same person stops it.
    def self.kata_run(ticket)
        lambda do |files:, argv:, timeout:, dialog: nil|
            KataRunner.run(:files => files, :argv => argv, :timeout => timeout, :dialog => dialog,
                           :cancel => ->(kill) { ticket.on_stop(kill) })
        end
    end

    KATA_TYPES = %w[function pip].freeze

    def kata_request
        assert(user_logged_in?)
        assert(tasks_visible?)
        data = parse_request_data(:required_keys => [:task, :answer], :optional_keys => [:language, :map],
            :max_body_length => 16384,
            :max_value_lengths => {:task => 120, :answer => KataCheck::MAX_CODE})
        found = find_task(data[:task], data[:language])
        halt 404 if found.nil? || !KATA_TYPES.include?(found[:task]['check']['type'])
        # Pip: the map the card shows (its tab), counted from 0
        found[:map] = data[:map].to_s =~ /\A\d{1,2}\z/ ? data[:map].to_i : 0
        [data[:answer], found]
    end

    post '/api/kata/check' do
        code, found = kata_request
        return respond(:done => true, :status => 'invalid', :message => render_task_markdown(KataCheck::EMPTY)) if code.strip.empty?

        task = found[:task]
        seed = KataCheck.seed(@session_user[:email], found[:key], found[:version])
        ticket = KATA_QUEUE.submit(@session_user[:email]) do |t|
            # one button: a function Kata's own program runs first, where it
            # has code of its own (KataCheck.top_level?), for what it prints
            language = task['check']['language']
            ran = task['check']['type'] == 'function' && KataCheck.top_level?(code, language) ? KataCheck.run_program(language, code, Main.kata_run(t)) : nil
            [TaskCheck.check(task, code, :kata => Main.kata_run(t), :kata_seed => seed, :kata_map => found[:map]), ran]
        end
        ticket.data = { :kind => :check, :found => found, :code => code }
        respond(:done => false, :ticket => ticket.id, :ahead => KATA_QUEUE.ahead(ticket))
    end

    # »Ausführen«: the program once; nothing is stored.
    post '/api/kata/run' do
        code, found = kata_request
        language = found[:task]['check']['language']
        return respond(:done => true, :status => 'invalid', :message => render_task_markdown(KataCheck::EMPTY)) if code.strip.empty?

        check = found[:task]['check']
        seed = KataCheck.seed(@session_user[:email], found[:key], found[:version])
        ticket = KATA_QUEUE.submit(@session_user[:email]) do |t|
            if check['type'] == 'pip'
                # Pip's plan on the map the card shows, to watch, as it is
                # written: with the plan's own calls of its functions. Only a
                # plan that does nothing by itself (just a function) gets the
                # call the check makes (call:), so that one sees what the
                # function does.
                index = found[:map].clamp(0, check['maps'].size - 1)
                map = PipCheck.maps(check, seed)[index]
                ran = PipCheck.run_map(check, code, map.reject { |k, _| %w[call expect].include?(k) }, Main.kata_run(t))
                if ran && map['call'] && ran[:world].trace.size <= 2 && ran[:world].ended == :done && ran[:world].said.empty?
                    ran = PipCheck.run_map(check, code, map, Main.kata_run(t))
                end
                next { :status => :unavailable, :output => '', :message => KataCheck::BUSY } if ran.nil?

                error = ran[:status] == 0 || ran[:errors].to_s.strip.empty? ? nil : ran[:errors]
                { :status => error ? :error : :ran, :output => ran[:output].to_s + ran[:errors].to_s,
                  :message => error ? KataCheck.explain(error, language) : nil,
                  :replay => PipCheck.replay(ran[:world], index, check['maps'].size, ran[:output]) }
            else
                KataCheck.run_program(language, code, Main.kata_run(t))
            end
        end
        ticket.data = { :kind => :run }
        respond(:done => false, :ticket => ticket.id, :ahead => KATA_QUEUE.ahead(ticket))
    end

    post '/api/kata/poll' do
        assert(user_logged_in?)
        data = parse_request_data(:required_keys => [:ticket], :max_value_lengths => {:ticket => 40})
        ticket = KATA_QUEUE.find(data[:ticket])
        halt 404 if ticket.nil? || ticket.owner != @session_user[:email]

        case ticket.state
        when :waiting
            if Time.now - ticket.created_at > KATA_MAX_WAIT
                ticket.cancel
                return respond(:done => true, :status => 'unavailable', :message => render_task_markdown(KataCheck::BUSY))
            end
            respond(:done => false, :ahead => KATA_QUEUE.ahead(ticket).to_i)
        when :running
            respond(:done => false, :running => true)
        when :cancelled
            # a newer run of the same person took its place
            respond(:done => true, :status => 'cancelled')
        else
            respond(ticket.once(:response) { kata_response(ticket) })
        end
    end

    # Einreichen (KataHandIns): a copy of the stored code of a solved Kata
    # in this language, as the task's hand-in; the student's own choice.
    post '/api/kata/hand_in' do
        assert(user_logged_in?)
        assert(tasks_visible?)
        data = parse_request_data(:required_keys => [:task], :optional_keys => [:language],
            :max_value_lengths => {:task => 120, :language => 20})
        found = find_task(data[:task], data[:language])
        halt 404 if found.nil? || !KATA_TYPES.include?(found[:task]['check']['type'])

        language = found[:task]['check']['language']
        email = @session_user[:email]
        stored = neo4j_query(<<~END_OF_QUERY, :email => email, :key => TaskStore.state_key(email, found[:key])).first
            MATCH (:User {email: $email})-[:HAS_TASK_STATE]->(s:TaskState {key: $key})
            RETURN s.status AS status, s.version AS version, s.last_answer AS answer;
        END_OF_QUERY
        halt 409 unless stored && stored['status'] == 'correct' && stored['version'] == found[:version]

        code = (JSON.parse(stored['answer'].to_s) rescue {})[language]
        halt 409 unless code.is_a?(String) && !code.strip.empty?

        SubmissionStore.write_text(submission_path(found[:key]), KataCheck::FILES.fetch(language), code)
        respond(:handed_in => true)
    end

    post '/api/kata/take_back' do
        assert(user_logged_in?)
        data = parse_request_data(:required_keys => [:task], :max_value_lengths => {:task => 120})
        found = find_task(data[:task])
        halt 404 if found.nil? || !KATA_TYPES.include?(found[:task]['check']['type'])

        FileUtils.rm_f(submission_path(found[:key]))
        respond(:handed_in => false)
    end

    # The Katas a group handed in, per Kata of the activity, for the class
    # board: numbered, without names until the teacher shows them.
    def class_kata_hand_ins_html(activity_key, tasks, members)
        tasks.each_with_index.map do |t, i|
            next '' unless KATA_TYPES.include?(t[:type])

            key = "#{activity_key}/#{t[:id]}"
            entries = members.filter_map do |m|
                name, code = SubmissionStore.read_text(submission_path(key, m[:email]))
                next unless code

                language = KataCheck::FILES.key(name) || 'ruby'
                lexer = Rouge::Lexer.find(language) || Rouge::Lexers::PlainText
                { :email => m[:email], :name => m[:name], :language => language,
                  :code_html => Rouge::Formatters::HTML.new.format(lexer.new.lex(code)) }
            end
            KataHandIns.html(key, i + 1, t[:title], entries)
        end.join
    end

    def kata_response(ticket)
        if ticket.error
            STDERR.puts "Kata run failed: #{ticket.error.class}: #{ticket.error.message}"
            return { :done => true, :status => 'unavailable',
                     :message => render_task_markdown('Das hat gerade nicht geklappt. Versuch es gleich noch einmal.') }
        end
        if ticket.data[:kind] == :run
            ran = ticket.result
            return { :done => true, :status => ran[:status].to_s, :output => ran[:output],
                     :message => ran[:message] ? render_task_markdown(ran[:message]) : nil, :replay => ran[:replay] }
        end

        result, ran = ticket.result
        found = ticket.data[:found]
        state = nil
        if result.counts?
            language = found[:task]['check']['language']
            previous = neo4j_query(<<~END_OF_QUERY, :email => ticket.owner, :key => TaskStore.state_key(ticket.owner, found[:key])).first
                MATCH (:User {email: $email})-[:HAS_TASK_STATE]->(s:TaskState {key: $key})
                RETURN s.last_answer AS answer;
            END_OF_QUERY
            answer = KataCheck.stored_answer(previous && previous['answer'], language, ticket.data[:code])
            state = store_task_result(found, result.status.to_s, answer, ticket.owner, :max => KataCheck::MAX_STORED)
            state[:handed_in] = File.file?(submission_path(found[:key], ticket.owner))
        end
        {
            :done => true,
            :status => result.status.to_s,
            :message => render_task_markdown(result.message),
            :solution => render_task_markdown(result.solution),
            :read => render_task_markdown(result.read),
            :state => state,
            :replay => result.replay,
            # what the program printed (or why it stopped), for the console
            :output => ran && ran[:output],
            :ran => ran && ran[:status].to_s,
        }
    end
end
