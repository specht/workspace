require_relative 'kata_check'
require_relative 'kata_queue'
require_relative 'kata_runner'

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
        lambda do |files:, argv:, timeout:|
            KataRunner.run(:files => files, :argv => argv, :timeout => timeout, :cancel => ->(kill) { ticket.on_stop(kill) })
        end
    end

    def kata_request
        assert(user_logged_in?)
        assert(tasks_visible?)
        data = parse_request_data(:required_keys => [:task, :answer], :optional_keys => [:language],
            :max_body_length => 16384,
            :max_value_lengths => {:task => 120, :answer => KataCheck::MAX_CODE})
        found = find_task(data[:task], data[:language])
        halt 404 if found.nil? || found[:task]['check']['type'] != 'function'
        [data[:answer], found]
    end

    post '/api/kata/check' do
        code, found = kata_request
        return respond(:done => true, :status => 'invalid', :message => render_task_markdown(KataCheck::EMPTY)) if code.strip.empty?

        task = found[:task]
        ticket = KATA_QUEUE.submit(@session_user[:email]) do |t|
            TaskCheck.check(task, code, :kata => Main.kata_run(t))
        end
        ticket.data = { :kind => :check, :found => found, :code => code }
        respond(:done => false, :ticket => ticket.id, :ahead => KATA_QUEUE.ahead(ticket))
    end

    # »Ausführen«: the program once; nothing is stored.
    post '/api/kata/run' do
        code, found = kata_request
        language = found[:task]['check']['language']
        return respond(:done => true, :status => 'invalid', :message => render_task_markdown(KataCheck::EMPTY)) if code.strip.empty?

        ticket = KATA_QUEUE.submit(@session_user[:email]) do |t|
            KataCheck.run_program(language, code, Main.kata_run(t))
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

    def kata_response(ticket)
        if ticket.error
            STDERR.puts "Kata run failed: #{ticket.error.class}: #{ticket.error.message}"
            return { :done => true, :status => 'unavailable',
                     :message => render_task_markdown('Das hat gerade nicht geklappt. Versuch es gleich noch einmal.') }
        end
        if ticket.data[:kind] == :run
            ran = ticket.result
            return { :done => true, :status => ran[:status].to_s, :output => ran[:output],
                     :message => ran[:message] ? render_task_markdown(ran[:message]) : nil }
        end

        result = ticket.result
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
        end
        {
            :done => true,
            :status => result.status.to_s,
            :message => render_task_markdown(result.message),
            :solution => render_task_markdown(result.solution),
            :read => render_task_markdown(result.read),
            :state => state,
        }
    end
end
