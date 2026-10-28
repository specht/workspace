require_relative 'turtle_check'
require_relative 'kata_check'

# The turtle (docs/plan-katas.md, »Turtle«): the program runs in the
# browser (turtle_worker.js), so nothing waits in the Katas' queue. A run
# that ends by itself comes here to be checked, with the commands it drew;
# a run that is stopped only keeps its code. Like a Kata, the code is kept
# per language ({"python": "…"}) until the student deletes it.
class Main < Sinatra::Base
    # a report of 200 000 commands, as JSON
    TURTLE_MAX_REPORT = 6_000_000

    def turtle_request(extra_keys = [])
        assert(user_logged_in?)
        assert(tasks_visible?)
        data = parse_request_data(:required_keys => [:task, :answer], :optional_keys => [:language] + extra_keys,
            :max_body_length => TURTLE_MAX_REPORT + 2 * KataCheck::MAX_CODE,
            :max_value_lengths => {:task => 120, :answer => KataCheck::MAX_CODE, :report => TURTLE_MAX_REPORT})
        found = find_task(data[:task], data[:language])
        halt 404 if found.nil? || found[:task]['check']['type'] != 'turtle'
        [data, found]
    end

    # the card's stored state: { 'answer' => …, 'status' => … } or nil
    def turtle_previous(email, key)
        neo4j_query(<<~END_OF_QUERY, :email => email, :key => TaskStore.state_key(email, key)).first
            MATCH (:User {email: $email})-[:HAS_TASK_STATE]->(s:TaskState {key: $key})
            RETURN s.last_answer AS answer, s.status AS status;
        END_OF_QUERY
    end

    # The one sentence for a picture that is not (yet) the one asked for.
    def turtle_gentle(task, result)
        show = result.replay
        return result.message unless show
        return result.message if show[:drawn].empty?
        if show[:call] && show[:run] != (task['check']['show'] || 0)
            return "Die Prüfung ruft deine Funktion auch als `#{show[:call].strip}` auf, und dieses Bild passt noch nicht ganz."
        end

        'Liegt dein Bild genau auf der Bleistiftlinie, ist die Aufgabe gelöst.'
    end

    # Keeps the code, leaves the status as it is; without a state yet, the
    # card starts as tried.
    def turtle_keep_code(found, email, code, previous)
        answer = KataCheck.stored_answer(previous && previous['answer'], found[:task]['check']['language'], code)
        return store_task_result(found, 'wrong', answer, email, :max => KataCheck::MAX_STORED) unless previous

        neo4j_query(<<~END_OF_QUERY, :email => email, :key => TaskStore.state_key(email, found[:key]), :answer => answer)
            MATCH (:User {email: $email})-[:HAS_TASK_STATE]->(s:TaskState {key: $key})
            SET s.last_answer = $answer;
        END_OF_QUERY
        { :status => previous['status'] }
    end

    post '/api/turtle/check' do
        data, found = turtle_request([:report])
        task = found[:task]
        code = data[:answer].to_s
        report = begin
            JSON.parse(data[:report].to_s)
        rescue JSON::ParserError
            nil
        end
        result = TaskCheck.check(task, code, :turtle => report)
        status = result.status.to_s
        state = nil
        if result.counts?
            email = @session_user[:email]
            previous = turtle_previous(email, found[:key])
            # Solved before: a run that draws something else is the
            # student's own experiment (the maintainer: not punished). The
            # code is kept, the card stays solved, and nothing says »wrong«.
            if previous && previous['status'] == 'correct' && status != 'correct'
                status = 'free'
                state = turtle_keep_code(found, email, code, previous)
            else
                answer = KataCheck.stored_answer(previous && previous['answer'], task['check']['language'], code)
                state = store_task_result(found, status, answer, email, :max => KataCheck::MAX_STORED)
            end
            state[:handed_in] = File.file?(submission_path(found[:key], email))
        end
        if status == 'free'
            respond(:done => true, :status => status, :state => state,
                    :message => render_task_markdown('Die Aufgabe hast du schon gelöst. Jetzt kannst du frei ausprobieren, geprüft wird nichts mehr.'))
        elsif status == 'wrong' && (result.replay || task['check']['free'])
            # A picture on its way (the maintainer: developing a fern takes
            # many runs, and a card should not say »wrong« at each one):
            # one quiet sentence; what differs, drawn and in words, only
            # when the student asks for it (tasks.js, »Unterschiede zeigen«).
            respond(:done => true, :status => 'drawn', :state => state,
                    :message => render_task_markdown(turtle_gentle(task, result)),
                    :detail => result.replay ? render_task_markdown(result.message) : nil,
                    :replay => result.replay)
        else
            respond(:done => true, :status => status,
                    :message => render_task_markdown(result.message),
                    :solution => render_task_markdown(result.solution, :lineno => true),
                    :state => state, :replay => result.replay)
        end
    end

    # A stopped run, or one that broke off: the code is kept, the status
    # stays as it was (a solved card stays solved).
    post '/api/turtle/save' do
        data, found = turtle_request
        email = @session_user[:email]
        state = turtle_keep_code(found, email, data[:answer], turtle_previous(email, found[:key]))
        respond(:saved => true, :state => state)
    end
end
