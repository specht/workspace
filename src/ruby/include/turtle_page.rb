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

    def turtle_previous_answer(email, key)
        row = neo4j_query(<<~END_OF_QUERY, :email => email, :key => TaskStore.state_key(email, key)).first
            MATCH (:User {email: $email})-[:HAS_TASK_STATE]->(s:TaskState {key: $key})
            RETURN s.last_answer AS answer;
        END_OF_QUERY
        row && row['answer']
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
        state = nil
        if result.counts?
            email = @session_user[:email]
            answer = KataCheck.stored_answer(turtle_previous_answer(email, found[:key]), task['check']['language'], code)
            state = store_task_result(found, result.status.to_s, answer, email, :max => KataCheck::MAX_STORED)
            state[:handed_in] = File.file?(submission_path(found[:key], email))
        end
        respond(:done => true, :status => result.status.to_s,
                :message => render_task_markdown(result.message),
                :solution => render_task_markdown(result.solution, :lineno => true),
                :state => state, :replay => result.replay)
    end

    # A stopped run, or one that broke off: the code is kept, the status
    # stays as it was (a solved card stays solved). Without a state yet,
    # it starts as tried.
    post '/api/turtle/save' do
        data, found = turtle_request
        email = @session_user[:email]
        key = TaskStore.state_key(email, found[:key])
        answer = KataCheck.stored_answer(turtle_previous_answer(email, found[:key]), found[:task]['check']['language'], data[:answer])
        updated = neo4j_query(<<~END_OF_QUERY, :email => email, :key => key, :answer => answer, :now => Time.now.to_i).first
            MATCH (:User {email: $email})-[:HAS_TASK_STATE]->(s:TaskState {key: $key})
            SET s.last_answer = $answer
            RETURN s.status AS status;
        END_OF_QUERY
        state = updated ? { :status => updated['status'] } : store_task_result(found, 'wrong', answer, email, :max => KataCheck::MAX_STORED)
        respond(:saved => true, :state => state)
    end
end
