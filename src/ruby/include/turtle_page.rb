require_relative 'turtle_check'
require_relative 'kata_check'
require_relative 'turtle_sketches'

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

    # the card's stored state: { 'answer' => …, 'status' => …, 'sketches' => … } or nil
    def turtle_previous(email, key)
        neo4j_query(<<~END_OF_QUERY, :email => email, :key => TaskStore.state_key(email, key)).first
            MATCH (:User {email: $email})-[:HAS_TASK_STATE]->(s:TaskState {key: $key})
            RETURN s.last_answer AS answer, s.status AS status, s.sketches AS sketches;
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

    # The open drawing of a free card (TurtleSketches) keeps the code that
    # just ran or was stopped.
    def turtle_sync_sketch(found, email, code, previous)
        return unless previous && previous['sketches'] && found[:task]['check']['free']

        books = TurtleSketches.parse(previous['sketches'])
        book = books[found[:task]['check']['language']] or return
        current = TurtleSketches.find(book['list'], book['current']) or return
        current['code'] = code.to_s[0, KataCheck::MAX_CODE]
        neo4j_query(<<~END_OF_QUERY, :email => email, :key => TaskStore.state_key(email, found[:key]), :sketches => JSON.generate(books))
            MATCH (:User {email: $email})-[:HAS_TASK_STATE]->(s:TaskState {key: $key})
            SET s.sketches = $sketches;
        END_OF_QUERY
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
            turtle_sync_sketch(found, email, code, previous)
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
        previous = turtle_previous(email, found[:key])
        state = turtle_keep_code(found, email, data[:answer], previous)
        turtle_sync_sketch(found, email, data[:answer], previous)
        respond(:saved => true, :state => state)
    end

    # ----- several drawings on a free card (TurtleSketches) -----

    # the drawings of a card handed in: [{ 'name', 'language', 'code', 'svg' }]
    def turtle_handed_in(task_key, email)
        name, text = SubmissionStore.read_text(submission_path(task_key, email), :max => 64 * 1024 * 1024)
        return [] unless name == TurtleSketches::FILE

        list = JSON.parse(text)
        list.is_a?(Array) ? list.select { |e| e.is_a?(Hash) && e['name'].is_a?(String) } : []
    rescue JSON::ParserError
        []
    end

    def turtle_write_handed(task_key, email, list)
        path = submission_path(task_key, email)
        if list.empty?
            FileUtils.rm_f(path)
        else
            SubmissionStore.write_text(path, TurtleSketches::FILE, JSON.generate(list))
        end
    end

    # The drawings of the card's language, the one open first; any action
    # answers with all of them, the open one's code and which are handed
    # in. action: load, save (code of the open one), open, new, rename (name,
    # to), delete (name), hand_in (code and the commands of its last run as
    # report), take_back. The open drawing is also the card's answer
    # (last_answer), as on every card.
    post '/api/turtle/sketch' do
        assert(user_logged_in?)
        assert(tasks_visible?)
        data = parse_request_data(:required_keys => [:task, :action], :optional_keys => [:language, :name, :to, :code, :report],
            :max_body_length => TURTLE_MAX_REPORT + 2 * KataCheck::MAX_CODE,
            :max_value_lengths => {:task => 120, :action => 20, :language => 20, :name => 200, :to => 200,
                                   :code => KataCheck::MAX_CODE, :report => TURTLE_MAX_REPORT})
        found = find_task(data[:task], data[:language])
        halt 404 if found.nil? || found[:task]['check']['type'] != 'turtle' || !found[:task]['check']['free']

        email = @session_user[:email]
        language = found[:task]['check']['language']
        starter = found[:task]['starter'].to_s
        previous = turtle_previous(email, found[:key])
        books = TurtleSketches.parse(previous && previous['sketches'])
        book = books[language] || { 'current' => '', 'list' => [] }
        list = book['list']
        if list.empty?
            code = previous ? (JSON.parse(previous['answer'].to_s) rescue {})[language] : nil
            list << { 'name' => 'Bild 1', 'code' => code.is_a?(String) ? code : starter }
        end
        book['current'] = list.first['name'] unless TurtleSketches.find(list, book['current'])
        current = TurtleSketches.find(list, book['current'])
        handed = turtle_handed_in(found[:key], email)
        handed_before = JSON.generate(handed)
        current['code'] = data[:code].to_s if data.key?(:code) && data[:action] != 'load'
        error = nil
        case data[:action]
        when 'load', 'save'
            nil
        when 'open'
            target = TurtleSketches.find(list, data[:name].to_s)
            halt 404 unless target
            book['current'] = target['name']
        when 'new'
            name = data[:name].to_s.strip.empty? ? TurtleSketches.free_name(list) : TurtleSketches.name(data[:name])
            if list.size >= TurtleSketches::MAX
                error = "Mehr als #{TurtleSketches::MAX} Bilder gehen nicht: Lösch eines, das du nicht mehr brauchst."
            elsif name.nil?
                error = "Ein Name hat 1 bis #{TurtleSketches::MAX_NAME} Zeichen."
            elsif TurtleSketches.find(list, name)
                error = "Ein Bild »#{name}« gibt es schon."
            else
                list << { 'name' => name, 'code' => starter }
                book['current'] = name
            end
        when 'rename'
            target = TurtleSketches.find(list, data[:name].to_s)
            halt 404 unless target
            to = TurtleSketches.name(data[:to])
            if to.nil?
                error = "Ein Name hat 1 bis #{TurtleSketches::MAX_NAME} Zeichen."
            elsif to != target['name'] && TurtleSketches.find(list, to)
                error = "Ein Bild »#{to}« gibt es schon."
            else
                handed.each { |e| e['name'] = to if e['name'] == target['name'] && e['language'] == language }
                book['current'] = to if book['current'] == target['name']
                target['name'] = to
            end
        when 'delete'
            target = TurtleSketches.find(list, data[:name].to_s)
            halt 404 unless target
            list.delete(target)
            handed.reject! { |e| e['name'] == target['name'] && e['language'] == language }
            list << { 'name' => TurtleSketches.free_name(list), 'code' => starter } if list.empty?
            book['current'] = list.first['name'] unless TurtleSketches.find(list, book['current'])
        when 'hand_in'
            commands = begin
                JSON.parse(data[:report].to_s)
            rescue JSON::ParserError
                nil
            end
            world = commands.is_a?(Array) ? TurtleSketches.world(commands) : nil
            segments = world&.segments
            others = handed.reject { |e| e['name'] == current['name'] && e['language'] == language }
            if segments.nil? || segments.empty?
                error = 'Lass dein Bild zuerst laufen: Eingereicht wird, was die Schildkröte zuletzt gezeichnet hat.'
            elsif others.size >= TurtleSketches::MAX_HANDED
                error = "Mehr als #{TurtleSketches::MAX_HANDED} Bilder gehen nicht zum Einreichen: Nimm erst eines zurück."
            else
                handed = others + [{ 'name' => current['name'], 'language' => language, 'code' => current['code'],
                                     'svg' => TurtleSketches.svg(segments, world.paper) }]
            end
        when 'take_back'
            handed.reject! { |e| e['name'] == current['name'] && e['language'] == language }
        else
            halt 400
        end
        turtle_write_handed(found[:key], email, handed) if JSON.generate(handed) != handed_before

        current = TurtleSketches.find(list, book['current'])
        handed_names = handed.select { |e| e['language'] == language }.map { |e| e['name'] }
        # only looking stores nothing
        state = previous ? { :status => previous['status'] } : nil
        unless data[:action] == 'load'
            books[language] = book
            state = turtle_keep_code(found, email, current['code'], previous)
            neo4j_query(<<~END_OF_QUERY, :email => email, :key => TaskStore.state_key(email, found[:key]), :sketches => JSON.generate(books))
                MATCH (:User {email: $email})-[:HAS_TASK_STATE]->(s:TaskState {key: $key})
                SET s.sketches = $sketches;
            END_OF_QUERY
        end
        respond(:list => list.map { |e| e['name'] }, :current => book['current'], :code => current['code'],
                :handed => handed_names, :error => error, :state => state)
    end
end
