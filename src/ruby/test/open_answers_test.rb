require 'minitest/autorun'
require_relative '../include/open_answers'

class OpenAnswersTest < Minitest::Test
    def test_only_reflection_tasks_in_page_order
        tasks = {
            :on_page => %w[b a c],
            :by_id => {
                'a' => { 'check' => { 'type' => 'reflection' } },
                'b' => { 'check' => { 'type' => 'reflection' } },
                'c' => { 'check' => { 'type' => 'answer' } },
            },
        }
        assert_equal %w[b a], OpenAnswers.task_ids(tasks)
    end

    def test_the_view_chosen_last_unless_it_cannot_be_shown
        assert_equal 'group', OpenAnswers.view('group', :examples => true, :groups => true)
        assert_equal 'off', OpenAnswers.view('off', :examples => true, :groups => false)
        assert_equal 'examples', OpenAnswers.view('examples', :examples => true, :groups => false)
        # Without a choice, or one that cannot be shown: closed
        assert_equal 'off', OpenAnswers.view(nil, :examples => true, :groups => true)
        assert_equal 'off', OpenAnswers.view('group', :examples => true, :groups => false)
        assert_equal 'off', OpenAnswers.view('examples', :examples => false, :groups => true)
        assert_equal 'off', OpenAnswers.view('nonsense', :examples => true, :groups => true)
    end

    def test_answers_sorted_by_name_without_empty_ones_marked_when_stale
        members = [
            { :email => 'z@x', :name => 'Zoe' },
            { :email => 'a@x', :name => 'anna' },
            { :email => 'm@x', :name => 'Max' },
            { :email => 'n@x', :name => 'Nora' },
        ]
        states = {
            'z@x' => { 'warum' => { :last_answer => 'Weil es so ist.', :version => 'v2', :updated => 20 } },
            'a@x' => { 'warum' => { :last_answer => 'Darum.', :version => 'v1', :updated => 10 } },
            'm@x' => { 'warum' => { :last_answer => '  ', :version => 'v2', :updated => 5 } },
        }
        answers = OpenAnswers.collect(members, { 'warum' => { :version => 'v2' }, 'leer' => { :version => 'v1' } }, states)
        assert_equal [], answers['leer']
        assert_equal %w[anna Zoe], answers['warum'].map { |a| a[:name] }
        assert_equal [true, false], answers['warum'].map { |a| a[:stale] }
        assert_equal 'Weil es so ist.', answers['warum'].last[:answer]
        assert_equal 20, answers['warum'].last[:updated]
    end

    def test_an_example_in_one_language_names_it
        assert_equal 'ruby', OpenAnswers.example_language("```ruby\ndef valid_isbn?(isbn)\nend\n```\n")
        assert_equal 'python', OpenAnswers.example_language("  ```python\ndef f():\n    pass\n```")
        assert_equal 'javascript', OpenAnswers.example_language("```js\nlet a = 1;\n```")
        # text, two blocks, or a language no page switches to: for every language
        assert_nil OpenAnswers.example_language("Die Kugeln spiegeln nichts.")
        assert_nil OpenAnswers.example_language("```ruby\na\n```\n\n```python\nb\n```")
        assert_nil OpenAnswers.example_language("```sql\nSELECT 1;\n```")
        assert_nil OpenAnswers.example_language("Erst Text, dann\n```ruby\na\n```")
    end
end
