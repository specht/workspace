require 'digest'

# What the Workspace remembers about a task (docs/aufgaben.md, »Was
# gespeichert wird«), as plain Ruby so it can be tested without Neo4j.
#
# In Neo4j:
#   (:User)-[:HAS_TASK_STATE]->(:TaskState {key, task, version, status,
#       last_answer, first_answer, attempts, first_status, first_correct,
#       updated})
#   (:TaskAttempt {task, version, status, ts})-[:BY]->(:User)
#
# A TaskState is the latest state of one task for one person; attempts keep
# only the status (no answer text), for the class overview later. Both are
# visible to the student and the teacher only, and the student can delete
# them at any time.
module TaskStore
    MAX_ANSWER = 4000

    # Stable key of a person's state for one task. The e-mail address is
    # hashed so it does not appear in the key itself.
    def self.state_key(email, task_key)
        Digest::SHA256.hexdigest("#{email}\n#{task_key}")[0, 32]
    end

    # Properties of the new TaskState. previous is the stored node (a Hash
    # with symbol keys) or nil. When the check of a task has changed
    # (another version), the count starts again. max: the longest answer
    # kept (a Kata keeps its code per language, KataCheck.stored_answer).
    # prediction: the task is a prediction, whose first answer is kept as
    # well (first_answer), for the class board's count of what the class
    # predicted; only while the version stays, and never for a state that
    # had tries before it was kept (its first answer is not known).
    def self.next_state(previous, task:, version:, status:, answer:, now:, max: MAX_ANSWER, prediction: false)
        prev = previous || {}
        same = prev[:version] == version
        first_correct = same ? prev[:first_correct] : nil
        first_correct ||= now if status == 'correct'
        first_answer = if !prediction
            nil
        elsif same && prev[:attempts].to_i > 0
            prev[:first_answer]
        else
            answer.to_s[0, max]
        end
        {
            :task => task,
            :version => version,
            :status => status,
            :last_answer => answer.to_s[0, max],
            :first_answer => first_answer,
            :attempts => (same ? prev[:attempts].to_i : 0) + 1,
            :first_status => (same && prev[:first_status]) ? prev[:first_status] : status,
            :first_correct => first_correct,
            :updated => now,
        }
    end

    # What the page gets back for one task. stale: the task's check has
    # changed since the answer was given, so it should be checked again.
    def self.public_state(stored, current_version)
        stale = stored[:version] != current_version
        {
            :status => stale ? 'stale' : stored[:status],
            :answer => stored[:last_answer],
            :attempts => stored[:attempts].to_i,
            :updated => stored[:updated],
        }
    end
end
