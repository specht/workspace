require 'digest'

# What the Workspace remembers about a task (docs/aufgaben.md, »Was
# gespeichert wird«), as plain Ruby so it can be tested without Neo4j.
#
# In Neo4j:
#   (:User)-[:HAS_TASK_STATE]->(:TaskState {key, task, version, status,
#       last_answer, attempts, first_status, first_correct, updated})
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
    def self.next_state(previous, task:, version:, status:, answer:, now:, max: MAX_ANSWER)
        prev = previous || {}
        same = prev[:version] == version
        first_correct = same ? prev[:first_correct] : nil
        first_correct ||= now if status == 'correct'
        {
            :task => task,
            :version => version,
            :status => status,
            :last_answer => answer.to_s[0, max],
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
