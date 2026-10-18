# Deleting an account with everything that belongs to it (Datenschutzerklärung,
# »Speicherdauer«; docs/datenschutz.md, Löschkonzept). Plain Ruby, tested on
# its own; Main#delete_account does the work.
#
# What belongs to a person:
# - the nodes in OWN_NODES, found by how they hang on the User node, and the
#   User node itself;
# - the folders of its Workspace and of the exams it took
#   (/user/<fs_tag of email + test tag>), and of a teacher's exam demos;
# - its MySQL database and login, the extra MySQL databases it made, its
#   Neo4j database, user and role;
# - its hand-ins under /internal/submissions.
#
# Exams (Test) a teacher owns go with the teacher; an exam a student took
# stays, only the student's relation to it goes (DETACH DELETE).
# account_deletion_test.rb checks that every label main.rb hangs on a User is
# either here or in NOT_OWNED.
module AccountDeletion
    OWN_NODES = {
        'Session' => '(n:Session)-[:FOR]->(u)',
        'LoginRequest' => '(n:LoginRequest)-[:FOR]->(u)',
        'QrLoginRequest' => '(n:QrLoginRequest)-[:APPROVED_BY]->(u)',
        'LookIn' => '(n:LookIn)-[:FOR]->(u)',
        'TaskState' => '(u)-[:HAS_TASK_STATE]->(n:TaskState)',
        'TaskAttempt' => '(n:TaskAttempt)-[:BY]->(u)',
        'Database' => '(u)-[:HAS]->(n:Database)',
        'LiveAppShare' => '(u)-[:SHARES_LIVE_APP]->(n:LiveAppShare)',
        'Test' => '(n:Test)-[:BELONGS_TO]->(u)',
    }.freeze

    # Linked to a User, but not the person's own: the exam a student takes
    # belongs to its teacher.
    NOT_OWNED = {
        'Test' => 'TAKES',
    }.freeze

    # One Cypher statement per kind of node, each with $email.
    def self.node_statements
        OWN_NODES.values.map do |pattern|
            "MATCH (u:User {email: $email}) MATCH #{pattern} DETACH DELETE n;"
        end + ['MATCH (u:User {email: $email}) DETACH DELETE u;']
    end

    # The address as typed, or nil when it cannot be an account: one line,
    # one @, nothing that would reach a shell or a query unescaped.
    def self.normalize_email(text)
        email = text.to_s.strip.downcase
        email.match?(/\A[^@\s'"`\\]+@[^@\s'"`\\]+\z/) ? email : nil
    end
end
