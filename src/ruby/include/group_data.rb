# What hangs on a group's name besides the group itself: its settings
# (GroupSetting: programming language, class repositories) and the
# activities pinned for it (Pin). They go with the group, so that a new
# group of the same name never inherits them (docs/teacher-access.md).
#
#   - the delete button (/api/groups/delete) drops them with the group;
#   - a group that is gone from the roster when it is read again (an
#     invitation file changed) loses them too (Main.load_invitations);
#   - before a group is created in the browser, leftovers of that name go;
#   - the import of the invitation files keeps them for the groups it
#     imports (the same groups, now in Neo4j) and drops the others.
#
# db is anything with neo4j_query(query, params) ($neo4j, or a test double).
module GroupData
    DROP = <<~END_OF_QUERY
        MATCH (n) WHERE (n:GroupSetting OR n:Pin) AND n.group IN $names
        DETACH DELETE n;
    END_OF_QUERY
    DROP_ALL_BUT = <<~END_OF_QUERY
        MATCH (n) WHERE (n:GroupSetting OR n:Pin) AND NOT n.group IN $keep
        DETACH DELETE n;
    END_OF_QUERY

    # The groups of the roster before that are not in it after. Nothing
    # when there was no roster before (the first reading) or none is left
    # after: a roster without any group was not read, it did not lose them.
    def self.vanished(before, after)
        before = before.to_a
        after = after.to_a
        return [] if before.empty? || after.empty?

        before - after
    end

    # Drops the settings and pins of these groups.
    def self.drop!(db, names)
        names = Array(names).map(&:to_s).reject(&:empty?).uniq
        return if names.empty?

        db.neo4j_query(DROP, :names => names)
    end

    # Drops the settings and pins of every name not in keep (the import).
    def self.drop_all_but!(db, keep)
        keep = Array(keep).map(&:to_s).uniq
        return if keep.empty?

        db.neo4j_query(DROP_ALL_BUT, :keep => keep)
    end
end
