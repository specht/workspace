# The classes within a group, for the dialog of who takes part in an exam
# (exams.js, open_exam_people): a course such as »Informatik LK« holds
# students from several classes, and choosing one class for an exam is
# easier when they stand apart. The roster does not know what a class is;
# what makes one is that everyone is in exactly one. So the classes are the
# other groups that do not overlap among this group's members and together
# hold the most of them (fewer groups and then names on a tie): a course a
# few of them share besides their classes overlaps those and is left out.
# Only groups that hold at least two of the members count (a class of one
# goes under »Weitere«), and only those of the group's school (the caller
# leaves the others out): the dialog shows a teacher the names of groups
# that are not theirs, so as few as needed.
# Nothing when that gives fewer than two classes.
module ExamPeopleClasses
    # at most this many groups are weighed, the largest (the search tries
    # every choice of them that does not overlap)
    MAX_CANDIDATES = 20

    # members: the group's emails; group_members: every group => emails;
    # skip: groups that are no class (test groups, »Lehrkräfte«). Returns
    # { email => class name or nil (in none of them) }, or {}.
    def self.split(group, members, group_members, skip: [])
        in_group = members.to_set
        candidates = group_members.filter_map do |name, emails|
            next if name == group || skip.include?(name)

            shared = emails.select { |email| in_group.include?(email) }.to_set
            [name, shared] if shared.size >= 2 && shared.size < in_group.size
        end
        candidates = candidates.sort_by { |name, shared| [-shared.size, name] }.first(MAX_CANDIDATES)
        chosen = best_disjoint(candidates)
        return {} if chosen.size < 2

        members.to_h { |email| [email, chosen.find { |_, shared| shared.include?(email) }&.first] }
    end

    # the choice of candidates, pairwise disjoint, that covers the most;
    # on a tie the one with fewer groups, found first in the order given
    def self.best_disjoint(candidates)
        best = []
        best_cover = 0
        rest = candidates.map { |_, shared| shared.size }.reverse.each_with_object([0]) { |n, sums| sums.unshift(sums.first + n) }
        search = lambda do |index, chosen, used, cover|
            if cover > best_cover || (cover == best_cover && chosen.size < best.size)
                best = chosen.dup
                best_cover = cover
            end
            return if index >= candidates.size || cover + rest[index] <= best_cover

            name, shared = candidates[index]
            if shared.none? { |email| used.include?(email) }
                chosen << [name, shared]
                search.call(index + 1, chosen, used | shared, cover + shared.size)
                chosen.pop
            end
            search.call(index + 1, chosen, used, cover)
        end
        search.call(0, [], Set.new, 0)
        best
    end
end
