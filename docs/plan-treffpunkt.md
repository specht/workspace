# A meeting point in an activity

Classes work through an activity at very different speeds. A teacher sets
one meeting point in an activity for a group: the place where the class
meets for a Zwischensicherung before it goes on. Students see on the page
where it is and what to do once they are there; the teacher sees live on
the class page how many, and who, have arrived. A plan, 28 October 2026.

## Decided by the maintainer (28 October 2026)

- **Soft, not a stop.** Nothing on the page is hidden or held back: every
  page stays readable in full, also in a private window without an account.
  The meeting point is a barrier that says »here we will talk«, not a lock.
- **Something to do for those who are early:** the box names the page's
  Extras, the next Kata set, and helping a classmate.
- **One meeting point** per activity and group. The teacher sets, moves and
  lifts it live; everyone with the page open sees the change without a
  reload (the websocket the class page uses already).
- **A student in two groups** (rare) sees the earlier of the two points.
- **Lifted is lifted:** once the teacher lifts it, it is gone, for absent
  students too.

## Open: at a task or at a chapter

The maintainer is unsure. Proposed: **after a task.**

- »Arrived« must be countable, and what the Workspace counts is tasks: a
  student has arrived when every task before the point that is not an
  Extra is solved. At a chapter, the count is the same (the tasks before
  the chapter), so the difference is only where the teacher clicks and
  where the barrier shows.
- On the class board (»Fortschritt«) the columns are tasks; a line between
  two columns is where a teacher sees the class anyway. A chapter is a
  group of columns, and a point »before chapter 5« is the line after the
  last task of chapter 4.
- On the page the barrier shows right after the task's card. If that task
  ends its chapter, it shows before the next chapter's heading, which is
  where a chapter-based point would be.
- A chapter without tasks (an introduction, »Zum Nachschlagen«) cannot hold
  a point after a task; it never needs one either.

## How it could be built

- **Stored:** in the group's settings (`GroupSetting`), per activity the
  task id after which the point is, or nothing. No data about students;
  arrival is computed from the task states the class board already reads.
  `privacy.rb` needs no change (it is a setting of the group, not about a
  person); to be confirmed when it is built.
- **Teacher, on the class board:** a control between two task columns
  (»Treffpunkt hier«), the line with »18 von 24 da«, a click showing who,
  and »Treffpunkt aufheben«. Moving it is setting it elsewhere.
- **Students, on the page:** after the task, a box across the width: »Hier
  trifft sich deine Klasse«, what comes next is talked about together, and
  what to do meanwhile (Extras on this page, the next Kata set, helping a
  classmate). Before they have arrived the box is quiet (a line with the
  heading); once they have, it says they are there.
- **Live:** setting, moving and lifting reach every open page of the group
  over the websocket; a page without a connection shows the state of its
  last load.
- **Tests:** a Ruby test for »arrived« (Extras do not count, the earlier of
  two groups), a Playwright test: the teacher sets the point, a student's
  open page shows it without a reload, solving the last task before it
  moves the student into »da«, lifting removes it.
