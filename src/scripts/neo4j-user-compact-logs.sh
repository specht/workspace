#!/bin/bash
# Gives back the space of the transaction logs of the user Neo4j (one
# database per person). Until October 2026 every database reserved 256 MB of
# log in advance; with db.tx_log.preallocate=false and a rotation every
# 16 MB (config.rb) new logs start small, but the reserved files stay, and a
# dump keeps them too.
#
# Most of these databases are empty. An empty one (no nodes, relationships,
# constraints, no indexes but the two that every database has) is dropped
# and created again under its name, which takes a second; its grants go with
# it, so they are read before (SHOW PRIVILEGES AS COMMANDS) and given again,
# and afterwards compared. Ten at a time: counted, then made anew, so that
# little time lies between the count and the drop.
#
# A database with data is copied (neo4j-admin database copy: a new store with
# fresh, small logs), the copy loaded under the database's name, and its
# indexes and constraints made again (the copy writes them as Cypher).
# Before, the original is dumped; if the copy does not have as many nodes,
# relationships, indexes and constraints as the original, the original goes
# back. The system database is left as it is.
#
# Run on the host, in the installation's folder, while the service runs and
# after ./config.rb up -d neo4j has started it with the new settings:
#
#   src/scripts/neo4j-user-compact-logs.sh                every database whose log is over 32 MB
#   src/scripts/neo4j-user-compact-logs.sh NAME …         only these, whatever their size
#   src/scripts/neo4j-user-compact-logs.sh --stop-other   first end a run that still goes on
#
# So a second run only does what is left. A database is offline for the
# seconds its turn takes; best when no class works with Neo4j. Only one run
# at a time: while another one goes on, it stops at once, or with
# --stop-other ends the other one at a point where it has changed nothing.
set -euo pipefail
cd "$(dirname "$0")/../.."

./config.rb exec -T neo4j bash -s -- "$@" <<'INSIDE'
set -uo pipefail
password=${NEO4J_AUTH#neo4j/}
conf=/var/lib/neo4j/conf/neo4j.conf
work=/data/compact-work
lock=/data/compact-lock
shell() { cypher-shell -u neo4j -p "$password" --format plain "$@"; }
query() { shell -d "$1" "$2"; }

# Only one run at a time. Another one is found by its loop: a »bash -s --«
# in a session other than this one (its own subshells share this one); that
# also finds a run of the earlier version of this script, which has no lock
# and which Ctrl-C on the host does not end (it goes on in the container).
stop_other=false
if [ "${1:-}" = --stop-other ]; then stop_other=true; shift; fi
# a field of /proc/PID/stat after the command's name: 2 the parent, 4 the session
# (processes come and go while they are read: then nothing)
field() { local stat n=$2; { read -r stat < "/proc/$1/stat"; } 2>/dev/null && set -- ${stat#*) } && echo "${!n}"; }
cmdline() { { tr '\0' ' ' < "/proc/$1/cmdline"; } 2>/dev/null; }
mine=$(field $$ 4)
other_loops() {
    local f pid
    for f in /proc/[0-9]*/cmdline; do
        pid=${f#/proc/}; pid=${pid%/cmdline}
        [ "$(field "$pid" 4)" = "$mine" ] && continue
        case "$(cmdline "$pid")" in 'bash -s -- '*) echo "$pid" ;; esac
    done
}
admin_running() {
    for f in /proc/[0-9]*/cmdline; do { tr '\0' ' ' < "$f"; } 2>/dev/null; echo; done | grep -q ' database \(dump\|copy\|load\) '
}
if [ -d "$lock" ]; then
    echo "Another run of this script is going on ($lock is there). If none runs any more, remove data/neo4j_user/compact-lock."
    exit 1
fi
loops=$(other_loops)
if [ -n "$loops" ] && ! $stop_other; then
    echo "An earlier run is still going on in the container (Ctrl-C on the host does not end it)."
    echo "Let it end, or start this one with --stop-other: it ends the other one where it only counts a database."
    exit 1
fi
if [ -n "$loops" ]; then
    # The earlier version counts a database (cypher-shell … count) with its
    # working folder still empty before it stops it, and again after loading
    # it, with the dump in that folder. Ended in the first count, it has
    # changed nothing; the database it counted is started in case.
    echo "Waiting for the other run to begin counting its next database …"
    stopped=''
    for _ in $(seq 1 6000); do
        loops=$(other_loops)
        [ -z "$loops" ] && break
        for f in /proc/[0-9]*/cmdline; do
            pid=${f#/proc/}; pid=${pid%/cmdline}
            printf '%s\n' "$loops" | grep -qxF "$(field "$pid" 2)" || continue
            case "$(cmdline "$pid")" in *' RETURN count('*) ;; *) continue ;; esac
            [ -d "$work/original" ] && [ -z "$(ls -A "$work/original")" ] || continue
            stopped=$(cmdline "$pid" | grep -o -- ' -d [^ ]*' | head -1 | cut -c5-)
            kill -KILL $loops 2>/dev/null
            kill -KILL "$pid" 2>/dev/null
            break 2
        done
        sleep 0.3
    done
    if [ -n "$(other_loops)" ]; then
        echo "The other run did not reach a point where it can be ended safely; nothing changed. Try again later."
        exit 1
    fi
    if [ -n "$stopped" ]; then
        query system "START DATABASE \`$stopped\` WAIT" >/dev/null 2>&1
        rm -rf "$work"
        echo "Ended the other run while it counted $stopped (left as it was)."
    else
        echo "The other run has ended by itself."
    fi
fi
if [ -e "$work" ] || admin_running || ! mkdir "$lock" 2>/dev/null; then
    echo "A run was interrupted ($work is left) or neo4j-admin still works on a database. Look at SHOW DATABASES;"
    echo "if everything is online and nothing runs any more, remove data/neo4j_user/compact-work and try again."
    exit 1
fi
trap 'rmdir "$lock" 2>/dev/null' EXIT

size_mb() { du -sm "/data/transactions/$1" 2>/dev/null | cut -f1; }
size() { du -sh "/data/transactions/$1" 2>/dev/null | cut -f1; }

if [ $# -gt 0 ]; then
    databases=("$@")
else
    mapfile -t all < <(query system "SHOW DATABASES YIELD name WHERE name <> 'system' AND NOT name STARTS WITH 'compact-' RETURN DISTINCT name" | tail -n +2 | tr -d '"')
    databases=()
    for db in "${all[@]}"; do
        [ "$(size_mb "$db")" -gt 32 ] 2>/dev/null && databases+=("$db")
    done
    echo "${#all[@]} databases, ${#databases[@]} with a log over 32 MB."
fi
total_before=$(du -sm /data/transactions | cut -f1)

# Lines »db nodes rels indexes lookup constraints« for the given databases
# that are online, with one cypher-shell; a database it cannot read is
# missing. Each one first says which database it reads (»is«: where :use
# fails, the shell would stay in the one before) and ends with »done«; the
# shell stops at the first error, so a database with both was read
# completely. SHOW … count(*) gives no row where there is nothing: that is 0.
count_all() {
    local file online
    file=$(mktemp)
    online=$(query system "SHOW DATABASES YIELD name, currentStatus WHERE currentStatus = 'online' RETURN name" | tail -n +2 | tr -d '"')
    for db in "$@"; do
        printf '%s\n' "$online" | grep -qxF "$db" || continue
        cat >> "$file" <<END
:use \`$db\`
CALL db.info() YIELD name RETURN "$db" AS db, "is" AS what, name AS c;
MATCH (n) RETURN "$db" AS db, "nodes" AS what, count(n) AS c;
MATCH ()-[r]->() RETURN "$db" AS db, "rels" AS what, count(r) AS c;
SHOW INDEXES YIELD type RETURN "$db" AS db, "indexes" AS what, count(*) AS c;
SHOW INDEXES YIELD type WHERE type = "LOOKUP" RETURN "$db" AS db, "lookup" AS what, count(*) AS c;
SHOW CONSTRAINTS YIELD name RETURN "$db" AS db, "constraints" AS what, count(*) AS c;
RETURN "$db" AS db, "done" AS what, 1 AS c;
END
    done
    [ -s "$file" ] && shell -d system -f "$file" 2>/dev/null | awk -F', ' '
        $1 ~ /^"/ { gsub(/"/, "", $1); gsub(/"/, "", $2); gsub(/"/, "", $3); v[$1, $2] = $3; seen[$1] = 1 }
        END { for (db in seen) if (v[db, "is"] == db && v[db, "done"] == 1) print db, v[db, "nodes"] + 0, v[db, "rels"] + 0, v[db, "indexes"] + 0, v[db, "lookup"] + 0, v[db, "constraints"] + 0 }'
    rm -f "$file"
}

# The grants that name a database, one statement per line.
grants_of() {
    printf '%s\n' "$privileges" | grep -F "\`$1\`" | sed 's/^"//; s/"$//; s/$/;/'
}

# A database with data: copied, loaded, counted against the original.
compact_by_copy() {
    local db=$1 before=$2 tmp="compact-$RANDOM$RANDOM" status=ok schema after before_size
    before_size=$(size "$db")
    rm -rf "$work" && mkdir -p "$work/original" "$work/copy"
    query system "STOP DATABASE \`$db\` WAIT" >/dev/null
    if ! neo4j-admin database dump "$db" --to-path="$work/original" >/dev/null 2>&1; then
        status='dump of the original failed, left as it was'
    elif ! neo4j-admin database copy "$db" "$tmp" --to-path-schema="$work" --additional-config="$conf" >/dev/null 2>&1 ||
        ! neo4j-admin database dump "$tmp" --to-path="$work/copy" >/dev/null 2>&1 ||
        ! mv "$work/copy/$tmp.dump" "$work/copy/$db.dump" ||
        ! neo4j-admin database load "$db" --from-path="$work/copy" --overwrite-destination=true --additional-config="$conf" >/dev/null 2>&1; then
        status='copy failed, original loaded again'
        neo4j-admin database load "$db" --from-path="$work/original" --overwrite-destination=true >/dev/null 2>&1 || status='copy AND reload failed: see the dump in data/neo4j_user/compact-keep'
    fi
    query system "START DATABASE \`$db\` WAIT" >/dev/null
    if [ "$status" = ok ]; then
        # indexes and constraints, as the copy wrote them; its file begins
        # with :use of the copy's own name, which is gone again
        schema=$(ls "$work"/*schema*.cypher 2>/dev/null | head -1)
        [ -n "$schema" ] && sed -i '/^:use /d' "$schema"
        [ -n "$schema" ] && [ -s "$schema" ] && shell -d "$db" -f "$schema" >/dev/null 2>&1
        after=$(count_all "$db" | cut -d' ' -f2-)
        if [ "$after" != "$before" ]; then
            query system "STOP DATABASE \`$db\` WAIT" >/dev/null
            neo4j-admin database load "$db" --from-path="$work/original" --overwrite-destination=true >/dev/null 2>&1 &&
                status="counts differed ($before vs $after), original loaded again" ||
                status='counts differed AND reload failed: see the dump in data/neo4j_user/compact-keep'
            query system "START DATABASE \`$db\` WAIT" >/dev/null
        fi
    fi
    case "$status" in *'see the dump'*) mkdir -p /data/compact-keep && cp "$work/original/$db.dump" /data/compact-keep/ ;; esac
    rm -rf "/data/databases/$tmp" "/data/transactions/$tmp" "$work"
    echo "$db: $before_size -> $(size "$db"), copied, nodes/relationships/indexes/lookup/constraints $before ($status)"
}

i=0
while [ $i -lt ${#databases[@]} ]; do
    chunk=("${databases[@]:$i:10}")
    i=$((i + 10))
    declare -A counted=()
    while read -r db rest; do counted[$db]=$rest; done < <(count_all "${chunk[@]}")
    privileges=$(query system 'SHOW PRIVILEGES AS COMMANDS YIELD command RETURN command' | tail -n +2)
    empty=()
    for db in "${chunk[@]}"; do
        if [ -z "${counted[$db]:-}" ]; then
            echo "$db: not online or not readable, left as it was"
            continue
        fi
        read -r nodes rels indexes lookup constraints <<< "${counted[$db]}"
        if [ "$nodes" = 0 ] && [ "$rels" = 0 ] && [ "$constraints" = 0 ] && [ "$indexes" = "$lookup" ]; then
            empty+=("$db")
        else
            compact_by_copy "$db" "${counted[$db]}"
        fi
    done
    [ ${#empty[@]} -eq 0 ] && { unset counted; continue; }

    # The empty ones: dropped and created again, their grants given back.
    declare -A before_size=() wanted=()
    file=$(mktemp)
    for db in "${empty[@]}"; do
        before_size[$db]=$(size "$db")
        wanted[$db]=$(grants_of "$db" | sort)
        { echo "DROP DATABASE \`$db\` IF EXISTS WAIT;"; echo "CREATE DATABASE \`$db\` IF NOT EXISTS WAIT;"; [ -n "${wanted[$db]}" ] && echo "${wanted[$db]}"; } >> "$file"
    done
    shell -d system --fail-at-end -f "$file" >/dev/null 2>&1
    # Whatever did not go through once more (all of it may run twice), then compared.
    : > "$file"
    for db in "${empty[@]}"; do
        { echo "CREATE DATABASE \`$db\` IF NOT EXISTS WAIT;"; [ -n "${wanted[$db]}" ] && echo "${wanted[$db]}"; } >> "$file"
    done
    shell -d system --fail-at-end -f "$file" >/dev/null 2>&1
    rm -f "$file"
    privileges=$(query system 'SHOW PRIVILEGES AS COMMANDS YIELD command RETURN command' | tail -n +2)
    online=$(query system "SHOW DATABASES YIELD name, currentStatus WHERE currentStatus = 'online' RETURN name" | tail -n +2 | tr -d '"')
    for db in "${empty[@]}"; do
        status=ok
        printf '%s\n' "$online" | grep -qxF "$db" || status='NOT ONLINE: check with SHOW DATABASES'
        [ "$(grants_of "$db" | sort)" = "${wanted[$db]}" ] || status="$status; GRANTS DIFFER: compare SHOW PRIVILEGES for $db"
        echo "$db: ${before_size[$db]} -> $(size "$db"), empty, made anew with $(printf '%s' "${wanted[$db]}" | grep -c .) grants ($status)"
    done
    unset counted before_size wanted
done
echo "Transaction logs: ${total_before} MB -> $(du -sm /data/transactions | cut -f1) MB"
INSIDE
