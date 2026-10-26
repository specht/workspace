#!/bin/bash
# Gives back the space of the transaction logs of the user Neo4j (one
# database per person). Until October 2026 every database reserved 256 MB of
# log in advance; with db.tx_log.preallocate=false and a rotation every
# 16 MB (config.rb) new logs start small, but the reserved files stay, and a
# dump keeps them too. So each database is copied (neo4j-admin database
# copy: a new store with fresh, small logs), the copy is loaded under the
# database's name, and its indexes and constraints are made again (the copy
# writes them as Cypher). Before, the original is dumped; if the copy does
# not have as many nodes, relationships, indexes and constraints as the
# original, the original goes back. The system database is left as it is.
#
# Run on the host, in the installation's folder, while the service runs and
# after ./config.rb up -d neo4j has started it with the new settings:
#
#   src/scripts/neo4j-user-compact-logs.sh            every database
#   src/scripts/neo4j-user-compact-logs.sh NAME …     only these
#
# A database is offline for the seconds its turn takes; best when no class
# works with Neo4j.
set -euo pipefail
cd "$(dirname "$0")/../.."

./config.rb exec -T neo4j bash -s -- "$@" <<'INSIDE'
set -uo pipefail
password=${NEO4J_AUTH#neo4j/}
conf=/var/lib/neo4j/conf/neo4j.conf
work=/data/compact-work
query() { cypher-shell -u neo4j -p "$password" -d "$1" --format plain "$2"; }
# nodes/relationships/indexes and constraints of a database
counts() {
    echo "$(query "$1" 'MATCH (n) RETURN count(n)' 2>/dev/null | tail -1)/$(query "$1" 'MATCH ()-[r]->() RETURN count(r)' 2>/dev/null | tail -1)/$(query "$1" 'SHOW INDEXES YIELD name RETURN count(*)' 2>/dev/null | tail -1)/$(query "$1" 'SHOW CONSTRAINTS YIELD name RETURN count(*)' 2>/dev/null | tail -1)"
}
if [ $# -gt 0 ]; then
    databases=("$@")
else
    mapfile -t databases < <(query system "SHOW DATABASES YIELD name WHERE name <> 'system' RETURN DISTINCT name" | tail -n +2 | tr -d '"')
fi
for db in "${databases[@]}"; do
    tmp="compact-$RANDOM$RANDOM"
    rm -rf "$work" && mkdir -p "$work/original" "$work/copy"
    before_size=$(du -sh "/data/transactions/$db" 2>/dev/null | cut -f1)
    before=$(counts "$db")
    query system "STOP DATABASE \`$db\` WAIT" >/dev/null
    status=ok
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
        # indexes and constraints, as the copy wrote them
        schema=$(ls "$work"/*schema*.cypher 2>/dev/null | head -1)
        [ -n "$schema" ] && [ -s "$schema" ] && cypher-shell -u neo4j -p "$password" -d "$db" -f "$schema" >/dev/null 2>&1
        after=$(counts "$db")
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
    echo "$db: $before_size -> $(du -sh "/data/transactions/$db" 2>/dev/null | cut -f1), nodes/relationships/indexes/constraints $before ($status)"
done
INSIDE
