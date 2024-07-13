# Hackschule Workspace

- login with invitation only
- user node gets created on first login

- `fs_tag`: derived from email
  - used for user data directory
  - never published
- `server_tag`:
  - random tag
  - stored in user node
  - used in url
  - created on user creation
  - changed on server reset for cache busting
  - stored in browser cookie
    - if change is detected => purge indexeddb
- `server_sid`:
  - random tag
  - stored in user node
  - sent to user on login
  - must be sent to already logged in user if not present
- `share_tag`:
  - pretty long, random tag
  - stored in user node
  - max. 1 per user
  - can be removed again (stop sharing)
  - used to share access with other user
  - used in nginx config (requires no sid)

tic-80
mysql
Ruby
Neo4j

DBML:

nicolas-liger.dbml-viewer
bocovo.dbml-erd-visualizer

