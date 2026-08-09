# Workspace TODO

Ideas and missing tutorials for https://workspace.hackschule.de/.

## High Priority

### Web Server

Build a small web server from scratch as a continuation of the TCP/IP tutorial.

* [ ] Start with a manual HTTP response using `nc`
* [ ] Inspect the request sent by the browser
* [ ] Explain the HTTP request/response format
* [ ] Write a minimal server using raw TCP sockets
* [ ] Handle `GET /`
* [ ] Add multiple routes
* [ ] Return `404 Not Found`
* [ ] Serve HTML
* [ ] Serve static files
* [ ] Read query parameters
* [ ] Optionally handle a simple `POST` request
* [ ] Connect the tutorial explicitly to the existing TCP/IP tutorial

Main idea:

> TCP → HTTP → browser → web server

Avoid hiding the important parts behind a web framework.

---

### WebSockets

Create a follow-up to the web-server tutorial showing persistent, bidirectional communication.

* [ ] Show why normal HTTP request/response is insufficient
* [ ] Inspect the WebSocket handshake
* [ ] Establish a WebSocket connection
* [ ] Send messages browser → server
* [ ] Send messages server → browser
* [ ] Support multiple connected browsers
* [ ] Build a small multiplayer/collaborative project

Possible projects:

* Shared pixel canvas
* Multiplayer Pong
* Collaborative cursors
* Classroom voting
* Tiny multiplayer world

Prefer something more interesting than a basic chat application.

---

### Cryptography in the Terminal

Keep this tutorial terminal-based and use real Unix tools wherever possible.

#### Hashes

* [ ] Calculate a SHA-256 hash with `sha256sum`
* [ ] Modify one byte and compare hashes
* [ ] Explain the avalanche effect
* [ ] Explain what hashes can and cannot prove

#### Passwords

* [ ] Explain why passwords should not be stored directly
* [ ] Introduce salts
* [ ] Demonstrate password hashing

#### Symmetric Encryption

* [ ] Encrypt a file
* [ ] Decrypt it again
* [ ] Explain the role of the shared secret

#### Public-Key Cryptography

* [ ] Generate a key pair
* [ ] Explain public vs. private keys
* [ ] Encrypt/sign something
* [ ] Verify a signature
* [ ] Modify the signed file and see verification fail

#### SSH Connection

* [ ] Generate an Ed25519 SSH key with `ssh-keygen`
* [ ] Inspect the public key
* [ ] Explain what GitHub/server SSH authentication actually does
* [ ] Connect this with the existing Git material

Possible tools:

* `sha256sum`
* `openssl`
* `ssh-keygen`
* `gpg`

Keep the mathematics optional at first. Start with observable behaviour.

---

### Neo4j

Neo4j is already available in the workspace but the tutorial is missing.

* [ ] Add Neo4j to the tutorial overview
* [ ] Introduce nodes and relationships immediately with Cypher
* [ ] Visualize the first graph
* [ ] Add properties and labels
* [ ] Query relationships
* [ ] Introduce paths
* [ ] Find shortest paths
* [ ] Explore variable-length paths
* [ ] Add aggregation
* [ ] Compare graph modelling with relational modelling / SQL

Possible dataset:

```text
(:Person)-[:ACTED_IN]->(:Movie)
(:Person)-[:DIRECTED]->(:Movie)
```

Possible questions:

* Which actors appeared together?
* Which movies connect two actors?
* How many steps apart are two people?
* What is the shortest path between two actors?
* Which actor has worked with the most directors?

The important goal is to show **why graph databases exist**, not merely teach Cypher syntax.

---

### BIF / Interactive Fiction

Add BIF as a first-class tutorial.

The tutorial should have a very low entry barrier and emphasize creative programming.

* [ ] Create a minimal first interactive story
* [ ] Add choices
* [ ] Link multiple scenes
* [ ] Introduce variables/state
* [ ] Add conditional choices
* [ ] Add inventory or flags
* [ ] Introduce reusable structures where appropriate
* [ ] Show the story as a graph
* [ ] Include a larger creative assignment

Possible final task:

> Create your own interactive story with at least 10 scenes, multiple endings and at least one state-dependent path.

Potential themes:

* Mystery at school
* Greek mythology
* Science fiction
* Escape room
* Time travel

---

### Flutter

Add a compact Flutter tutorial rather than a complete Flutter course.

Use Flutter Web so everything works inside the workspace.

* [ ] Create the smallest Flutter application
* [ ] Explain widgets
* [ ] Build a simple layout
* [ ] Handle button presses
* [ ] Introduce state
* [ ] Rebuild UI from state
* [ ] Work with lists
* [ ] Add sorting/filtering
* [ ] Add a small animation if useful
* [ ] Run the finished application in the browser

The existing sorting demo could be the basis for the tutorial.

Important conceptual comparison:

```text
HTML/JavaScript:
Find an element and change it.

Flutter:
Change the state and rebuild the interface.
```

---

### Shared Live Apps / Classroom Network

Allow students to explicitly share a running port with other authenticated Hackschule users.

This should turn the individual workspaces into a small classroom network for client/server, WebSocket and multiplayer projects.

* [ ] Detect student services listening on workspace ports
* [ ] Keep ports private by default
* [ ] Let a student share an individual port
* [ ] Let the student give the shared service a short title
* [ ] Show currently shared services from other students
* [ ] Show the owner, title and port
* [ ] Allow another student to open a shared web application directly
* [ ] Support WebSocket connections through the shared route
* [ ] Allow students' own programs to connect to another student's shared service
* [ ] Keep VS Code, terminal, files and all non-shared ports inaccessible
* [ ] Keep existing private workspace/session authorization unchanged
* [ ] Allow the owner to revoke a share even when the underlying server is offline
* [ ] Clearly distinguish persistent share metadata from whether the port is currently running
* [ ] Use POST/DELETE-style actions for creating and revoking shares rather than GET routes
* [ ] Handle stopped workspaces and stale/offline shares cleanly

Possible classroom uses:

* Shared pixel canvas
* Multiplayer games
* Chat servers
* Student-built APIs
* Client/server exercises between two groups
* Protocol implementation exercises
* Collaborative applications

A useful end goal would be that a student can share a service and another student can either open it in the browser or connect to it from their own workspace, for example:

```text
curl http://anna:8000/
nc ben 5000
```

Only explicitly shared ports should be reachable.

---

## Possible Later Additions

### Automated Testing

* [ ] Unit tests
* [ ] Assertions
* [ ] Intentionally introduce bugs
* [ ] Browser testing with Playwright
* [ ] Connect automated tests with existing tutorials

### WebAssembly

* [ ] Compile a tiny C program to WebAssembly
* [ ] Call it from JavaScript
* [ ] Pass numbers/data between JavaScript and WASM
* [ ] Use it for a visual computation
* [ ] Possible connection to JPEG/image-processing tutorials

### Git: Collaboration

Extend the existing Git tutorial.

* [ ] Branches
* [ ] Merge
* [ ] Merge conflicts
* [ ] Pull requests
* [ ] Working with another student
* [ ] Basic CI/testing

### Data Formats

Possible short tutorial or collection.

* [ ] Text vs. binary data
* [ ] CSV
* [ ] JSON
* [ ] Hex dumps
* [ ] Character encodings
* [ ] Connect to JPEG/GIF and network protocols

---

## Not Planned for Now

### Tiny Language Model

Interesting in principle, but currently not a priority.

Problems:

* Very limited processing power in student workspaces
* A genuinely tiny model produces disappointing results
* Students now compare any language model with modern commercial LLMs
* Using a pretrained model would hide much of what the tutorial is supposed to explain

Revisit if we find an approach where the limitations themselves become pedagogically useful.

---

## Suggested Order

1. [ ] Web Server
2. [ ] WebSockets
3. [ ] Neo4j
4. [ ] Cryptography
5. [ ] BIF / Interactive Fiction
6. [ ] Flutter

The first two should form a continuous sequence after the existing TCP/IP tutorial.