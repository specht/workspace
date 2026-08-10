# Workspace TODO

Ideas, missing tutorials, completed infrastructure and follow-up work for https://workspace.hackschule.de/.


## Recently Completed

### Shared Live Apps ✅

Implemented in August 2026.

Students can share a running browser-based application from their workspace with other authenticated Workspace users without publishing it publicly. This is now useful infrastructure for Web Server, WebSocket, BIF, Anaglyph, Flutter and multiplayer/collaborative projects.

* [x] Detect TCP services listening in the student's workspace
* [x] Only consider listeners owned by the workspace user; ignore root-owned/container-internal services
* [x] Keep the workspace's own internal server port inaccessible
* [x] Show the port and owning command for the student's running applications
* [x] Keep all ports private by default
* [x] Let the student explicitly share or unshare an individual port
* [x] Use the port number as the app label; do not require a user-supplied title
* [x] Show the student's full name and port for shared apps
* [x] Show apps from the current user's group first, followed by a small separator and apps from other groups
* [x] Require a valid Workspace login to open shared applications
* [x] Support normal HTTP applications and WebSocket applications
* [x] Keep VS Code, terminal, files and every non-shared port inaccessible
* [x] Use a stable share URL for each user/port pair
* [x] Reuse that stable URL when the user explicitly shares the same port again
* [x] Bind an active share to the actual listening socket, not merely the port number
* [x] Automatically deactivate the share when the server stops or a new process takes over the same port
* [x] Keep a workspace container running while it has an active shared app
* [x] Update the profile page live through WebSockets when ports or shares change
* [x] Use POST actions for sharing and revoking
* [x] Handle stopped workspaces and stale/offline shares cleanly

Useful classroom scenarios:

* Share a small HTTP server with a classmate
* Peer-test BIF / Interactive Fiction stories before publishing
* Share Anaglyph scenes before publishing
* Run collaborative WebSocket exercises
* Run multiplayer browser games
* Share student-built HTTP APIs
* Let groups test each other's browser applications

Important principle:

> A port becoming available does not make it public. A student must explicitly share the currently running application.

Follow-up work:

* [ ] Add E2E coverage for share/unshare, authenticated access, WebSocket proxying and automatic unsharing
* [ ] Mention Shared Live Apps explicitly in the tutorials where it is useful

---


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

A good final step is to run the finished server in the workspace, share its port through **Shared Live Apps**, and have another student open it.

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

**Shared Live Apps** means the final project can actually be used by other students in the class without first publishing it publicly.

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

Use **Shared Live Apps** for peer testing before the finished story is published.

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

Use **Shared Live Apps** so students can show the running Flutter Web application to each other directly from their workspace.

Important conceptual comparison:

```text
HTML/JavaScript:
Find an element and change it.

Flutter:
Change the state and rebuild the interface.
```

---

### Static website generators

We have Hugo in the Docker image, write a tutorial for that.

---

## Existing Tools / Discoverability

### Automatron / Formal Languages

The Workspace already contains the Automatron page for deterministic finite automata and related formal-language exercises.

For now, keep this as a **contextual teaching tool rather than a permanent main-navigation item**. Students normally need it only when the topic is being taught.

* [ ] Link Automatron directly from relevant automata / formal-languages teaching material
* [ ] Mention the direct URL in teacher-facing lesson notes so it is easy to hand out
* [ ] Add a short description/example if students are expected to use it without teacher explanation
* [ ] If more specialist standalone tools accumulate, consider a small **Werkzeuge** overview page instead of adding every tool to the primary navigation

---

## Possible Later Additions

### Automated Testing

* [ ] Unit tests
* [ ] Assertions
* [ ] Intentionally introduce bugs
* [ ] Browser testing with Playwright
* [ ] Connect automated tests with existing tutorials

### raylib + WebAssembly

Use raylib as a simple code-first environment for graphical programs and small games in C.

The final result should run in the browser through WebAssembly so students can publish their programs like other Hackschule web projects.

* [ ] Install/test a minimal raylib toolchain in the workspace
* [ ] Create a window/canvas and draw basic shapes
* [ ] Handle keyboard and mouse input
* [ ] Add a simple game loop
* [ ] Build a small graphical game or simulation
* [ ] Compile the same C program to WebAssembly with Emscripten
* [ ] Explain at a high level what the generated `.wasm` file is
* [ ] Run the WebAssembly build in the browser
* [ ] Publish the finished browser build on a Hackschule subdomain
* [ ] Keep the build process understandable and avoid unnecessary framework/tooling magic

Possible projects:

* Pong
* Breakout
* Snake
* Asteroids
* Particle simulation
* Cellular automaton

This can serve as the main WebAssembly introduction rather than having a separate WASM tutorial.

### PixelRAM Graphics

After Pixelflow Canvas with Ruby, let students program highly performant low level framebuffer graphics via WASM.

Old school graphics effects, Game of life, Mandelbrot set, etc.

Also: doomgeneric + chocolatedescent

### Digital Audio

Introduce sound synthesis and audio programming in the browser.

A browser-based approach keeps the processing on the student's machine and avoids additional server infrastructure.

* [ ] Start with a single oscillator and a 440 Hz tone
* [ ] Compare sine, square, triangle and sawtooth waves
* [ ] Change frequency and connect it to pitch
* [ ] Control volume/amplitude
* [ ] Introduce simple envelopes
* [ ] Mix several oscillators
* [ ] Visualize a waveform or frequency spectrum
* [ ] Build a tiny synthesizer, sequencer or drum machine
* [ ] Evaluate libraries students are interested in where they add something useful

Practical requirement:

* Students need headphones for classroom use.

### Prolog

Add a small introduction to logic programming using SWI-Prolog in the terminal.

The goal is to expose students to a genuinely different programming paradigm rather than add another conventional language.

* [ ] Introduce facts
* [ ] Introduce rules
* [ ] Ask queries
* [ ] Show variables and pattern matching
* [ ] Explore backtracking
* [ ] Work with recursive rules
* [ ] Build a small knowledge base
* [ ] Use Prolog to solve a search or logic problem

Possible projects:

* Murder mystery
* Route finding
* Family or mythology relationships
* Logic puzzle
* Simple scheduling problem

Avoid making the first tutorial about Prolog syntax alone; the interesting part is that the system searches for solutions from facts and rules.

### WebGPU

Add an advanced browser-based introduction to GPU and massively parallel computing.

The main focus should be compute rather than another graphics API tutorial.

* [ ] Verify the WebGPU setup on the classroom machines
* [ ] Introduce the idea of many computations running in parallel
* [ ] Write the smallest useful WGSL compute shader
* [ ] Pass data from JavaScript to the GPU
* [ ] Read or visualize the computed result
* [ ] Compare a CPU implementation with a GPU implementation
* [ ] Run a large particle simulation or cellular automaton
* [ ] Discuss why GPUs are well suited to highly parallel workloads

Possible projects:

* 100,000-particle simulation
* Conway's Game of Life
* Reaction-diffusion
* Image filters
* Mandelbrot/fractal computation

The conceptual goal is:

> CPU: a few powerful workers
> GPU: many small workers doing similar jobs at the same time

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

**Shared Live Apps is already implemented infrastructure, not another tutorial to schedule.** Use it throughout the Web Server, WebSockets, BIF and Flutter material whenever students should be able to try each other's running projects.