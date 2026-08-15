# Workspace TODO

Open infrastructure, maintenance and teaching work for the Hackschule Workspace.

The TODO should describe **what is still worth doing**, not serve as a changelog. Completed work belongs in Git history and documentation.


## Infrastructure and Hardening

### Student network isolation

Student workspaces are isolated from each other by the peer firewall. Keep the policy deliberately narrow.

* [ ] Allow direct student-to-student connections only on **TCP port 1234**, matching the existing TCP/IP tutorial
* [ ] Remove the generic `40000-40999` peer range
* [ ] Do not allow UDP peer traffic unless a future tutorial has a concrete need for it
* [ ] Update the peer-network E2E test to enforce the final policy
* [ ] Mention the special role of port 1234 in the TCP/IP tutorial

Shared Live Apps remains the normal way for students to expose browser applications to other Workspace users.


### Resource limits for student workspaces

Student containers already have a CPU limit. Add generous memory and process limits so that a broken or intentionally hostile program affects one workspace rather than the whole server.

Do not guess the limits. Measure real student workloads first.

Representative workloads:

* C/C++ compilation
* PixelRAM / Emscripten builds
* Flutter Web builds
* Svelte / npm builds
* LaTeX compilation
* ordinary interpreted-language work

Then:

* [ ] Measure peak memory and process counts for representative workloads
* [ ] Choose limits with comfortable classroom headroom
* [ ] Add memory limits to student containers
* [ ] Add PID/process limits to student containers
* [ ] Verify the existing E2E suite under the chosen limits

The goal is containment, not maximizing the number of students per server.


### Authentication and sessions

* [ ] Harden the login and session flow
* [ ] Review authentication-related logging and error handling
* [ ] Add regression tests for the important login/session behaviour

Keep security-sensitive implementation details out of this public TODO.


### Operational logging

* [ ] Audit command/error logging so secrets and credentials cannot accidentally appear in logs
* [ ] Prefer safe human-readable operation descriptions over dumping complete commands when commands fail


### Reduce backend privileges

The trusted Ruby backend needs Docker control, but it may not need every privilege currently granted to its container.

* [ ] Test the complete application and E2E suite without `privileged: true` on the Ruby service
* [ ] Keep the reduced configuration if everything still works
* [ ] Revisit other broad container capabilities when there is a concrete opportunity to remove them safely


### Dependency and image verification

Improve supply-chain reproducibility gradually rather than trying to rewrite the complete Docker build at once.

* [ ] Add checksum verification for important downloaded release archives where upstream checksums are available
* [ ] Consider digest pinning for important base images
* [ ] Keep version pins centralized and easy to update


## Testing and Operations

### Continuous integration

The Playwright suite now covers a growing number of real student workflows. Add automated execution without making every push run the most expensive tests.

* [ ] Add a small, fast CI gate for cheap checks
* [ ] Run heavier browser/toolchain tests manually, nightly or on a suitable self-hosted runner
* [ ] Keep tutorial tests focused on whether the documented student workflow actually works
* [ ] Add E2E coverage when important tutorials are changed or newly written


### Deployment visibility

* [ ] Show the deployed short Git commit SHA somewhere in the admin area

This should make it immediately obvious which revision is running in production.


### Ruby cleanup

Continue extracting coherent pieces from `main.rb` incrementally rather than attempting a rewrite.

* [ ] Remove duplicated helper/setup definitions that now exist both in `helper.rb` and `main.rb`
* [ ] Consider authentication/session handling as a future extraction candidate
* [ ] Consider workspace lifecycle / nginx management as another future extraction candidate


## Existing Tutorial Cleanup

Do small editorial and technical fixes as the affected tutorials are touched.

* [ ] Svelte: fix remaining markup/editorial issues and clean up event-listener teardown
* [ ] MySQL: improve SQL consistency, fix the population-density example and reduce repetitive download boilerplate
* [ ] Git/GitHub: mark low-level internals as optional/advanced and correct the implication that `git mv` is required for rename detection
* [ ] Fact-check programming-language history, creator/date and lineage claims and use a consistent convention for the year shown in headings
* [ ] Remove the stray `</td></tr>` in `working-with-files.md`
* [ ] Review `_template_codebites.html` language metadata and remaining manually authored homepage image alt attributes
* [ ] On narrow screens, consider left-aligning long tutorial paragraphs instead of using global justified text
* [ ] Add a small `prefers-reduced-motion` pass for Workspace-owned transitions/animations


## Tutorials

The infrastructure is now mature enough that the main growth area should again be teaching material. Prefer tutorials that add genuinely new concepts rather than simply more installed technology.

### 1. Web Server

Build a small web server from scratch as a continuation of the existing TCP/IP tutorial.

Main idea:

> TCP → HTTP → browser → web server

The initial manual HTTP/netcat material already exists. Continue from there without hiding the important parts behind a framework.

* [ ] Connect the tutorial explicitly to the TCP/IP tutorial
* [ ] Write a minimal server using raw TCP sockets
* [ ] Handle `GET /`
* [ ] Add multiple routes
* [ ] Return `404 Not Found`
* [ ] Serve HTML
* [ ] Serve static files
* [ ] Read query parameters
* [ ] Optionally handle a small `POST` request
* [ ] Finish by sharing the running server through Shared Live Apps


### 2. WebSockets

Follow the web-server tutorial with persistent, bidirectional communication.

A small Ruby WebSocket server + browser prototype already exists; turn it into a clean teaching sequence.

* [ ] Show why normal HTTP request/response is insufficient for some applications
* [ ] Inspect the WebSocket handshake at a useful level
* [ ] Establish a WebSocket connection
* [ ] Send messages browser → server
* [ ] Send messages server → browser
* [ ] Support multiple connected browsers
* [ ] Build a small multiplayer or collaborative project
* [ ] Use Shared Live Apps so classmates can actually join the running project

Possible projects:

* Shared pixel canvas
* Multiplayer Pong
* Collaborative cursors
* Classroom voting
* Tiny multiplayer world

Prefer something more interesting than a basic chat application.


### 3. Neo4j

Neo4j is already part of the Workspace; add teaching material that explains **why graph databases exist**, not merely Cypher syntax.

* [ ] Add Neo4j to the tutorial overview
* [ ] Introduce nodes and relationships immediately with Cypher
* [ ] Visualize the first graph
* [ ] Add properties and labels
* [ ] Query relationships
* [ ] Introduce paths and variable-length paths
* [ ] Find shortest paths
* [ ] Add aggregation
* [ ] Compare graph modelling with relational modelling / SQL

A small people/movies graph remains a good starting dataset.


### 4. Cryptography in the Terminal

Keep this practical and terminal-based, using real Unix tools wherever possible.

* [ ] Hash a file and observe the effect of changing one byte
* [ ] Explain what hashes can and cannot prove
* [ ] Introduce password hashing and salts
* [ ] Encrypt and decrypt a file symmetrically
* [ ] Generate a public/private key pair
* [ ] Sign and verify data
* [ ] Modify signed data and observe verification fail
* [ ] Generate and inspect an Ed25519 SSH key
* [ ] Connect SSH authentication to the existing Git material

Possible tools: `sha256sum`, `openssl`, `ssh-keygen`, `gpg`.

Keep the mathematics optional at first; start with observable behaviour.


### 5. Flutter

Write a compact Flutter Web tutorial rather than a complete Flutter course.

* [ ] Create the smallest Flutter application
* [ ] Explain widgets and layout
* [ ] Handle button presses
* [ ] Introduce state
* [ ] Rebuild UI from state
* [ ] Work with lists
* [ ] Add sorting/filtering
* [ ] Add a small animation if useful
* [ ] Run the finished application in the browser
* [ ] Share the running app through Shared Live Apps

The existing sorting demo can be a starting point.

Useful conceptual contrast:

```text
HTML/JavaScript:
Find an element and change it.

Flutter:
Change the state and rebuild the interface.
```


### 6. raylib + WebAssembly

The toolchain is already installed and tested. This is now purely a teaching-material task.

Use raylib as the higher-level C graphics/game route and keep PixelRAM as the separate low-level framebuffer route.

* [ ] Write the smallest raylib program
* [ ] Draw basic shapes
* [ ] Handle keyboard and mouse input
* [ ] Explain the game loop
* [ ] Build a small graphical game or simulation
* [ ] Compile the same C program to WebAssembly with Emscripten
* [ ] Explain at a high level what the generated `.wasm` file is
* [ ] Run the WebAssembly build in the browser
* [ ] Publish the finished browser build on a Hackschule subdomain

Possible projects: Pong, Breakout, Snake, Asteroids or a particle simulation.


### 7. Hugo / Static Site Generators

Hugo is already installed in the Workspace.

* [ ] Write a compact tutorial showing why a static-site generator is useful
* [ ] Create a minimal site
* [ ] Explain content vs. templates/layouts
* [ ] Add several pages/posts
* [ ] Build the site
* [ ] Publish the generated static files


## Tutorial Follow-ups

### Shared Live Apps in existing tutorials

Shared Live Apps is infrastructure, not a separate tutorial.

* [ ] Mention it in BIF as an option for peer-testing unfinished stories
* [ ] Add it to other tutorials where sharing a running application is genuinely useful


### BIF workshop feedback

* [ ] Revisit the BIF tutorial after the teacher workshop and incorporate useful classroom feedback


### Automatron / Formal Languages

Keep Automatron as a contextual teaching tool rather than adding every specialist tool to the main navigation.

* [ ] Link it directly from relevant formal-language / automata material
* [ ] Mention the direct URL in teacher-facing lesson notes where useful
* [ ] Add a short introduction if students are expected to use it independently


## Later Tutorial Ideas

These are worthwhile directions, but not all need to become tutorials soon.

### Prolog

Expose students to a genuinely different programming paradigm.

* facts, rules and queries
* variables and pattern matching
* backtracking
* recursive rules
* a small knowledge base
* a search/logic problem such as route finding, a mystery or a logic puzzle


### Digital Audio

Browser-based sound synthesis would add a new creative computing area without extra server infrastructure.

* oscillators and waveforms
* frequency/pitch and amplitude
* envelopes
* mixing
* waveform/spectrum visualization
* a tiny synthesizer, sequencer or drum machine

Practical requirement: headphones in the classroom.


### WebGPU / Parallel Computing

If classroom browser support is reliable, focus primarily on compute rather than introducing yet another graphics API.

* smallest useful WGSL compute shader
* moving data between JavaScript and the GPU
* CPU vs. GPU comparison
* a highly parallel workload such as particles, cellular automata, image processing or fractals


### Git Collaboration

Extend the existing Git material when collaborative workflows become useful.

* branches
* merge
* merge conflicts
* pull requests
* working with another student
* basic CI/testing


### Data Formats

Possible compact tutorial or collection connecting several existing topics.

* text vs. binary data
* CSV
* JSON
* character encodings
* hex dumps
* connections to JPEG and network protocols


### PixelRAM: Further Projects

The introductory PixelRAM tutorial is complete. Future additions should be optional projects for students who want to go deeper.

Possible projects:

* plasma / classic demoscene effect
* Conway's Game of Life
* Mandelbrot set
* raycaster
* raytracer
* simple software triangle/rasterizer
* advanced game-port demonstrations

Important principle:

> Do not turn PixelRAM into a second raylib. Drawing and rendering algorithms are valuable precisely because students can implement them themselves on top of the framebuffer.


## Suggested Tutorial Order

1. Web Server
2. WebSockets
3. Neo4j
4. Cryptography
5. Flutter
6. raylib + WebAssembly
7. Hugo / Static Site Generators

The first two should form a continuous sequence after the existing TCP/IP tutorial. After that, prefer breadth: graph databases, security, UI programming and other genuinely different areas of computing.