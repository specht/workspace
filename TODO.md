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

### PixelRAM ✅

Implemented and published in August 2026.

PixelRAM is now the low-level graphics path for older students: a tiny software framebuffer library for fast pixel graphics, software rendering and old-school effects. raylib is only the private backend; student programs use the small PixelRAM API.

* Source: https://github.com/specht/pixelram
* Documentation: https://specht.github.io/pixelram/

Completed:

* [x] Publish `pixelram.c` + `pixelram.h` as a self-contained library
* [x] Keep the public API independent of raylib
* [x] Use simple snake_case functions such as `set_pixel()`, `get_pixel()` and `present()`
* [x] Support `pixel_indexed8`, `pixel_rgb565`, `pixel_rgb24` and `pixel_rgba32`
* [x] Expose direct framebuffer access for high-performance renderers and game ports
* [x] Include the same 88 predefined palettes as Pixelflow Canvas
* [x] Use the classic 256-color VGA palette by default
* [x] Add keyboard, mouse, timing, fullscreen and pixel-aspect support
* [x] Add configurable frame limiting with 60 FPS as the default
* [x] Provide a Pixelflow-style fire demo as the bridge from Ruby to C
* [x] Add detailed API documentation and GitHub Pages
* [x] Add tests and CI
* [x] Make pinned two-file downloads possible from a student project's Makefile

Important teaching distinction:

> Pixelflow Canvas is a language-independent canvas inside VS Code. PixelRAM is a compiled software framebuffer. The fire demo can connect the two because the algorithm stays almost unchanged while the execution model becomes much faster.

Workspace tutorial completed:

* [x] Publish a student-facing PixelRAM tutorial in the Workspace
* [x] Start with the smallest possible framebuffer example
* [x] Introduce indexed color and palettes
* [x] Build up from individual pixels to calculations over the complete framebuffer
* [x] Develop an interference pattern step by step
* [x] Connect the visual result to animation, performance and low-level C programming
* [x] Keep the tutorial focused on direct pixel access rather than turning PixelRAM into a drawing API

Further optional PixelRAM projects are tracked below under **PixelRAM: Further Projects**.

---

### raylib / WebAssembly Infrastructure ✅

The underlying C/WebAssembly graphics toolchain is now working in the Workspace.

* [x] raylib is available in the workspace image
* [x] Emscripten builds raylib programs to WebAssembly
* [x] Browser rendering, keyboard and mouse input work
* [x] Pixel aspect-ratio handling works for low-resolution graphics
* [x] Sound effects work in browser builds
* [x] A DOOM-based demonstration runs through the framebuffer abstraction
* [x] PixelRAM now provides the student-facing low-level framebuffer layer

The remaining work is mainly **teaching material**, not infrastructure.

---

### BIF / Interactive Fiction Tutorial ✅

Published in August 2026.

The BIF tutorial is now a first-class Workspace tutorial at https://workspace.hackschule.de/bif. It deliberately uses a tiny human-written school story, **Nach Schulschluss**, so the example stays small enough to understand while students gradually add the actual BIF concepts.

Completed:

* [x] Create a very small human-written starter story
* [x] Explain the basic BIF file structure
* [x] Add page choices and link multiple scenes
* [x] Show the story graph early and use it throughout the tutorial
* [x] Introduce local choices
* [x] Introduce variables/state
* [x] Add conditional text and conditional choices
* [x] Use a persistent flag (`has_key`) so earlier actions affect later scenes
* [x] Show how to run and preview the story in the Workspace
* [x] Explain the Problems and State views
* [x] Cover images, audio and video
* [x] Add examples for values in text, randomness, graph groups and advanced JavaScript
* [x] Document themes, brightness, custom colors and separate body/heading fonts
* [x] End with guidance for developing an original story rather than another large starter
* [x] Keep the Odyssey and other large AI-written stories out of the starter workflow

Teaching approach:

> Start with story structure and meaningful choices, then introduce state only when the simple story creates a real need for it. Technology should support the story rather than become the point of the exercise.

The creative-writing section gives students a compact framework built around figure, goal, setting, conflict, plot, decisions, consequences and endings, plus practical advice for branching stories.

Follow-up work:

* [ ] Mention **Shared Live Apps** explicitly as an option for peer testing unfinished stories
* [ ] Add E2E coverage for the documented BIF student workflow
* [ ] Revisit the tutorial after the first teacher workshop and incorporate classroom feedback

---

### Tutorial Review / Cleanup ✅

A focused review of the existing tutorial collection was started in August 2026. The main goal is to improve the teaching sequence and remove small technical/editorial problems without turning the tutorials into a uniform template.

Completed:

* [x] Rewrite the static-website tutorial around one small coherent project with frequent visible results
* [x] Keep image import practical for the browser-based Workspace and reuse the proven drag/drop and clipboard workflow
* [x] Add responsive layout work with a clear before/after mobile comparison
* [x] Extract the Shower.js miniature screenshot/lightbox into the reusable `tutorial-preview` component
* [x] Use `tutorial-preview` as a visual anchor for rendered results in the static-website tutorial
* [x] Remove the duplicated Shower.js preview/lightbox CSS during that extraction
* [x] Clean up `sections.yaml`: keep the database description about databases and add a proper description for **Experimente**

Remaining cleanup from the same review:

* [ ] Svelte: fix small markup/editorial issues, put the `<style>` guidance in the correct place and clean up event-listener teardown
* [ ] MySQL: use single quotes consistently for SQL strings, fix the misleading population-density example and reduce repeated file-download boilerplate
* [ ] Git/GitHub: keep the technical depth but mark internals as optional/advanced and correct the implication that `git mv` is required for rename detection
* [ ] Fact-check the programming-language tutorials, especially creator/date/history/lineage claims such as the simplified COBOL attribution
* [ ] Remove the stray `</td></tr>` in `working-with-files.md`
* [ ] Check `_template_codebites.html` for `lang="de"` and review the remaining manually authored homepage icon alt attributes


## High Priority

### Web Server

Build a small web server from scratch as a continuation of the TCP/IP tutorial.

The initial HTTP/netcat material has already been prototyped and debugged; the main remaining work is turning it into the complete server tutorial.

* [x] Start with a manual HTTP response using `nc`
* [x] Inspect the request sent by the browser
* [x] Explain the HTTP request/response format
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

A small Ruby WebSocket server + browser shared-canvas prototype already works, including use through Shared Live Apps. The remaining task is to turn that prototype into a clean teaching sequence.

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

### Flutter

Add a compact Flutter tutorial rather than a complete Flutter course.

Use Flutter Web so everything works inside the workspace. Flutter Web has been verified in the Workspace and the existing sorting demo is a working starting point; what remains is tutorial authoring.

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

A Playwright-based E2E test setup for tutorials is already in use.

* [x] Establish browser-based E2E testing for Workspace tutorials
* [x] Add toolchain coverage for the LaTeX tutorial
* [x] Add coverage for Pixelflow Canvas
* [ ] Make Pixelflow Canvas tests reliable in both headed and normal test runs
* [ ] Add E2E coverage for more tutorials as they are touched
* [ ] Add Shared Live Apps E2E coverage for share/unshare, authenticated access, WebSocket proxying and automatic unsharing
* [ ] Keep tests focused on whether the documented student workflow actually works


### raylib + WebAssembly Tutorial

The infrastructure is already working; this item is now about writing a **student-facing raylib tutorial**.

Use raylib as the higher-level, code-first environment for graphical programs and small games in C. PixelRAM is the separate low-level framebuffer route.

* [x] Install and test the raylib + Emscripten toolchain in the Workspace
* [x] Verify native-style game loops, keyboard/mouse input and browser rendering
* [x] Verify WebAssembly builds
* [ ] Write the smallest raylib tutorial program
* [ ] Draw basic shapes
* [ ] Handle keyboard and mouse input
* [ ] Explain the game loop
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

This can serve as the main **high-level WebAssembly/game-programming** introduction. PixelRAM should cover the lower-level framebuffer route instead of trying to merge the two APIs.


### PixelRAM: Further Projects

The main PixelRAM tutorial is complete and published. Further work should extend it with optional projects for older students rather than create a second introductory teaching sequence.

Possible projects:

* [ ] Build a simple plasma or another classic demoscene effect
* [ ] Implement Conway's Game of Life
* [ ] Render the Mandelbrot set
* [ ] Rebuild the existing raycaster with PixelRAM
* [ ] Rebuild the existing raytracer with PixelRAM
* [ ] Optionally introduce a simple software triangle / 3D rasterizer
* [ ] Use DOOM as an advanced demonstration of how far the framebuffer abstraction can go
* [ ] Add Chocolate Descent when the WASM port is stable

Important principle:

> Do not turn PixelRAM into a second raylib. Drawing algorithms such as lines, circles, raycasters and rasterizers are valuable precisely because students can implement them on top of the framebuffer.

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
5. [ ] Flutter
6. [ ] raylib + WebAssembly tutorial
7. [ ] PixelRAM follow-up projects

The Web Server and WebSockets tutorials should form a continuous sequence after the existing TCP/IP tutorial.

**Shared Live Apps is already implemented infrastructure, not another tutorial to schedule.** Use it throughout the Web Server, WebSockets and Flutter material, and add it to the completed BIF tutorial where peer testing is useful.

**PixelRAM and raylib should remain two distinct teaching routes:** raylib for higher-level games/graphics, PixelRAM for low-level framebuffer programming and software rendering. The introductory PixelRAM tutorial is already complete; future PixelRAM work should mainly add optional advanced projects.