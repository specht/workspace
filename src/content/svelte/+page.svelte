<script>
    import { onMount } from "svelte";

    let state = $state(0);
    // 0 : idle
    // 1 : space pressed
    // 2 : space pressed for 500 ms
    // 3 : timer running
    // 4 : timer stopped
    let t0 = 0;
    let timerString = $state("00:00<span class='small'>.00</span>");
    let timeoutId = null;

    function updateTimer() {
        if (state != 3) return;

        let t1 = Date.now();
        let duration = (t1 - t0) / 1000.0;

        let minutes = `${Math.floor(duration / 60.0)}`;
        if (minutes.length < 2) minutes = "0" + minutes;

        let seconds = `${Math.floor(duration % 60)}`;
        if (seconds.length < 2) seconds = "0" + seconds;

        let centiseconds = `${Math.floor(duration * 100.0) % 100}`;
        if (centiseconds.length < 2) centiseconds = "0" + centiseconds;

        timerString = `${minutes}:${seconds}<span class='small'>.${centiseconds}</span>`;

        requestAnimationFrame(updateTimer);
    }

    function handleKeyDown() {
        if (state === 0) {
            state = 1;
            timeoutId = setTimeout(() => {
                if (state === 1) {
                    state = 2;
                    document.querySelector(".timer")?.classList.add("ready");
                }
            }, 500);
        } else if (state === 3) {
            state = 4;
        }
    }

    function handleKeyUp() {
        if (state === 1) {
            state = 0;
            clearTimeout(timeoutId);
        } else if (state === 2) {
            state = 3;
            t0 = Date.now();
            requestAnimationFrame(updateTimer);
        }
    }

    onMount(() => {
        document.addEventListener("keydown", (e) => {
            if (e.code === "Space") {
                handleKeyDown();
            }
        });
        document.addEventListener("keyup", (e) => {
            if (e.code === "Space") {
                handleKeyUp();
            }
        });
        document.addEventListener("touchstart", () => handleKeyDown());
        document.addEventListener("touchend", () => handleKeyUp());
    });
</script>

<div class="main">
    <h1>Rubik's Cube Timer</h1>

    <p>State = {state}</p>

    <p>
        Halte die Leertaste gedrückt, bis der Timer grün wird. Wenn du dann los
        lässt, beginnt die Zeit zu laufen.
    </p>

    <p class="timer">{@html timerString}</p>
</div>

<style>
    .main {
        display: flex;
        flex-direction: column;
        align-items: center;
        justify-content: center;
        min-height: 100vh;
        margin: 0 1em;
        user-select: none;
    }
    .timer {
        font-size: 300%;
        font-weight: bold;
        padding: 0.25em 0.5em;
        border-radius: 0.2em;
        background-color: #eeeeec;

        :global(.small) {
            font-size: 75%;
        }

        :global(&.ready) {
            transition: background-color 0.3s ease-in;
            background-color: #73a946;
        }
    }
</style>
