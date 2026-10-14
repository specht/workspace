<script>
    let state = $state(0);
    // 0 : idle
    // 1 : space pressed
    // 2 : space pressed for 500 ms
    // 3 : timer running
    // 4 : timer stopped
</script>

<div class="main">
    <h1>Rubik's Cube Timer</h1>
    <p>State = {state}</p>
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
</style>
