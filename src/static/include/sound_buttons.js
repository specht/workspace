// Recordings to play, shown as the phone of the Telefonspiel shows a call
// (default.css, .voice-chat): each .voice-item has a .voice-line button and
// an <audio>. A click plays it, a second click stops it; another recording
// stops the one before. ».voice-all« plays them one after the other.
(function () {
    let current = null;     // { item, audio }
    let queue = [];         // the items still to come after it

    function finish() {
        if (!current) return;
        current.audio.pause();
        current.audio.currentTime = 0;
        current.item.classList.remove('playing');
        current.item.querySelector('.voice-line').removeAttribute('aria-pressed');
        current = null;
    }

    function play(item) {
        finish();
        const audio = item.querySelector('audio');
        current = { item, audio };
        item.classList.add('playing');
        item.querySelector('.voice-line').setAttribute('aria-pressed', 'true');
        audio.onended = () => {
            finish();
            if (queue.length) play(queue.shift());
        };
        audio.play().catch(() => { queue = []; finish(); });
    }

    document.addEventListener('click', (e) => {
        const all = e.target.closest('.voice-all');
        const line = e.target.closest('.voice-line');
        if (all) {
            const items = [...all.closest('.voice-chat').querySelectorAll('.voice-item')];
            queue = items.slice(1);
            play(items[0]);
        } else if (line) {
            const item = line.closest('.voice-item');
            const same = current && current.item === item;
            queue = [];
            if (same) finish();
            else play(item);
        }
    });
})();
