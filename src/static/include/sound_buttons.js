// Recordings to play beside their text (default.css, .sound-grid): each
// .sound-item has a .sound-play button and an <audio>. A click plays it and
// turns the button into a stop button; another recording stops the one
// before, so that only one plays at a time.
(function () {
    let current = null;

    function stop() {
        if (!current) return;
        current.audio.pause();
        current.audio.currentTime = 0;
        current.item.classList.remove('playing');
        current.icon.className = 'bi bi-play-fill';
        current.button.setAttribute('aria-label', current.label);
        current = null;
    }

    document.addEventListener('click', (e) => {
        const button = e.target.closest('.sound-play');
        if (!button) return;
        const item = button.closest('.sound-item');
        const same = current && current.item === item;
        stop();
        if (same) return;
        const audio = item.querySelector('audio');
        current = { item, audio, button, icon: button.querySelector('i'), label: button.getAttribute('aria-label') };
        item.classList.add('playing');
        current.icon.className = 'bi bi-stop-fill';
        button.setAttribute('aria-label', 'Anhalten');
        audio.addEventListener('ended', stop, { once: true });
        audio.play().catch(stop);
    });
})();
