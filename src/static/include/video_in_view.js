// Videos that play while they are on the screen: <video data-play-in-view
// muted loop playsinline preload='none' poster='…'>. Nothing loads before
// the video comes into view; then it plays, muted and in a loop, and stops
// when it leaves. Whoever asks the system for less motion gets the controls
// instead and starts it themselves.
//
//   <script src='/include/video_in_view.js'></script>
(function () {
    'use strict';
    const still = window.matchMedia('(prefers-reduced-motion: reduce)').matches;

    function setUp(video) {
        video.muted = true;
        if (still || !('IntersectionObserver' in window)) {
            video.controls = true;
            return;
        }
        new IntersectionObserver((entries) => {
            for (const entry of entries) {
                if (entry.isIntersecting) video.play().catch(() => { video.controls = true; });
                else video.pause();
            }
        }, { threshold: 0.33 }).observe(video);
    }

    const go = () => document.querySelectorAll('video[data-play-in-view]').forEach((video) => {
        if (!video.dataset.ready) { video.dataset.ready = '1'; setUp(video); }
    });
    if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', go);
    else go();
})();
