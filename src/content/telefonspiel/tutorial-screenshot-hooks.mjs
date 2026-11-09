// Screenshot hooks for »Ein Spiel am Telefon«
// (src/tutorial-screenshots/README.md, »Tutorial-specific hooks«).

// Where infinite animations stand in the picture (ms into their cycle):
// the cursor at its largest, the »Anruf läuft« dot fully lit, the voice
// bars each at its own height (their delays stay).
const PHASES = { pulse: 500, blink: 0, bar: 250 };

export default {
    actions: {
        // hook: freeze-at TEXT: waits until the game's last bubble contains
        // TEXT, then stops the call exactly there, so every capture shows
        // the same words. The typing follows the AudioContext's clock
        // (page.html, typeSentence), so suspending it stops the words, the
        // voice and the game, which waits for the sentence to end. Words a
        // slow poll let through after TEXT are taken out again; the call
        // timer shows a fixed time; animations stand still.
        async 'freeze-at'({ page, args, timeout }) {
            const text = args;
            await page.waitForFunction((text) => {
                const bubble = [...document.querySelectorAll('.msg.game')].pop();
                return bubble && bubble.textContent.includes(text);
            }, text, { timeout });
            await page.evaluate(async ({ text, phases }) => {
                await ctx.suspend();
                const bubble = [...document.querySelectorAll('.msg.game')].pop();
                const words = [...bubble.querySelectorAll('.w')];
                while (words.length && !bubble.textContent.trimEnd().endsWith(text.trim())) {
                    const word = words.pop();
                    if (word.previousSibling && word.previousSibling.nodeType === Node.TEXT_NODE) word.previousSibling.remove();
                    word.remove();
                }
                // the click on the receiver scrolled it into view; the phone may
                // be cut off at the bottom, not at the top
                window.scrollTo(0, 0);
                clearInterval(clock);
                document.getElementById('status').textContent = 'Verbunden 0:42';
                for (const animation of document.getAnimations()) {
                    if (animation.effect.getTiming().iterations !== Infinity) { animation.finish(); continue; }
                    animation.pause();
                    animation.currentTime = (phases[animation.animationName] ?? 0) - animation.effect.getTiming().delay;
                }
            }, { text, phases: PHASES });
            // no key under the mouse lit up
            await page.mouse.move(page.viewportSize().width - 2, 2);
        },

        // hook: teaser DEGREES: the layout of the teaser: the phone turned a
        // little (the chat stays straight) and both centred with a margin, so
        // the phone is cut off only at the bottom; the page itself is upright
        async teaser({ page, args }) {
            await page.evaluate((degrees) => {
                document.querySelector('main').style.gridTemplateColumns = '310px minmax(0, 540px)';
                const phone = document.querySelector('.phone');
                phone.style.transformOrigin = '50% 30%';
                phone.style.transform = `rotate(${degrees}deg)`;
            }, Number(args));
        },
    },

    variants: {
        // variants: dark: the phone page of telefon follows the system's
        // colour scheme
        async dark({ page, on }) {
            await page.emulateMedia({ colorScheme: on ? 'dark' : 'light' });
            await page.waitForTimeout(500);
        },
    },
};
