// Screenshot hooks for »Pip unter Funkelbach: Hinab«
// (src/tutorial-screenshots/README.md, »Tutorial-specific hooks«).

const RIGHT = ['turn_left', 'turn_left', 'turn_left'];
const PLANS = {
    tunnel: ['move', 'take', 'move', 'take', ...RIGHT, 'move', 'take', 'move', 'take', 'turn_left',
        'move', 'take', 'move', 'take', 'move', 'take', ...RIGHT, 'move', 'take', 'move', 'take', ...RIGHT,
        'move', 'take', 'move', 'take', 'move'],
};
const SPELLING = {
    ruby: (c) => c,
    python: (c) => c + '()',
    javascript: (c) => (c === 'turn_left' ? 'turnLeft' : c) + '();',
};

export default {
    actions: {
        // hook: solve ID: the visible card of the Kata ID gets its plan and is
        // checked; once the replay has run, it goes back to the middle of
        // the run, Pip halfway with half the stones taken.
        async solve({ page, args, timeout }) {
            const id = String(args).trim().split(/\s+/)[0];
            const card = page.locator(`.task-card[data-task$="/${id}"]:visible`);
            const field = card.locator('.task-code');
            const language = await field.getAttribute('data-code-language');
            await field.fill(PLANS[id].map(SPELLING[language]).join('\n') + '\n');
            await card.locator('.task-submit').click();
            // the result shows once the replay is over: skip to its end
            await card.locator('.pip-world[data-steps]:not([data-steps="1"])').waitFor({ timeout });
            await card.locator("[data-go='last']").click();
            await card.locator('.task-feedback .bi-stars, .task-feedback-correct').first().waitFor({ timeout });
            await card.evaluate((element) => {
                const player = element.pipPlayer;
                player.stop();
                player.go(Math.floor(player.frames.length / 2));
                // away from the sticky bar of the chapters
                element.querySelector('.pip-map').scrollIntoView({ block: 'center' });
            });
            await page.addStyleTag({ content: 'footer.footer-fixed { display: none; }' });
        },
    },

    // variants: dark: the site follows the system's colour scheme.
    variants: {
        async dark({ page, on }) {
            await page.emulateMedia({ colorScheme: on ? 'dark' : 'light' });
            // the page's colours move over to the new scheme
            await page.waitForTimeout(800);
        },
    },
};
