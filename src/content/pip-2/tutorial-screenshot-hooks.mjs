// Screenshot hooks for »Pip unter Funkelbach: Bellos Spur«
// (src/tutorial-screenshots/README.md, »Tutorial-specific hooks«).

// The plan the card picture shows, per Kata and language.
const PLANS = {
    trail: {
        "ruby": "def turn_right\n  3.times do\n    turn_left\n  end\nend\n\nuntil bello_here?\n  turn_left\n  turn_right until trail_ahead? || puddle_ahead?\n  move\nend\n",
        "python": "def turn_right():\n    for i in range(3):\n        turn_left()\n\nwhile not bello_here():\n    turn_left()\n    while not (trail_ahead() or puddle_ahead()):\n        turn_right()\n    move()\n",
        "javascript": "function turnRight() {\n  for (let i = 0; i < 3; i++) {\n    turnLeft();\n  }\n}\n\nwhile (!belloHere()) {\n  turnLeft();\n  while (!(trailAhead() || puddleAhead())) {\n    turnRight();\n  }\n  move();\n}\n"
    },
};

export default {
    actions: {
        // hook: solve ID: the visible card of the Kata ID gets its plan and is
        // checked; once the replay has run, it goes back to the middle of
        // the run.
        async solve({ page, args, timeout }) {
            const id = String(args).trim().split(/\s+/)[0];
            const card = page.locator(`.task-card[data-task$="/${id}"]:visible`);
            const field = card.locator('.task-code');
            const language = await field.getAttribute('data-code-language');
            await field.fill(PLANS[id][language]);
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
