// Screenshot hooks for »Katas: Erste Funktionen«
// (src/tutorial-screenshots/README.md, »Tutorial-specific hooks«).

// The code the card picture shows, per Kata and language.
const SOLUTIONS = {
    greet: {
        ruby: 'def greet(name)\n  "Hallo, #{name}!"\nend\n\nputs greet(\'Ada\')\n',
        python: "def greet(name):\n    return f'Hallo, {name}!'\n\nprint(greet('Ada'))\n",
    },
};

export default {
    actions: {
        // hook: solve ID: the visible card of the Kata ID gets its code and is
        // checked; the action waits for the feedback.
        async solve({ page, args, timeout }) {
            const id = String(args).trim().split(/\s+/)[0];
            const card = page.locator(`.task-card[data-task$="/${id}"]:visible`);
            const field = card.locator('.task-code');
            const language = await field.getAttribute('data-code-language');
            await field.fill(SOLUTIONS[id][language]);
            await card.locator('.task-submit').click();
            await card.locator('.task-feedback .bi-stars, .task-feedback-correct').first().waitFor({ timeout });
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
