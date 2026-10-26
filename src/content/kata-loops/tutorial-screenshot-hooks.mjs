// Screenshot hooks for »Katas: Wiederholen«
// (src/tutorial-screenshots/README.md, »Tutorial-specific hooks«).

// The code the card picture shows, per Kata and language.
const SOLUTIONS = {
    'digit-sum': {
        ruby: 'def digit_sum(n)\n  total = 0\n  while n > 0\n    total += n % 10\n    n /= 10\n  end\n  total\nend\n',
        python: 'def digit_sum(n):\n    total = 0\n    while n > 0:\n        total += n % 10\n        n //= 10\n    return total\n',
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
