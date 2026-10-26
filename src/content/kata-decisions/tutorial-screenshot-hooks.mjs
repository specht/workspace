// Screenshot hooks for »Katas: Entscheidungen«
// (src/tutorial-screenshots/README.md, »Tutorial-specific hooks«).

// The code the card picture shows, per Kata and language.
const SOLUTIONS = {
    fizzbuzz: {
        ruby: "def fizzbuzz(n)\n  if n % 15 == 0\n    'FizzBuzz'\n  elsif n % 3 == 0\n    'Fizz'\n  elsif n % 5 == 0\n    'Buzz'\n  else\n    n.to_s\n  end\nend\n",
        python: "def fizzbuzz(n):\n    if n % 15 == 0:\n        return 'FizzBuzz'\n    elif n % 3 == 0:\n        return 'Fizz'\n    elif n % 5 == 0:\n        return 'Buzz'\n    return str(n)\n",
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
