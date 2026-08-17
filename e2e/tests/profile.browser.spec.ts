import { expect, test } from './fixtures';
import { loginAsE2eUser } from './workspace';

const E2E_TEACHER_EMAIL = 'e2e-teacher@example.com';

test('student profile groups controls and protects credentials', async ({
  page,
  e2eEmail,
}, testInfo) => {
  await loginAsE2eUser(page, e2eEmail, testInfo);
  await page.goto('/profil');

  const expectedSections = [
    ['Workspace', '#workspace'],
    ['Werkzeuge', '#tools'],
    ['Datenbanken', '#databases'],
    ['Konto & Darstellung', '#account-appearance'],
  ] as const;

  for (const [name, anchor] of expectedSections) {
    await expect(page.getByRole('heading', { name, exact: true })).toBeVisible();
    await expect(
      page.locator(`.autotoc-secondary a[href="${anchor}"]`),
    ).toHaveCount(1);
  }

  await expect(page.locator('#bu_launch_profile')).toBeVisible();
  await expect(page.locator('#live_apps_container')).toBeVisible();
  await expect(page.locator('#bu_reset_workspace')).toBeVisible();
  await expect(page.locator('#profile-for-teachers')).toBeHidden();
  await expect(
    page.locator('.autotoc-secondary a[href="#for-teachers"]'),
  ).toHaveCount(0);

  for (const database of ['mysql', 'neo4j'] as const) {
    const databaseName = database === 'mysql' ? 'MySQL' : 'Neo4j';
    const credentials = page.locator(
      `#${database}-database .profile-credentials`,
    );
    const password = page.locator(`#${database}_password`);

    // The accessible name intentionally changes between "anzeigen" and
    // "verbergen", so keep the locator valid for both states.
    const toggle = credentials.getByRole('button', {
      name: new RegExp(
        `^${databaseName}-Passwort (anzeigen|verbergen)$`,
      ),
    });

    await expect(credentials).toBeVisible();
    await expect(
      credentials.getByRole('heading', {
        name: `${databaseName}-Zugangsdaten`,
        exact: true,
      }),
    ).toBeVisible();
    await expect(
      page.locator(`#${database}-database details.profile-credentials`),
    ).toHaveCount(0);
    await expect(password).toBeVisible();
    await expect(password).toHaveAttribute('type', 'password');

    await expect(toggle).toHaveAccessibleName(
      `${databaseName}-Passwort anzeigen`,
    );
    await expect(toggle).toHaveAttribute('aria-pressed', 'false');

    await toggle.click();

    await expect(password).toHaveAttribute('type', 'text');
    await expect(toggle).toHaveAttribute('aria-pressed', 'true');
    await expect(toggle).toHaveAccessibleName(
      `${databaseName}-Passwort verbergen`,
    );

    await toggle.click();

    await expect(password).toHaveAttribute('type', 'password');
    await expect(toggle).toHaveAttribute('aria-pressed', 'false');
    await expect(toggle).toHaveAccessibleName(
      `${databaseName}-Passwort anzeigen`,
    );
  }

  await page.context().grantPermissions(
    ['clipboard-read', 'clipboard-write'],
    { origin: new URL(page.url()).origin },
  );

  const mysqlPassword = await page.locator('#mysql_password').inputValue();

  await page.getByRole('button', {
    name: 'MySQL-Passwort kopieren',
  }).click();

  await expect.poll(
    () => page.evaluate(() => navigator.clipboard.readText()),
  ).toBe(mysqlPassword);

  await expect(page.locator('#mysql_password')).toHaveAttribute(
    'type',
    'password',
  );

  await page.locator('#bu_reset_workspace').click();

  const resetModal = page.locator('#__template_modal');

  await expect(
    resetModal.getByText(
      'Dieser Vorgang kann nicht rückgängig gemacht werden.',
    ),
  ).toBeVisible();

  await expect(
    resetModal.locator('#ti_reset_workspace_confirmation'),
  ).toBeVisible();

  await expect(
    resetModal.getByRole('button', {
      name: 'Workspace endgültig zurücksetzen',
    }),
  ).toBeDisabled();

  await resetModal.getByRole('button', { name: 'Abbrechen' }).click();
});

test('profile remains usable at a narrow viewport', async ({
  page,
  e2eEmail,
}, testInfo) => {
  await page.setViewportSize({ width: 390, height: 844 });
  await loginAsE2eUser(page, e2eEmail, testInfo);
  await page.goto('/profil');

  await expect(
    page.locator('#mysql-database .profile-credentials'),
  ).toBeVisible();
  await expect(page.locator('#mysql_password')).toBeVisible();
  await expect(page.locator('.autotoc-secondary')).toBeHidden();

  await expect.poll(
    () => page.evaluate(() => ({
      clientWidth: document.documentElement.clientWidth,
      scrollWidth: document.documentElement.scrollWidth,
    })),
  ).toEqual({
    clientWidth: 390,
    scrollWidth: 390,
  });
});

test('teacher profile shows the permission-protected teacher section', async ({
  page,
}, testInfo) => {
  await loginAsE2eUser(page, E2E_TEACHER_EMAIL, testInfo);
  await page.goto('/profil');

  await expect(page.locator('#profile-for-teachers')).toBeVisible();

  await expect(
    page.getByRole('heading', {
      name: 'Für Lehrkräfte',
      exact: true,
    }),
  ).toBeVisible();

  await expect(page.locator('#bu_upload_test_archive')).toBeVisible();

  await expect(
    page.locator('.autotoc-secondary a[href="#for-teachers"]'),
  ).toHaveCount(1);
});