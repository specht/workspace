// Start page: whether the person's clone of a pinned activity's template is
// current (TemplateFreshness via /api/template/status). The Workspace only
// looks; git pull is up to the student.
(function () {
    const TEXTS = {
        current: (folder) => `✓ Deine Kopie in ${folder} ist aktuell.`,
        outdated: (folder) => `Es gibt eine neue Version: führe in ${folder} git pull aus.`,
        absent: (folder) => `Noch nicht geklont: ${folder} gibt es nicht.`,
        other: (folder) => `${folder} ist keine Kopie dieser Vorlage.`,
        unavailable: () => 'Ob deine Kopie aktuell ist, sieht man, wenn dein Workspace läuft.',
    };
    document.querySelectorAll('.shc-template').forEach((line) => {
        const folder = '/workspace/' + line.dataset.folder;
        api_call('/api/template/status', { slug: line.dataset.slug }, function (data) {
            if (!data.success || !TEXTS[data.status]) return;
            line.textContent = TEXTS[data.status](folder);
            line.classList.add('shc-template-' + data.status);
        }, { no_please_wait: true });
    });
})();
