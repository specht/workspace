document.addEventListener("DOMContentLoaded", () => {
  const markdownContent = document.getElementById("markdown-content");
  const runAllBtn = document.getElementById("run-all-btn");

  // Fetch markdown file
  fetch("strudel.md?" + new Date().getTime())
    .then((response) => {
      if (!response.ok) throw new Error(`HTTP Status ${response.status}`);
      return response.text();
    })
    .then((markdown) => {
      console.log("Fetched markdown:", markdown);
      markdownContent.innerHTML = `<div class="markdown-body">${marked.parse(markdown)}</div>`;
    })
    .catch((error) => {
      console.error("Error loading strudel.md:", error);
      markdownContent.innerHTML = `
        <div class="error-indicator">
          <i class="bi bi-exclamation-triangle-fill" style="font-size: 2rem;"></i>
          <h3>Failed to load strudel.md</h3>
          <p>${error.message}</p>
        </div>
      `;
    });

  // Run all button
  runAllBtn?.addEventListener("click", () => {
    document
      .querySelectorAll("strudel-editor")
      .forEach((editor) => editor.play?.());
  });

  // Keyboard shortcuts
  document.addEventListener("keydown", (e) => {
    if (e.ctrlKey && e.key === "Enter") {
      e.preventDefault();
      document
        .querySelectorAll("strudel-editor")
        .forEach((editor) => editor.play?.());
    }
    if (e.ctrlKey && e.key === ".") {
      e.preventDefault();
      console.log("Stop shortcut pressed");
    }
  });

  console.log("Strudel editors initialized");
});
