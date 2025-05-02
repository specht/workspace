import markdownit from 'https://cdn.jsdelivr.net/npm/markdown-it@14/+esm';
import markdownitAttrs from 'https://cdn.jsdelivr.net/npm/markdown-it-attrs@4/+esm';

const md = markdownit({ html: true }).use(markdownitAttrs);
const content = document.getElementById('content');

const lz = {
    compress: LZString.compressToEncodedURIComponent,
    decompress: LZString.decompressFromEncodedURIComponent
};

let context = {
  page: 1
};

const contextProxy = new Proxy(context, {
    has(target, key) {
      return true; // Ensure all names are handled
    },
    get(target, key) {
      if (key in target) {
        return target[key];
      } else {
        return globalThis[key]; // fallback to global (Math, console, etc.)
      }
    },
    set(target, key, value) {
      target[key] = value;
      return true;
    }
  });

function runInContext(code, isCondition = false) {
    const wrappedCode = isCondition ? `return (${code});` : `${code}`;

    let result = Function('ctx', `
      with (ctx) {
        ${wrappedCode}
      }
    `)(contextProxy);

    const json = JSON.stringify(context);
    const compressed = lz.compress(json);
    window.location.hash = `#${compressed}`;
    return result;
}

document.addEventListener("DOMContentLoaded", function () {
    updatePageFromHash();
    // window.addEventListener('hashchange', updatePageFromHash);
});

function appendSection(text) {
    const rule = document.createElement('hr');
    content.appendChild(rule);
    const section = document.createElement('div');
    section.innerHTML = md.render(text);
    content.appendChild(section);
}

function handleConditions(text) {
    const tempDiv = document.createElement('div');
    tempDiv.innerHTML = text;
    const elementsWithCondition = tempDiv.querySelectorAll('[condition]');
    elementsWithCondition.forEach(element => {
        let condition = element.getAttribute('condition');
        let value = runInContext(condition, true);
        if (!value) {
            element.style.display = 'none';
        }
    });
    return tempDiv.innerHTML;
}

function updatePageFromHash() {
    const hash = window.location.hash.substring(1);
    if (hash) {
        const decompressed = lz.decompress(hash);
        if (decompressed) {
            try {
                context = JSON.parse(decompressed);
            } catch (e) {
                console.error('Error parsing context from hash:', e);
            }
        }
    }
    // runInContext(`page = "${page}"`, false);

    fetch(`/pages/${context.page}.md`)
    .then(response => {
        if (!response.ok) {
            throw new Error('Network response was not ok');
        }
        return response.text();
    })
    .then(data => {
        const parser = new DOMParser();
        const doc = parser.parseFromString(data, 'text/html');
        const scripts = doc.querySelectorAll('script');
        scripts.forEach(script => {
            runInContext(script.textContent, false);

        });
        appendSection(handleConditions(md.render(data)));
    })
    .catch(error => {
        appendSection(`Fehler: Seite ${page} nicht gefunden. (${error.message})`);
    });
}
