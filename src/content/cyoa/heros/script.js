import markdownit from 'https://cdn.jsdelivr.net/npm/markdown-it@14/+esm';
import markdownitAttrs from 'https://cdn.jsdelivr.net/npm/markdown-it-attrs@4/+esm';
import { Graphviz} from 'https://cdn.jsdelivr.net/npm/@hpcc-js/wasm/dist/graphviz.min.js';

const dot = `digraph {
    A -> B;
    B -> C;
    C -> A;
}`;

function mulberry32(seed) {
    let t = seed >>> 0;
    return function() {
        t += 0x6D2B79F5;e
        let r = Math.imul(t ^ (t >>> 15), t | 1);
        r ^= r + Math.imul(r ^ (r >>> 7), r | 61);
        return ((r ^ (r >>> 14)) >>> 0) / 4294967296; // [0,1)
    };
}

const md = markdownit({ html: true }).use(markdownitAttrs);
const content = document.getElementById('content');

const lz = {
    compress: LZString.compressToEncodedURIComponent,
    decompress: LZString.decompressFromEncodedURIComponent
};

let history = [];
let context = {};

let devMode = false;

const contextProxy = new Proxy(context, {
    has(target, key) {
        return true;
    },
    get(target, key) {
        if (key in target) {
            return target[key];
        } else {
            return globalThis[key];
        }
    },
    set(target, key, value) {
        target[key] = value;
        return true;
    }
});

function runInContext(code, isCondition = false) {
    const wrappedCode = isCondition ? `return (${code});` : `${code}`;
    return Function('ctx', `with (ctx) { ${wrappedCode} }`)(contextProxy);
}

async function appendPage(page) {
    await fetch(`/pages/${page}.md`)
    .then(response => {
        if (!response.ok) {
            throw new Error('Network response was not ok');
        }
        return response.text();
    })
    .then(data => {
        const parser = new DOMParser();
        let doc = parser.parseFromString(md.render(data), 'text/html');
        let count = 0;

        while (true) {
            count += 1;
            if (count > 100) {
                console.error('Infinite loop detected');
                break;
            }
            let xpathResult = doc.evaluate('//script | //*[@condition] | //*[@expression]', doc, null, XPathResult.ORDERED_NODE_ITERATOR_TYPE, null);
            try {
                let element;
                while (element = xpathResult.iterateNext()) {
                    if (element.hasAttribute('__handled')) {
                        continue;
                    }
                    if (element.hasAttribute('condition')) {
                        let condition = element.getAttribute('condition');
                        let value = runInContext(condition, true);
                        element.setAttribute('__handled', 'true');
                        if (!value) {
                            element.remove();
                            continue;
                        }
                    }
                    if (element.tagName === 'SCRIPT') {
                        let code = element.textContent;
                        runInContext(code, false);
                        element.setAttribute('__handled', 'true');
                    } else if (element.hasAttribute('expression')) {
                        let expression = element.getAttribute('expression');
                        let value = runInContext(expression, true);
                        element.setAttribute('__handled', 'true');
                        element.innerHTML = value;
                    }
                }
                break;
            } catch (e) {
                if (e.name !== 'InvalidStateError') throw e;
            }
        }

        if (history.length > 1) {
            content.appendChild(document.createElement('hr'));
        }
        history.push(page);
        let slug = history.join(',');
        let compressed = lz.compress(slug);
        window.location.hash = compressed;

        appendSection(doc.body.innerHTML);

        for (let link of document.querySelectorAll('a')) {
            if (link.getAttribute('__handled')) {
                continue;
            }
            let href = link.getAttribute('href');
            if (href.indexOf('/') < 0) {
                link.setAttribute('__handled', 'true');
                let page = link.getAttribute('href');
                link.setAttribute('href', '#');
                link.removeAttribute('href');
                let parent = link.parentNode;
                if (parent.tagName === 'LI') link = parent;
                link.classList.add('pagelink');
                link.style.height = `${link.scrollHeight}px`;
                link.addEventListener('click', function(event) {
                    if (link.classList.contains('chosen')) {
                        return;
                    }
                    event.preventDefault();
                    link.classList.add('chosen');
                    for (let el of document.querySelectorAll('.pagelink')) {
                        if (!el.classList.contains('chosen')) {
                            el.classList.add('dismissed');
                            el.addEventListener('transitionend', () => {
                                el.remove();
                            }, { once: true });
                        }
                    }
                    appendPage(page);
                });
            }
        }
    })
    .catch(error => {
        appendSection(`Fehler: Seite ${page} nicht gefunden. (${error.message})`);
    });
}

function randomSeed() {
    const array = new Uint32Array(1);
    crypto.getRandomValues(array);
    return array[0];
}

async function loadPage(page) {
    const response = await fetch(`/pages/${page}.md`);
    if (!response.ok) {
        throw new Error('Network response was not ok');
    }
    return response.text();
}

function parsePage(text) {
    let group = null;
    let summary = null;

    let match = text.match(/<!--\s*(.*?)\s*--\s*(.*?)\s*-->/);
    if (match) {
        group = match[1].trim();
        summary = match[2].trim();
    } else {
        match = text.match(/<!--\s*(.*?)\s*-->/);
        if (match) {
            group = match[1].trim();
        }
    }
    let html = md.render(text);
    let dom = new DOMParser().parseFromString(html, 'text/html');
    let links = {};
    for (let link of dom.querySelectorAll('a')) {
        let href = link.getAttribute('href');
        if (href.indexOf('/') < 0) {
            links[href] = link;
        }
    }
    return {
        group: group,
        summary: summary,
        links: Object.keys(links),
    }
}

function getColorForGroup(groupLabel) {
    // Simple hash of string to integer
    let hash = 0;
    for (let i = 0; i < groupLabel.length; i++) {
        hash = groupLabel.charCodeAt(i) + ((hash << 5) - hash);
    }
    const hue = Math.abs(hash) % 360;
    const saturation = 50; // 50%

    return [50, 70, 90].map(function(lightness) {
        // Convert HSL to RGB
        function hslToRgb(h, s, l) {
            s /= 100;
            l /= 100;
            const k = n => (n + h / 30) % 12;
            const a = s * Math.min(l, 1 - l);
            const f = n => {
                const v = l - a * Math.max(-1, Math.min(k(n) - 3, Math.min(9 - k(n), 1)));
                return Math.round(255 * v);
            };
            return [f(0), f(8), f(4)];
        }

        const [r, g, b] = hslToRgb(hue, saturation, lightness);
        const hex = `#${r.toString(16).padStart(2, '0')}${g
            .toString(16)
            .padStart(2, '0')}${b.toString(16).padStart(2, '0')}`;

        return hex;

    });
}

async function loadGraph() {
    let seenLinks = {};
    let wavefront = {};
    wavefront['1'] = true;
    let dotLinks = [];
    let subGraphs = {};
    let pageSummaries = {};
    while (Object.keys(wavefront).length > 0) {
        let newWavefront = {};
        for (let pageCode of Object.keys(wavefront)) {
            seenLinks[pageCode] = true;
            let page = await loadPage(pageCode);
            let pageData = parsePage(page);
            pageData.group ??= '';
            subGraphs[pageData.group] ??= [];
            subGraphs[pageData.group].push(pageCode);
            pageData.summary ??= '';
            pageData.summary = `${pageCode}\n${pageData.summary}`;
            pageSummaries[pageCode] = pageData.summary;
            for (let link of pageData.links) {
                dotLinks.push(`"${pageCode}" -> "${link}";`);
                if (seenLinks[link]) continue;
                newWavefront[link] = true;
            }
        }
        wavefront = newWavefront;
    }
    let dot = "";
    dot += `digraph Adventure {
        rankdir="TB"
        graph [fontname="Arial"]
        node [shape=box, style=filled, fontname="Arial", fontsize=11]
        edge [fontname="Arial"]`;
    for (let group of Object.keys(subGraphs)) {
        if (group.length > 0) {
            const groupColor = getColorForGroup(group);
            dot += `subgraph cluster_${group} {
                label="${group}"
                style=filled
                color="${groupColor[0]}"
                fillcolor="${groupColor[2]}"
                node [style=filled, fillcolor="${groupColor[1]}"]
                ${subGraphs[group].map(page => `"${page}" [label="${pageSummaries[page]}"]`).join('\n')}
            }`;
            } else {
            dot += `
                ${subGraphs[group].map(page => `"${page}" [label="${pageSummaries[page]}"]`).join('\n')}
                `;
        }
    }
    dot += dotLinks.join('\n');
    dot += `}`;
    Graphviz.load().then(graphviz => {
        const svg = graphviz.dot(dot);
        document.querySelector('#graph-container').innerHTML = svg;
    });
}

document.addEventListener("DOMContentLoaded", async function () {
    if (window.location.hash) {
        const hash = window.location.hash.substring(1);
        if (hash) {
            const decompressed = lz.decompress(hash);
            if (decompressed) {
                try {
                    history = decompressed.split(',');
                } catch (e) {
                }
            }
        }
    }

    if (history.length < 3) {
        let seed = randomSeed();
        history = [seed];
        let slug = history.join(',');
        let compressed = lz.compress(slug);
        window.location.hash = compressed;
        Math.rand = mulberry32(seed);
        Math.chance = (chance) => (Math.rand() * 100 < chance);
        appendPage(1);
    } else {
        // restore narrative
        let seed = parseInt(history[0]);
        Math.rand = mulberry32(seed);
        Math.chance = (chance) => (Math.rand() * 100 < chance);
        let pages = history.slice(1);
        history = history.slice(0, 1);
        for (let i = 0; i < pages.length; i++) {
            let page = pages[i];
            if (page) {
                await appendPage(page);
            }
        }
    }

    loadGraph();
});

function appendSection(text) {
    const section = document.createElement('div');
    section.classList.add('page');
    section.classList.add('hidden');
    section.innerHTML = md.render(text);
    content.appendChild(section);
    setTimeout(() => section.classList.remove('hidden'), 100);
}

function handleConditions(text) {
    const tempDiv = document.createElement('div');
    tempDiv.innerHTML = text;
    const elementsWithCondition = tempDiv.querySelectorAll('[condition]');
    elementsWithCondition.forEach(element => {
        let condition = element.getAttribute('condition');
        let value = runInContext(condition, true);
        if (!value) {
            element.remove();
            // element.style.display = 'none';
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

    fetch(`/pages/${page}.md`)
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
        throw error;
    });
}
