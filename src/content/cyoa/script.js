import markdownit from 'https://cdn.jsdelivr.net/npm/markdown-it@14/+esm';
import markdownitAttrs from 'https://cdn.jsdelivr.net/npm/markdown-it-attrs@4/+esm';
import { Graphviz} from 'https://cdn.jsdelivr.net/npm/@hpcc-js/wasm/dist/graphviz.min.js';

const md = markdownit({ html: true, typographer: true }).use(markdownitAttrs);

const lz = {
    compress: LZString.compressToEncodedURIComponent,
    decompress: LZString.decompressFromEncodedURIComponent
};

const el = {
    html: document.querySelector('html'),
    body: document.querySelector('body'),
    devPane: document.getElementById('dev_pane'),
    graphContainer: document.getElementById('graph-container'),
    stateContainer: document.getElementById('state-container'),
    divider: document.getElementById('divider'),
    gamePane: document.getElementById('game_pane'),
    content: document.getElementById('content'),
}

let history = [];
let context = {};

let devMode = (window.location.port.length > 0) || (window.location.search.indexOf('dev') > 0);
let printAnchor = el.content;
let nextPageLinks = {};

function mulberry32(seed) {
    let t = seed >>> 0;
    return function() {
        t += 0x6D2B79F5;
        let r = Math.imul(t ^ (t >>> 15), t | 1);
        r ^= r + Math.imul(r ^ (r >>> 7), r | 61);
        return ((r ^ (r >>> 14)) >>> 0) / 4294967296;
    };
}

function present_choice(choices) {
    return new Promise((resolve) => {
        let ul = document.createElement('ul');
        printAnchor.appendChild(ul);
        for (let i = 0; i < choices.length; i++) {
            let li = document.createElement('li');
            ul.appendChild(li);
            let button = document.createElement('button');
            button.classList.add('pagelink');
            li.appendChild(button);
            button.innerHTML = choices[i];
            button.addEventListener('click', function(event) {
                event.preventDefault();
                event.stopPropagation();
                ul.remove();
                resolve(i);
            });
        }
        scrollToBottom();
    });
}

const contextProxy = new Proxy(context, {
    has(target, key) {
        return true;
    },
    get(target, key) {
        if (key in target) {
            return target[key];
        } else if (key === 'print') {
            return function(...args) {
                let div = document.createElement('div');
                div.innerHTML = args.map((x) => md.render(x)).join(' ') + '\n';
                printAnchor.appendChild(div);
                scrollToBottom();
            };
        } else if (key === 'present_choice') {
            return present_choice;
        } else {
            return globalThis[key];
        }
    },
    set(target, key, value) {
        target[key] = value;
        return true;
    }
});

function runInContext(code) {
    let result = Function('ctx', `with (ctx) { ${code} }`)(contextProxy);
    el.stateContainer.innerHTML = JSON.stringify(context, null, 2);
    return result;
}

function replaceDoubleBrackets(node) {
    if (node.nodeName.toLowerCase() === 'script') {
      return;
    }

    if (node.nodeType === Node.TEXT_NODE) {
      const pattern = /\[\[\s*(.+?)\s*\]\]/g;
      let match;
      const parent = node.parentNode;
      let lastIndex = 0;
      const fragment = document.createDocumentFragment();

      while ((match = pattern.exec(node.textContent)) !== null) {
        if (match.index > lastIndex) {
          fragment.appendChild(document.createTextNode(node.textContent.slice(lastIndex, match.index)));
        }

        const span = document.createElement('span');
        span.setAttribute('expression', match[1]);
        fragment.appendChild(span);

        lastIndex = pattern.lastIndex;
      }

      if (lastIndex < node.textContent.length) {
        fragment.appendChild(document.createTextNode(node.textContent.slice(lastIndex)));
      }

      if (fragment.childNodes.length > 0) {
        parent.replaceChild(fragment, node);
      }
    }

    for (const child of Array.from(node.childNodes)) {
        replaceDoubleBrackets(child);
    }
}

function processDOM(inputRoot) {
    function evaluate(expr) {
        return Function(...Object.keys(contextProxy), `return (${expr});`)(...Object.values(contextProxy));
    }

    function checkCondition(node) {
        if (!node.hasAttribute('condition')) return true;
        try {
            return Boolean(evaluate(node.getAttribute('condition')));
        } catch {
            return false;
        }
    }

    // Recursive processor
    function processNode(node, outputParent) {
        if (node.nodeType === Node.TEXT_NODE) {
            outputParent.appendChild(document.createTextNode(node.nodeValue));
            return;
        }

        if (node.nodeType !== Node.ELEMENT_NODE) return;

        if (!checkCondition(node)) return; // Skip subtree

        if (node.hasAttribute('expression')) {
            const value = evaluate(node.getAttribute('expression'));
            const textNode = document.createTextNode(value);
            outputParent.appendChild(textNode);
            return;
        }

        if (node.tagName.toLowerCase() === 'script') {
            if (checkCondition(node)) {
                runInContext(node.textContent);
            }
            return;
        }

        const clone = document.createElement(node.tagName);
        // Copy attributes (except 'expression' and 'condition')
        for (const attr of node.attributes) {
            if (attr.name !== 'expression' && attr.name !== 'condition') {
                clone.setAttribute(attr.name, attr.value);
            }
        }

        outputParent.appendChild(clone);

        for (const child of node.childNodes) {
            processNode(child, clone);
        }
    }

    for (const child of inputRoot.childNodes) {
        processNode(child, el.content);
    }

    scrollToBottom();
}


async function appendPage(page) {
    el.html.scrollTop = 0;
    await fetch(`/pages/${page}.md`)
    .then(response => {
        if (!response.ok) {
            throw new Error('Network response was not ok');
        }
        return response.text();
    })
    .then(data => {
        const parser = new DOMParser();
        let html = md.render(data);
        let doc = parser.parseFromString('<div></div>' + html, 'text/html');
        replaceDoubleBrackets(doc);

        let count = 0;

        // collect all next page links for navigation
        nextPageLinks = {};
        for (let link of doc.querySelectorAll('a')) {
            let href = link.getAttribute('href');
            if (href.indexOf('/') < 0) {
                let page = href;
                nextPageLinks[page] = link;
            }
        }

        processDOM(doc.body);

        if (history.length > 1) {
            el.content.appendChild(document.createElement('hr'));
        }
        history.push(page);
        let slug = history.join(',');
        let compressed = lz.compress(slug);
        window.location.hash = compressed;

        for (let link of document.querySelectorAll('a')) {
            let href = link.getAttribute('href') ?? '';
            if (href.indexOf('/') < 0) {
                let page = link.getAttribute('href');
                link.removeAttribute('href');
                let parent = link.parentNode;
                if ((parent.tagName ?? '').toLowerCase() === 'li') link = parent;
                nextPageLinks[page] = link;
                link.classList.add('pagelink');
                link.style.height = `${link.scrollHeight + 2}px`;
                link.addEventListener('click', function(event) {
                    if (link.classList.contains('chosen')) return;
                    event.preventDefault();
                    turnToPage(page);
                });
            }
        }

        markNodesInGraph();
    })
    .catch(error => {
        appendSection(`Fehler: Seite ${page} nicht gefunden. (${error.message})`);
        throw error;
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
    let linkLabels = {};
    for (let link of dom.querySelectorAll('a')) {
        let href = link.getAttribute('href');
        if (href.indexOf('/') < 0) {
            links[href] = link;
            if (link.hasAttribute('label')) {
                let label = link.getAttribute('label');
                linkLabels[href] = label.trim();
                if (linkLabels[href].length === 0)
                    linkLabels[href] = link.innerHTML.trim();
            }
        }
    }
    return {
        group: group,
        summary: summary,
        links: Object.keys(links),
        linkLabels: linkLabels,
    }
}

function getColorForGroup(groupLabel) {
    // Simple hash of string to integer
    let hash = 0;
    for (let i = 0; i < groupLabel.length; i++) {
        hash = groupLabel.charCodeAt(i) + ((hash << 5) - hash);
    }
    const hue = Math.abs(hash) % 360;
    const saturation = 50;

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
        const hex = `#${r.toString(16).padStart(2, '0')}${g.toString(16).padStart(2, '0')}${b.toString(16).padStart(2, '0')}`;
        return hex;
    });
}

function markNodesInGraph() {
    if (!devMode) return;
    let graph = el.graphContainer;
    for (let e of graph.querySelectorAll('.node')) {
        e.classList.remove('active');
    }
    let lastPage = '';
    for (let page of history.slice(1)) {
        let node = graph.querySelector(`#node_${page}`);
        if (node) {
            node.classList.add('active');
        }
        let edge = graph.querySelector(`#edge_${lastPage}_${page}`);
        if (edge) {
            edge.classList.add('active');
        }
        lastPage = page;
    }
    // focusOnElement(graph.querySelector(`#node_${history[history.length - 1]}`));
    
}

function wordWrap(text, maxLength) {
    const words = text.split(' ');
    let line = '';
    let wrappedText = '';

    words.forEach(word => {
        if (line.length + word.length + 1 <= maxLength) {
            line += (line.length ? ' ' : '') + word;
        } else {
            wrappedText += line + '\n';
            line = word;
        }
    });
    wrappedText += line;

    return wrappedText;
}

async function turnToPage(page) {
    if (nextPageLinks[page]) {
        nextPageLinks[page].classList.add('chosen');
        for (let el of document.querySelectorAll('.pagelink')) {
            if (!el.classList.contains('chosen')) {
                el.classList.add('dismissed');
                el.addEventListener('transitionend', () => {
                    el.remove();
                }, { once: true });
            }
        }
        await appendPage(page);
    }
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
            let page = null;
            let pageData = null;
            try {
                page = await loadPage(pageCode);
                pageData = parsePage(page);
            } catch (e) {
                pageData = { links: [], missing: true};
            }
            pageData.group ??= '';
            subGraphs[pageData.group] ??= [];
            subGraphs[pageData.group].push(pageCode);
            pageData.summary ??= '';
            pageData.summary = `${pageCode}\n${pageData.summary}`;
            pageSummaries[pageCode] = pageData.summary;
            for (let link of pageData.links) {
                if (pageData.linkLabels[link]) {
                    dotLinks.push(`"${pageCode}" -> "${link}" [id="edge_${pageCode}_${link}", label="  ${wordWrap(pageData.linkLabels[link], 10)}"];`);
                } else {
                    dotLinks.push(`"${pageCode}" -> "${link}" [id="edge_${pageCode}_${link}"];`);
                }
                if (seenLinks[link]) continue;
                newWavefront[link] = true;
            }
        }
        wavefront = newWavefront;
    }
    let dot = "";
    dot += `digraph Adventure {
    rankdir="TB"
    graph [fontname="Arial", fontsize=11]
    node [shape=box, style=filled, fontname="Arial", fontsize=11, color="#000000"]
    edge [fontname="Arial", fontsize=11, penwidth=1, style="solid", color="#000000"]`;
    for (let group of Object.keys(subGraphs)) {
        if (group.length > 0) {
            const groupColor = getColorForGroup(group);
            dot += `subgraph cluster_${group.replace(/[^a-zA-Z0-9]/g, '_')} {
            label="${group}"
            labelloc="t"
            labeljust="l"
            style=filled
            color="${groupColor[0]}"
            fillcolor="${groupColor[2]}"
            node [style=filled, fillcolor="${groupColor[1]}", color="${groupColor[0]}"]
            ${subGraphs[group].map(page => `"${page}" [label="${wordWrap(pageSummaries[page] ?? '', 10).trim()}", id="node_${page}"]`).join('\n')}
        }`;
        } else {
            dot += `
            ${subGraphs[group].map(page => `"${page}" [label="${wordWrap(pageSummaries[page] ?? '', 10).trim()}", id="node_${page}", style=filled, fillcolor="#cccccc", color="#888888"]`).join('\n')}
            `;
        }
    }
    dot += dotLinks.join('\n');
    dot += `}`;
    Graphviz.load().then(graphviz => {
        const svg = graphviz.dot(dot);
        el.graphContainer.innerHTML = svg;
        el.graphContainer.querySelector('svg').removeAttribute('width');
        el.graphContainer.querySelector('svg').removeAttribute('height');
        for (let e of document.querySelectorAll('#graph-container svg title')) e.remove();
        markNodesInGraph();
        installPanAndZoomHandler(document.querySelector('#graph-container svg'));
        for (let e of document.querySelectorAll('svg g.node')) {
            e.addEventListener('click', async function(event) {
                let id = this.getAttribute('id');
                let page = id.substring(5);
                if (nextPageLinks[page]) {
                    // the node is one of the next pages, turn to that page
                    await turnToPage(page);
                } else {
                    // the node is in the history, turn to that page
                    let index = history.lastIndexOf(page);
                    if (index > 0 && index < history.length - 1) {
                        el.body.classList.add('skip-animations');
                        let new_history = history.slice(0, index + 1);
                        history = history.slice(0, 1);
                        markNodesInGraph();
                        el.content.innerHTML = '';
                        Math.random = mulberry32(parseInt(history[0]));
                        context = {};
                        runInContext('');
                        await appendPage('1');
                        for (let i = 2; i < new_history.length; i++) {
                            console.log('turning to page', new_history[i]);
                            await turnToPage(new_history[i]);
                        }
                        el.body.classList.remove('skip-animations');
                    }
                }
            });
        }

    });
}

document.addEventListener("DOMContentLoaded", async function () {
    el.body.classList.add('skip-animations');
    if (devMode) {
        el.body.classList.add('dev');
        document.querySelector('#bu_reset_game').addEventListener('click', function() {
            window.location = '/';
        });
        document.querySelector('#bu_fit_zoom').addEventListener('click', function() {
            resetViewBox();
        });
    }
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

    Math.w6 = () => Math.floor(Math.rand() * 6) + 1;
    if (history.length < 3) {
        // it's a new game because there's only the seed and the first page in the history
        let seed = randomSeed();
        history = [seed];
        let slug = history.join(',');
        let compressed = lz.compress(slug);
        window.location.hash = compressed;
        Math.random = mulberry32(seed);
        await appendPage('1');
    } else {
        // restore narrative
        let seed = parseInt(history[0]);
        Math.random = mulberry32(seed);
        let pages = history.slice(1);
        history = history.slice(0, 1);
        await appendPage('1');
        for (let i = 1; i < pages.length; i++) {
            await turnToPage(pages[i]);
        }
    }

    if (devMode) {
        loadGraph();
        el.stateContainer.innerHTML = JSON.stringify(context, null, 2);
        initPaneSlider();
    }
    el.body.classList.remove('skip-animations');
});

function scrollToBottom() {
    let options = {
        top: el.gamePane.scrollHeight,
        behavior: el.body.classList.contains('skip-animations') ? 'instant' : 'smooth',
    }
    el.gamePane.scrollTo(options);
}

function appendSection(text) {
    const section = document.createElement('div');
    section.classList.add('page');
    section.classList.add('hidden');
    section.innerHTML = md.render(text);
    content.appendChild(section);
    scrollToBottom();
    setTimeout(() => section.classList.remove('hidden'), 1);
}

let isPanning = false;
let startPoint = { x: 0, y: 0 };
let viewBox = { x: 0, y: 0, width: 0, height: 0 };
let currentScale = 1;

let touchStartDistance = 0;
let isTouching = false;

function handlePan(e) {
    if (!isPanning) return;

    const dx = (e.clientX - startPoint.x) / currentScale; // Scale-aware
    const dy = (e.clientY - startPoint.y) / currentScale;

    viewBox.x -= dx;
    viewBox.y -= dy;

    updateViewBox();
    startPoint = { x: e.clientX, y: e.clientY }; // Update for next move
}

function startPan(e) {
    isPanning = true;
    startPoint = { x: e.clientX, y: e.clientY };
}

function pan(e) {
    if (!isPanning) return;

    const dx = (e.clientX - startPoint.x) / currentScale; // Scale-corrected
    const dy = (e.clientY - startPoint.y) / currentScale;

    viewBox.x -= dx;
    viewBox.y -= dy;

    updateViewBox();
    startPoint = { x: e.clientX, y: e.clientY };
}

function endPan() {
    isPanning = false;
}

function zoom(e) {
    e.preventDefault();
    const zoomIntensity = 0.1;
    const wheelDelta = -e.deltaY; // Invert for natural scrolling
    const zoomFactor = wheelDelta > 0 ? 1 - zoomIntensity : 1 + zoomIntensity;

    // Get mouse position in SVG coordinates
    const mouseX = e.clientX - window.svg.getBoundingClientRect().left;
    const mouseY = e.clientY - window.svg.getBoundingClientRect().top;
    const mouseXPercent = mouseX / window.svg.clientWidth;
    const mouseYPercent = mouseY / window.svg.clientHeight;

    // Store previous dimensions
    const prevWidth = viewBox.width;
    const prevHeight = viewBox.height;

    // Apply zoom
    viewBox.width *= zoomFactor;
    viewBox.height *= zoomFactor;

    // Adjust viewBox origin to zoom toward mouse
    viewBox.x += mouseXPercent * (prevWidth - viewBox.width);
    viewBox.y += mouseYPercent * (prevHeight - viewBox.height);

    updateViewBox();
}

function handleTouchStart(e) {
    if (e.touches.length === 2) {
        // Pinch-to-zoom
        touchStartDistance = getDistance(e.touches[0], e.touches[1]);
        isTouching = true;
    } else if (e.touches.length === 1) {
        // Single-touch pan
        startPan({ clientX: e.touches[0].clientX, clientY: e.touches[0].clientY });
        isTouching = true;
    }
}

function handleTouchMove(e) {
    if (!isTouching) return;
    e.preventDefault();

    if (e.touches.length === 2) {
        // Pinch-to-zoom
        const currentDistance = getDistance(e.touches[0], e.touches[1]);
        const zoomFactor = currentDistance / touchStartDistance;
        const midX = (e.touches[0].clientX + e.touches[1].clientX) / 2;
        const midY = (e.touches[0].clientY + e.touches[1].clientY) / 2;

        zoom({
            deltaY: zoomFactor < 1 ? 1 : -1,
            clientX: midX,
            clientY: midY,
            preventDefault: () => {}
        });
        touchStartDistance = currentDistance;
    } else if (e.touches.length === 1) {
        // Single-touch pan
        pan({ clientX: e.touches[0].clientX, clientY: e.touches[0].clientY });
    }
}

function handleTouchEnd(e) {
    isTouching = false;
    endPan(); // Reset panning state if needed
}


function getDistance(touch1, touch2) {
    return Math.hypot(
        touch2.clientX - touch1.clientX,
        touch2.clientY - touch1.clientY
    );
}

function updateViewBox() {
    window.svg.setAttribute('viewBox', `${viewBox.x} ${viewBox.y} ${viewBox.width} ${viewBox.height}`);
    currentScale = window.svg.clientWidth / viewBox.width; // Update scale
}

function resetViewBox() {
    const bbox = window.svg.getBBox();
    const padding = 10; // Optional padding
    const maxZoom = 1.5; // Maximum allowed zoom level (2x)

    // Get container dimensions
    const containerWidth = window.svg.clientWidth;
    const containerHeight = window.svg.clientHeight;

    // Calculate minimum allowed viewBox dimensions based on maxZoom
    const minWidth = containerWidth / maxZoom;
    const minHeight = containerHeight / maxZoom;

    // Calculate initial fit
    const containerRatio = containerWidth / containerHeight;
    const contentRatio = bbox.width / bbox.height;

    if (contentRatio > containerRatio) {
        // Fit to width
        viewBox.width = Math.max(bbox.width + padding * 2, minWidth);
        viewBox.height = viewBox.width / containerRatio;
    } else {
        // Fit to height
        viewBox.height = Math.max(bbox.height + padding * 2, minHeight);
        viewBox.width = viewBox.height * containerRatio;
    }

    // Center content
    viewBox.x = bbox.x - (viewBox.width - bbox.width) / 2;
    viewBox.y = bbox.y - (viewBox.height - bbox.height) / 2;

    updateViewBox();
}

let animationFrameId = null;

function animateViewBox(target, duration = 300) {
    const start = { ...viewBox };
    const end = { ...target };
    const startTime = performance.now();

    if (animationFrameId) {
        cancelAnimationFrame(animationFrameId);
    }

    function step(now) {
        const elapsed = now - startTime;
        const t = Math.min(elapsed / duration, 1); // Clamp to [0,1]
        const ease = easeInOutQuad(t);

        // Interpolate each property
        viewBox.x = lerp(start.x, end.x, ease);
        viewBox.y = lerp(start.y, end.y, ease);
        viewBox.width = lerp(start.width, end.width, ease);
        viewBox.height = lerp(start.height, end.height, ease);

        updateViewBox();

        if (t < 1) {
            animationFrameId = requestAnimationFrame(step);
        } else {
            animationFrameId = null; // Done
        }
    }

    requestAnimationFrame(step);
}

// Helpers
function lerp(a, b, t) {
    return a + (b - a) * t;
}

function easeInOutQuad(t) {
    return t < 0.5 ? 2 * t * t : -1 + (4 - 2 * t) * t;
}

function getBBoxInSVGCoords(element, svg) {
    const bbox = element.getBBox();
    const matrix = element.getCTM();

    const points = [
        svg.createSVGPoint(), svg.createSVGPoint(),
        svg.createSVGPoint(), svg.createSVGPoint()
    ];

    points[0].x = bbox.x;               points[0].y = bbox.y;
    points[1].x = bbox.x + bbox.width;  points[1].y = bbox.y;
    points[2].x = bbox.x;               points[2].y = bbox.y + bbox.height;
    points[3].x = bbox.x + bbox.width;  points[3].y = bbox.y + bbox.height;

    const transformedPoints = points.map(p => p.matrixTransform(matrix));

    const xs = transformedPoints.map(p => p.x);
    const ys = transformedPoints.map(p => p.y);
    const minX = Math.min(...xs);
    const maxX = Math.max(...xs);
    const minY = Math.min(...ys);
    const maxY = Math.max(...ys);

    return {
        x: minX,
        y: minY,
        width: maxX - minX,
        height: maxY - minY
    };
}

function isElementWellInView(elementBBox, margin = 20) {
    const boxLeft = elementBBox.x - margin;
    const boxRight = elementBBox.x + elementBBox.width + margin;
    const boxTop = elementBBox.y - margin;
    const boxBottom = elementBBox.y + elementBBox.height + margin;

    const viewLeft = viewBox.x;
    const viewRight = viewBox.x + viewBox.width;
    const viewTop = viewBox.y;
    const viewBottom = viewBox.y + viewBox.height;

    return (
        boxLeft >= viewLeft &&
        boxRight <= viewRight &&
        boxTop >= viewTop &&
        boxBottom <= viewBottom
    );
}

function focusOnElement(element) {
    if (window.svg === undefined || element === null) return;
    console.log('focusing on element', element);
    let graphGroup = window.svg.querySelector('g.graph');
    const svg = window.svg;
    const containerWidth = svg.clientWidth;
    const containerHeight = svg.clientHeight;
    const containerRatio = containerWidth / containerHeight;

    const elementBBox = getBBoxInSVGCoords(element, svg);
    const graphBBox = getBBoxInSVGCoords(graphGroup, svg);

    const padding = 10;       // Around graph or focused element
    const focusMargin = 40;   // Space above focused element
    const maxZoom = 1.5;

    // Calculate minimum allowed viewBox dimensions based on maxZoom
    const minWidth = containerWidth / maxZoom;
    const minHeight = containerHeight / maxZoom;

    // Smart skip: If element is already nicely visible, don't move
    if (isElementWellInView(elementBBox)) {
        return;
    }

    // Determine if whole graph is small enough to center instead
    const fitsHorizontally = graphBBox.width + padding * 2 <= minWidth;
    const fitsVertically = graphBBox.height + padding * 2 <= minHeight;
    const graphIsSmall = fitsHorizontally && fitsVertically;

    let targetViewBox = {};

    if (graphIsSmall) {
        // Center whole graph
        const contentRatio = graphBBox.width / graphBBox.height;
        if (contentRatio > containerRatio) {
            // Fit to width
            targetViewBox.width = Math.max(graphBBox.width + padding * 2, minWidth);
            targetViewBox.height = targetViewBox.width / containerRatio;
        } else {
            // Fit to height
            targetViewBox.height = Math.max(graphBBox.height + padding * 2, minHeight);
            targetViewBox.width = targetViewBox.height * containerRatio;
        }

        targetViewBox.x = graphBBox.x - (targetViewBox.width - graphBBox.width) / 2;
        targetViewBox.y = graphBBox.y - (targetViewBox.height - graphBBox.height) / 2;
    } else {
        // Focus on element — keep near top
        const focusWidth = Math.max(elementBBox.width + padding * 2, minWidth);
        const focusHeight = Math.max(elementBBox.height + padding * 2, minHeight);

        let viewWidth, viewHeight;
        if (focusWidth / focusHeight > containerRatio) {
            // Fit to width
            viewWidth = focusWidth;
            viewHeight = viewWidth / containerRatio;
        } else {
            // Fit to height
            viewHeight = focusHeight;
            viewWidth = viewHeight * containerRatio;
        }

        const centerX = elementBBox.x + elementBBox.width / 2;
        const viewX = centerX - viewWidth / 2;
        const viewY = elementBBox.y - focusMargin;

        targetViewBox = {
            x: viewX,
            y: viewY,
            width: viewWidth,
            height: viewHeight
        };
    }

    // viewBox = targetViewBox;
    // updateViewBox();

    animateViewBox(targetViewBox);
}

function installPanAndZoomHandler(svg) {
    window.svg = svg;

    // Initialize viewBox to default
    resetViewBox();

    // Mouse/touch events
    window.svg.addEventListener('mousedown', (e) => {
        if (e.button !== 0) return; // Only left button
        isPanning = true;
        startPoint = { x: e.clientX, y: e.clientY };
        e.preventDefault(); // Prevent text selection during drag
    });

    // Track mouse movement globally
    document.addEventListener('mousemove', handlePan);
    document.addEventListener('mouseup', (e) => {
        if (e.button !== 0) return; // Only left button
        isPanning = false;
    });

    // Handle SVG-specific mouseleave (optional cleanup)
    window.svg.addEventListener('mouseleave', (e) => {
        if (!isPanning) return; // Only reset if not panning
    });
    window.svg.addEventListener('wheel', zoom);

    // Touch events (for mobile)
    window.svg.addEventListener('touchstart', handleTouchStart, { passive: false });
    window.svg.addEventListener('touchmove', handleTouchMove, { passive: false });
    window.svg.addEventListener('touchend', handleTouchEnd);
}

function initPaneSlider() {
    const rightPanel = el.gamePane;
    const container = document.querySelector('#resizable-children');
    let isDragging = false;

    // Initialize with saved percentage or default
    function initPanel() {
        const savedPercent = localStorage.getItem('rightPanelPercent');
        const containerWidth = container.clientWidth;

        if (savedPercent) {
            const percent = parseFloat(savedPercent);
            rightPanel.style.width = `${Math.min(Math.max(percent, 20), 50)}%`;
        } else {
            rightPanel.style.width = '30%'; // Default
        }
    }

    // Handle divider drag
    el.divider.addEventListener('mousedown', (e) => {
        isDragging = true;
        document.body.style.cursor = 'col-resize';
        document.addEventListener('mousemove', onMouseMove);
        document.addEventListener('mouseup', onMouseUp);
        e.preventDefault();
    });

    function onMouseMove(e) {
        if (!isDragging) return;

        const containerRect = container.getBoundingClientRect();
        const newWidthPx = containerRect.right - e.clientX;
        const containerWidth = containerRect.width;
        const newPercent = (newWidthPx / containerWidth) * 100;

        // Apply constraints (20% to 50%)
        rightPanel.style.width = `${Math.min(Math.max(newPercent, 20), 50)}%`;
    }

    function onMouseUp() {
        isDragging = false;
        document.body.style.cursor = '';
        document.removeEventListener('mousemove', onMouseMove);
        document.removeEventListener('mouseup', onMouseUp);

        // Save percentage
        const containerWidth = container.clientWidth;
        const rightPanelWidth = rightPanel.clientWidth;
        const percent = (rightPanelWidth / containerWidth) * 100;
        localStorage.setItem('rightPanelPercent', percent);
    }

    // Handle window resize
    function handleResize() {
        const savedPercent = localStorage.getItem('rightPanelPercent');
        if (savedPercent) {
            const percent = parseFloat(savedPercent);
            rightPanel.style.width = `${Math.min(Math.max(percent, 20), 50)}%`;
        }
    }

    // Initialize
    initPanel();
    window.addEventListener('resize', handleResize);
}