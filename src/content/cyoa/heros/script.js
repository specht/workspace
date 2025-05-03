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
        t += 0x6D2B79F5;
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

let devMode = window.location.port.length > 0;

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
    let result = Function('ctx', `with (ctx) { ${wrappedCode} }`)(contextProxy);
    document.querySelector('#state-container').innerHTML = JSON.stringify(context, null, 2);
    return result;
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
            if (count > 10000) {
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
    let graph = document.querySelector('#graph-container');
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
                dotLinks.push(`"${pageCode}" -> "${link}" [id="edge_${pageCode}_${link}"];`);
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
            ${subGraphs[group].map(page => `"${page}" [label="${pageSummaries[page]}", id="node_${page}"]`).join('\n')}
        }`;
        } else {
            dot += `
            ${subGraphs[group].map(page => `"${page}" [label="${pageSummaries[page]}", id="node_${page}", style=filled, fillcolor="#cccccc", color="#888888"]`).join('\n')}
            `;
        }
    }
    dot += dotLinks.join('\n');
    dot += `}`;
    Graphviz.load().then(graphviz => {
        const svg = graphviz.dot(dot);
        document.querySelector('#graph-container').innerHTML = svg;
        document.querySelector('#graph-container svg').removeAttribute('width');
        document.querySelector('#graph-container svg').removeAttribute('height');
        for (let e of document.querySelectorAll('#graph-container svg title')) e.remove();
        markNodesInGraph();
        installPanAndZoomHandler(document.querySelector('#graph-container svg'));
    });
}

document.addEventListener("DOMContentLoaded", async function () {
    if (devMode) {
        document.querySelector('body').classList.add('dev');
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

    if (devMode) {
        loadGraph();
        document.querySelector('#state-container').innerHTML = JSON.stringify(context, null, 2);
        initPaneSlider();
    }
});

function appendSection(text) {
    const section = document.createElement('div');
    section.classList.add('page');
    section.classList.add('hidden');
    section.innerHTML = md.render(text);
    content.appendChild(section);
    setTimeout(() => section.classList.remove('hidden'), 100);
}

function installPanAndZoomHandler(svg) {
    let isPanning = false;
    let startPoint = { x: 0, y: 0 };
    let viewBox = { x: 0, y: 0, width: 0, height: 0 };
    let currentScale = 1;

    let touchStartDistance = 0;
    let isTouching = false;

    // Initialize viewBox to default
    resetViewBox();

    // Mouse/touch events
    svg.addEventListener('mousedown', (e) => {
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
    svg.addEventListener('mouseleave', (e) => {
        if (!isPanning) return; // Only reset if not panning
    });
    svg.addEventListener('wheel', zoom);

    // Touch events (for mobile)
    svg.addEventListener('touchstart', handleTouchStart, { passive: false });
    svg.addEventListener('touchmove', handleTouchMove, { passive: false });
    svg.addEventListener('touchend', handleTouchEnd);

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
        const mouseX = e.clientX - svg.getBoundingClientRect().left;
        const mouseY = e.clientY - svg.getBoundingClientRect().top;
        const mouseXPercent = mouseX / svg.clientWidth;
        const mouseYPercent = mouseY / svg.clientHeight;

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
        svg.setAttribute('viewBox', `${viewBox.x} ${viewBox.y} ${viewBox.width} ${viewBox.height}`);
        currentScale = svg.clientWidth / viewBox.width; // Update scale
    }

    function resetViewBox() {
        const bbox = svg.getBBox();
        const padding = 10; // Optional padding
        const maxZoom = 1.5; // Maximum allowed zoom level (2x)

        // Get container dimensions
        const containerWidth = svg.clientWidth;
        const containerHeight = svg.clientHeight;

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
}

function initPaneSlider() {
    const divider = document.getElementById('divider');
    const rightPanel = document.querySelector('#game_pane');
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
    divider.addEventListener('mousedown', (e) => {
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