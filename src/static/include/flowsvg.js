class FlowSVG {
    constructor({
        container,
        width = 800,
        height = 400,
        viewBox = `0 0 ${width} ${height}`
    }) {
        this.ns = "http://www.w3.org/2000/svg";
        this.svg = document.createElementNS(this.ns, "svg");
        this.svg.setAttribute("viewBox", viewBox);
        this.svg.setAttribute("width", "100%");
        this.svg.setAttribute("height", "100%");
        this.svg.style.position = "absolute";
        this.svg.style.inset = "0";
        this.svg.style.pointerEvents = "none";

        if (typeof container === "string") {
            container = document.querySelector(container);
        }

        if (!container) {
            throw new Error("Container not found");
        }

        const computed = getComputedStyle(container);
        if (computed.position === "static") {
            container.style.position = "relative";
        }

        container.appendChild(this.svg);

        this.ensureDefs();
    }

    ensureDefs() {
        let defs = this.svg.querySelector("defs");
        if (!defs) {
            defs = document.createElementNS(this.ns, "defs");
            this.svg.appendChild(defs);
        }
        this.defs = defs;
    }

    createPath({
        points, // [x1, y1, x2, y2, ...]
        stroke = "#e5185d",
        strokeWidth = 2,
        dash = "12 8",
        duration = 1.5,
        opacity = 1,
        linecap = "round",
        className = ""
    }) {
        if (!Array.isArray(points)) {
            throw new Error("points must be an array of numbers");
        }

        const path = document.createElementNS(this.ns, "path");

        let d;

        if (points.length === 4) {
            // Line: [x1, y1, x2, y2]
            const [x1, y1, x2, y2] = points;
            d = `M ${x1} ${y1} L ${x2} ${y2}`;
        } else if (points.length === 6) {
            // Quadratic: [x1, y1, cx, cy, x2, y2]
            const [x1, y1, cx, cy, x2, y2] = points;
            d = `M ${x1} ${y1} Q ${cx} ${cy}, ${x2} ${y2}`;
        } else if (points.length === 8) {
            // Cubic: [x1, y1, c1x, c1y, c2x, c2y, x2, y2]
            const [x1, y1, c1x, c1y, c2x, c2y, x2, y2] = points;
            d = `M ${x1} ${y1} C ${c1x} ${c1y}, ${c2x} ${c2y}, ${x2} ${y2}`;
        } else {
            throw new Error("points must have length 4, 6, or 8");
        }

        path.setAttribute("d", d);
        path.setAttribute("fill", "none");
        path.setAttribute("stroke", stroke);
        path.setAttribute("stroke-width", strokeWidth);
        path.setAttribute("stroke-dasharray", dash);
        path.setAttribute("stroke-linecap", linecap);
        path.setAttribute("opacity", opacity);

        if (className) {
            path.setAttribute("class", className);
        }

        const dashLength = this.getDashCycleLength(dash);
        const animationName = `flowDash_${Math.random().toString(36).slice(2)}`;

        this.addKeyframes(animationName, dashLength);

        path.style.animation = `${animationName} ${duration}s linear infinite`;

        this.svg.appendChild(path);
        return path;
    }

    getDashCycleLength(dash) {
        return dash
            .split(/[ ,]+/)
            .map(Number)
            .filter(n => !isNaN(n))
            .reduce((sum, n) => sum + n, 0) || 20;
    }

    addKeyframes(name, dashOffset) {
        if (!document.getElementById(name)) {
            const style = document.createElement("style");
            style.id = name;
            style.textContent = `
        @keyframes ${name} {
          from { stroke-dashoffset: 0; }
          to { stroke-dashoffset: -${dashOffset}; }
        }
      `;
            document.head.appendChild(style);
        }
    }
}