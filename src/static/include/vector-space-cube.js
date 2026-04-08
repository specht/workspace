export class VectorSpaceCube {
    constructor(options = {}) {
        const {
            svg,
            labelEl = null,
            state = null,
            value = null,
            basis = null,
            onChange = null,
            background = { from: '#191b22', to: '#1b1d24' },
            showDragPlaneFill = true,
            dragPlaneFillResolution = 220,
            dragPlaneFillOpacity = 0.5,
            showDragPlaneGrid = true,
            dragPlaneGridStep = 0.1,
            dragPlaneGridOpacity = 0.18,
            dragPlaneGridMajorOpacity = 0.34
        } = options;

        if (!svg) {
            throw new Error('VectorSpaceCube requires an SVG element.');
        }

        this.svg = svg;
        this.labelEl = labelEl;
        this.onChange = typeof onChange === 'function' ? onChange : null;
        this.background = background;
        this.showDragPlaneFill = showDragPlaneFill;
        this.dragPlaneFillResolution = dragPlaneFillResolution;
        this.dragPlaneFillOpacity = dragPlaneFillOpacity;
        this.showDragPlaneGrid = showDragPlaneGrid;
        this.dragPlaneGridStep = dragPlaneGridStep;
        this.dragPlaneGridOpacity = dragPlaneGridOpacity;
        this.dragPlaneGridMajorOpacity = dragPlaneGridMajorOpacity;

        this.state = {
            yaw: -0.65,
            pitch: 0.5,
            zoom: 1.0,
            minZoom: 0.55,
            maxZoom: 2.5,
            dragging: false,
            lastX: 0,
            lastY: 0,
            pinchDistance: null,
            mode: null,
            pointGrabRadius: 18,
            previewTriggerRadius: 256,
            pointHover: false,
            previewNear: false,
            pointDragPlane: null,
            dragPlaneDef: null,
            dragPlaneCache: null,
            dragPointerAnchor: null,
            dragWorldAnchor: null,
            previewOpacity: 0,
            previewTargetOpacity: 0,
            previewAnimFrame: null,
            ...(state || {})
        };

        this.basis = this.#normalizeBasis(
            basis || [
                { name: 'R', color: '#ff3b30', vector: { x: 1, y: 0, z: 0 } },
                { name: 'G', color: '#34c759', vector: { x: 0, y: 1, z: 0 } },
                { name: 'B', color: '#0a84ff', vector: { x: 0, y: 0, z: 1 } }
            ]
        );

        const initialRgb = this.#clampWorldPointToRgbCube(
            value || { x: 0.49, y: 0.8, z: 0.76 }
        );

        this.value = this.#worldToBasisCoefficients(initialRgb);
        this.boundHandlers = null;

        if (!this.svg.getAttribute('viewBox')) {
            this.svg.setAttribute('viewBox', '0 0 1000 700');
        }
        this.svg.style.touchAction = 'none';
        this.svg.style.userSelect = 'none';

        this.#setupInteraction();
        this.draw();
    }

    destroy() {
        if (!this.boundHandlers) return;

        if (this.state.previewAnimFrame != null) {
            cancelAnimationFrame(this.state.previewAnimFrame);
            this.state.previewAnimFrame = null;
        }

        const { onMouseDown, onMouseMove, onMouseUp, onTouchStart, onTouchMove, onTouchEnd, onWheel } = this.boundHandlers;
        this.svg.removeEventListener('mousedown', onMouseDown);
        window.removeEventListener('mousemove', onMouseMove);
        window.removeEventListener('mouseup', onMouseUp);

        this.svg.removeEventListener('touchstart', onTouchStart);
        window.removeEventListener('touchmove', onTouchMove);
        window.removeEventListener('touchend', onTouchEnd);
        window.removeEventListener('touchcancel', onTouchEnd);

        this.svg.removeEventListener('wheel', onWheel);
        this.svg.removeEventListener('mousemove', onMouseMove);

        this.boundHandlers = null;
    }

    setValue(nextValue, { silent = false } = {}) {
        const clampedWorld = this.#clampWorldPointToRgbCube(nextValue);
        this.value = this.#worldToBasisCoefficients(clampedWorld);
        this.draw();
        if (!silent) this.#emitChange();
    }

    getValue() {
        return this.#clampWorldPointToRgbCube(this.#currentWorldPoint());
    }

    getCoefficients() {
        return { ...this.value };
    }

    setCoefficients(nextCoeffs, { silent = false } = {}) {
        const world = this.#mixToWorld(nextCoeffs);
        const clampedWorld = this.#clampWorldPointToRgbCube(world);
        this.value = this.#worldToBasisCoefficients(clampedWorld);
        this.draw();
        if (!silent) this.#emitChange();
    }

    setBasis(nextBasis) {
        const worldBefore = this.#currentWorldPoint();
        this.basis = this.#normalizeBasis(nextBasis);
        const clampedWorld = this.#clampWorldPointToRgbCube(worldBefore);
        this.value = this.#worldToBasisCoefficients(clampedWorld);
        this.#invalidateDragPlaneCache();
        this.draw();
        this.#emitChange();
    }

    getBasis() {
        return this.basis.map((b) => ({
            name: b.name,
            color: b.color,
            vector: { ...b.vector }
        }));
    }

    setBasisVector(index, nextVector) {
        if (index < 0 || index > 2) {
            throw new Error('Basis vector index must be 0, 1, or 2.');
        }

        const current = this.getBasis();
        current[index] = {
            ...current[index],
            ...nextVector,
            vector: {
                x: Number(nextVector?.vector?.x ?? current[index].vector.x),
                y: Number(nextVector?.vector?.y ?? current[index].vector.y),
                z: Number(nextVector?.vector?.z ?? current[index].vector.z)
            }
        };

        this.setBasis(current);
    }

    updateBasis(mutator) {
        const next = mutator(this.getBasis());
        this.setBasis(next);
    }

    resize() {
        this.#invalidateDragPlaneCache();
        this.draw();
    }

    #setPreviewVisible(visible) {
        const target = visible ? 0.85 : 0;
        if (Math.abs(this.state.previewTargetOpacity - target) < 1e-4) return;

        this.state.previewTargetOpacity = target;
        this.#startPreviewOpacityAnimation();
    }

    #startPreviewOpacityAnimation() {
        if (this.state.previewAnimFrame != null) return;

        const step = () => {
            const current = this.state.previewOpacity;
            const target = this.state.previewTargetOpacity;
            const next = current + (target - current) * 0.22;

            if (Math.abs(next - target) < 0.01) {
                this.state.previewOpacity = target;
                this.state.previewAnimFrame = null;
                this.draw();
                return;
            }

            this.state.previewOpacity = next;
            this.draw();
            this.state.previewAnimFrame = requestAnimationFrame(step);
        };

        this.state.previewAnimFrame = requestAnimationFrame(step);
    }

    #midLabelPos(a, b, offset = 14) {
        const mx = (a.x + b.x) * 0.5;
        const my = (a.y + b.y) * 0.5;

        const dx = b.x - a.x;
        const dy = b.y - a.y;
        const len = Math.hypot(dx, dy) || 1;

        const px = -dy / len;
        const py = dx / len;

        return {
            x: mx + px * offset,
            y: my + py * offset
        };
    }

    #formatCoeff(name, v) {
        const text = v.toFixed(2);
        const pretty = text.startsWith('-') ? `−${text.slice(1)}` : text;
        return `${name} = ${pretty}`;
    }

    #segmentLength2D(a, b) {
        return Math.hypot(b.x - a.x, b.y - a.y);
    }

    #sub(a, b) {
        return {
            x: a.x - b.x,
            y: a.y - b.y,
            z: a.z - b.z
        };
    }

    #add(a, b) {
        return {
            x: a.x + b.x,
            y: a.y + b.y,
            z: a.z + b.z
        };
    }

    #scale(v, s) {
        return {
            x: v.x * s,
            y: v.y * s,
            z: v.z * s
        };
    }

    #length(v) {
        return Math.hypot(v.x, v.y, v.z);
    }

    #cross(a, b) {
        return {
            x: a.y * b.z - a.z * b.y,
            y: a.z * b.x - a.x * b.z,
            z: a.x * b.y - a.y * b.x
        };
    }

    #intersectSegmentWithPlane(p0, p1, planePoint, planeNormal, eps = 1e-8) {
        const d0 = this.#dot(this.#sub(p0, planePoint), planeNormal);
        const d1 = this.#dot(this.#sub(p1, planePoint), planeNormal);

        if (Math.abs(d0) < eps && Math.abs(d1) < eps) {
            return [p0, p1];
        }

        if ((d0 < 0 && d1 < 0) || (d0 > 0 && d1 > 0)) {
            return [];
        }

        const denom = d0 - d1;
        if (Math.abs(denom) < eps) return [];

        const t = d0 / denom;
        if (t < -eps || t > 1 + eps) return [];

        return [{
            x: p0.x + (p1.x - p0.x) * t,
            y: p0.y + (p1.y - p0.y) * t,
            z: p0.z + (p1.z - p0.z) * t
        }];
    }

    #uniquePoints(points, eps = 1e-5) {
        const out = [];
        for (const p of points) {
            const exists = out.some((q) =>
                Math.abs(p.x - q.x) < eps &&
                Math.abs(p.y - q.y) < eps &&
                Math.abs(p.z - q.z) < eps
            );
            if (!exists) out.push(p);
        }
        return out;
    }

    #buildDragPlaneCache() {
        const def = this.#getDragPlaneWorldDefinition();
        if (!def) return null;

        const { width, height, scale, bounds } = this.#getMetrics();
        const center = bounds.center;

        const polygon3D = this.#dragPlaneIntersectionPolygon();
        if (polygon3D.length < 3) return null;

        const polygonProj = polygon3D.map((p) =>
            this.#projectPoint(p, width, height, scale, center)
        );

        const fillSvg = this.#dragPlaneFillSVG(polygon3D, polygonProj, def);
        const gridSvg = this.#dragPlaneGridSVG(polygon3D, def, width, height, scale, center);

        return {
            width,
            height,
            scale,
            yaw: this.state.yaw,
            pitch: this.state.pitch,
            zoom: this.state.zoom,
            polygon3D,
            polygonProj,
            fillSvg,
            gridSvg,
            outlineSvg: this.#polygonOutlineSVG(
                polygonProj,
                'rgba(255,255,255,0.7)',
                2,
                'rgba(255,255,255,0.05)',
                '6 4'
            )
        };
    }

    #invalidateDragPlaneCache() {
        this.state.dragPlaneCache = null;
    }

    #dragPlaneIntersectionPolygon() {
        const def = this.#getDragPlaneWorldDefinition();
        if (!def) return [];

        const nLen = this.#length(def.normal);
        if (nLen < 1e-8) return [];

        const edges = this.#rgbCubeEdgePairs();
        let pts = [];

        for (const [p0, p1] of edges) {
            pts.push(...this.#intersectSegmentWithPlane(p0, p1, def.origin, def.normal));
        }

        pts = this.#uniquePoints(pts);
        if (pts.length < 3) return [];

        return this.#sortCoplanarPolygonPoints(pts, def.normal);
    }

    #normalize(v) {
        const len = this.#length(v) || 1;
        return {
            x: v.x / len,
            y: v.y / len,
            z: v.z / len
        };
    }

    #polygonBounds2D(points, pad = 0) {
        if (!points.length) {
            return { minX: 0, minY: 0, maxX: 0, maxY: 0, width: 0, height: 0 };
        }

        let minX = Infinity;
        let minY = Infinity;
        let maxX = -Infinity;
        let maxY = -Infinity;

        for (const p of points) {
            minX = Math.min(minX, p.x);
            minY = Math.min(minY, p.y);
            maxX = Math.max(maxX, p.x);
            maxY = Math.max(maxY, p.y);
        }

        minX -= pad;
        minY -= pad;
        maxX += pad;
        maxY += pad;

        return {
            minX,
            minY,
            maxX,
            maxY,
            width: Math.max(1, maxX - minX),
            height: Math.max(1, maxY - minY)
        };
    }

    #escapeAttr(value) {
        return String(value)
            .replace(/&/g, '&amp;')
            .replace(/"/g, '&quot;')
            .replace(/</g, '&lt;')
            .replace(/>/g, '&gt;');
    }

    #sortCoplanarPolygonPoints(points, normal) {
        const center = points.reduce(
            (acc, p) => ({
                x: acc.x + p.x / points.length,
                y: acc.y + p.y / points.length,
                z: acc.z + p.z / points.length
            }),
            { x: 0, y: 0, z: 0 }
        );

        const n = this.#normalize(normal);

        let ref = Math.abs(n.x) < 0.8
            ? { x: 1, y: 0, z: 0 }
            : { x: 0, y: 1, z: 0 };

        const u = this.#normalize(this.#cross(n, ref));
        const v = this.#cross(n, u);

        return [...points].sort((p1, p2) => {
            const a = this.#sub(p1, center);
            const b = this.#sub(p2, center);

            const ang1 = Math.atan2(this.#dot(a, v), this.#dot(a, u));
            const ang2 = Math.atan2(this.#dot(b, v), this.#dot(b, u));
            return ang1 - ang2;
        });
    }

    #polygonOutlineSVG(points, stroke = 'rgba(255,255,255,0.65)', width = 2, fill = 'rgba(255,255,255,0.04)', dash = '6 4') {
        if (!points.length) return '';
        const pts = points.map((p) => `${p.x},${p.y}`).join(' ');
        return `
    <polygon points="${pts}" fill="${fill}" stroke="${stroke}" stroke-width="${width}"
      ${dash ? `stroke-dasharray="${dash}"` : ''} stroke-linejoin="round" />
  `;
    }

    #bestScreenAlignedPlane() {
        const [a, b, c] = this.basis.map((entry) => entry.vector);

        const planes = [
            { name: 'xy', normal: this.#cross(a, b) },
            { name: 'xz', normal: this.#cross(a, c) },
            { name: 'yz', normal: this.#cross(b, c) }
        ];

        let best = 'xy';
        let bestScore = -Infinity;

        for (const plane of planes) {
            const rotated = this.#rotateCenteredVector(plane.normal);
            const len = Math.hypot(rotated.x, rotated.y, rotated.z) || 1;
            const score = Math.abs(rotated.z) / len;
            if (score > bestScore) {
                bestScore = score;
                best = plane.name;
            }
        }

        return best;
    }

    #solve2x2(a11, a12, a21, a22, b1, b2) {
        const det = a11 * a22 - a12 * a21;
        if (Math.abs(det) < 1e-8) return [0, 0];
        return [
            (b1 * a22 - b2 * a12) / det,
            (a11 * b2 - a21 * b1) / det
        ];
    }

    #svgPointToWorldOnPlane(screenX, screenY, anchorWorld, def, iterations = 4) {
        const { width, height, scale, bounds } = this.#getMetrics();
        const center = bounds.center;

        const { u, v } = this.#planeFrame(def);

        let world = anchorWorld;

        {
            const anchorProj = this.#projectPoint(anchorWorld, width, height, scale, center);
            const deriv = this.#screenDerivativesForWorldVectors(anchorWorld, u, v);
            const [du, dv] = this.#solve2x2(
                deriv.a.x, deriv.b.x,
                deriv.a.y, deriv.b.y,
                screenX - anchorProj.x,
                screenY - anchorProj.y
            );

            world = this.#add(
                anchorWorld,
                this.#add(this.#scale(u, du), this.#scale(v, dv))
            );
        }

        for (let i = 0; i < iterations; i += 1) {
            const proj = this.#projectPoint(world, width, height, scale, center);
            const errX = screenX - proj.x;
            const errY = screenY - proj.y;

            if (Math.abs(errX) + Math.abs(errY) < 0.01) break;

            const deriv = this.#screenDerivativesForWorldVectors(world, u, v);
            const [du, dv] = this.#solve2x2(
                deriv.a.x, deriv.b.x,
                deriv.a.y, deriv.b.y,
                errX, errY
            );

            world = this.#add(
                world,
                this.#add(this.#scale(u, du), this.#scale(v, dv))
            );
        }

        return world;
    }

    #dragPlaneFillSVG(polygon3D, polygonProj, def) {
        if (!this.showDragPlaneFill) return '';
        if (!def || polygon3D.length < 3 || polygonProj.length < 3) return '';

        const bounds = this.#polygonBounds2D(polygonProj, 1.5);
        const maxDim = Math.max(bounds.width, bounds.height) || 1;
        const scale = this.dragPlaneFillResolution / maxDim;

        const canvasWidth = Math.max(2, Math.ceil(bounds.width * scale));
        const canvasHeight = Math.max(2, Math.ceil(bounds.height * scale));

        const canvas = document.createElement('canvas');
        canvas.width = canvasWidth;
        canvas.height = canvasHeight;

        const ctx = canvas.getContext('2d');
        if (!ctx) return '';

        const image = ctx.createImageData(canvasWidth, canvasHeight);
        const data = image.data;

        const { u, v } = this.#planeFrame(def);
        const poly2D = polygon3D.map((p) => this.#planeTo2D(p, def.origin, u, v));

        const anchorWorld = polygon3D.reduce(
            (acc, p) => ({
                x: acc.x + p.x / polygon3D.length,
                y: acc.y + p.y / polygon3D.length,
                z: acc.z + p.z / polygon3D.length
            }),
            { x: 0, y: 0, z: 0 }
        );

        for (let py = 0; py < canvasHeight; py += 1) {
            const sy = bounds.minY + (py + 0.5) / scale;

            for (let px = 0; px < canvasWidth; px += 1) {
                const sx = bounds.minX + (px + 0.5) / scale;
                const idx = (py * canvasWidth + px) * 4;

                const world = this.#svgPointToWorldOnPlane(sx, sy, anchorWorld, def);
                const p2 = this.#planeTo2D(world, def.origin, u, v);

                if (!this.#pointInPolygon2D(p2, poly2D)) {
                    data[idx + 3] = 0;
                    continue;
                }

                const rgb = this.#clampWorldPointToRgbCube(world);

                data[idx + 0] = Math.round(rgb.x * 255);
                data[idx + 1] = Math.round(rgb.y * 255);
                data[idx + 2] = Math.round(rgb.z * 255);
                data[idx + 3] = Math.round(this.dragPlaneFillOpacity * 255);
            }
        }

        ctx.putImageData(image, 0, 0);

        const dataUrl = canvas.toDataURL('image/png');
        const clipId = `drag_plane_clip_${Math.random().toString(36).slice(2)}`;
        const pts = polygonProj.map((p) => `${p.x},${p.y}`).join(' ');

        return `
      <defs>
<clipPath id="${clipId}">
<polygon points="${pts}" />
</clipPath>
</defs>
<image x="${bounds.minX}" y="${bounds.minY}" width="${bounds.width}" height="${bounds.height}" href="${this.#escapeAttr(dataUrl)}" preserveAspectRatio="none" clip-path="url(#${clipId})" opacity="1" />
    `;
    }

    #planeBasisCoordinates(point, def) {
        const d = this.#sub(point, def.origin);
        const a = def.span1;
        const b = def.span2;

        const aa = this.#dot(a, a);
        const ab = this.#dot(a, b);
        const bb = this.#dot(b, b);
        const da = this.#dot(d, a);
        const db = this.#dot(d, b);

        const [u, v] = this.#solve2x2(aa, ab, ab, bb, da, db);
        return { x: u, y: v };
    }

    #planePointFromBasisCoordinates(p, def) {
        return this.#add(
            def.origin,
            this.#add(this.#scale(def.span1, p.x), this.#scale(def.span2, p.y))
        );
    }

    #uniquePoints2D(points, eps = 1e-6) {
        const out = [];
        for (const p of points) {
            const exists = out.some((q) =>
                Math.abs(p.x - q.x) < eps &&
                Math.abs(p.y - q.y) < eps
            );
            if (!exists) out.push(p);
        }
        return out;
    }

    #clipPolygonLineInBasisCoords(polygon2D, axis, value, eps = 1e-8) {
        const intersections = [];

        for (let i = 0; i < polygon2D.length; i += 1) {
            const a = polygon2D[i];
            const b = polygon2D[(i + 1) % polygon2D.length];

            const av = axis === 'x' ? a.x : a.y;
            const bv = axis === 'x' ? b.x : b.y;

            if (Math.abs(av - value) < eps && Math.abs(bv - value) < eps) {
                intersections.push(a, b);
                continue;
            }

            if ((av < value && bv < value) || (av > value && bv > value)) {
                continue;
            }

            const denom = bv - av;
            if (Math.abs(denom) < eps) continue;

            const t = (value - av) / denom;
            if (t < -eps || t > 1 + eps) continue;

            intersections.push({
                x: a.x + (b.x - a.x) * t,
                y: a.y + (b.y - a.y) * t
            });
        }

        const uniq = this.#uniquePoints2D(intersections);
        if (uniq.length < 2) return null;

        uniq.sort((p1, p2) => (axis === 'x' ? p1.y - p2.y : p1.x - p2.x));

        return [uniq[0], uniq[uniq.length - 1]];
    }

    #dragPlaneGridSVG(polygon3D, def, width, height, scale, center) {
        if (!this.showDragPlaneGrid) return '';
        if (!def || polygon3D.length < 3) return '';

        const step = Math.max(1e-4, Number(this.dragPlaneGridStep) || 0.1);
        const polygonBasis = polygon3D.map((p) => this.#planeBasisCoordinates(p, def));
        if (polygonBasis.length < 3) return '';

        let minX = Infinity;
        let maxX = -Infinity;
        let minY = Infinity;
        let maxY = -Infinity;

        for (const p of polygonBasis) {
            minX = Math.min(minX, p.x);
            maxX = Math.max(maxX, p.x);
            minY = Math.min(minY, p.y);
            maxY = Math.max(maxY, p.y);
        }

        const eps = 1e-8;

        const firstGridAtOrAbove = (v) => Math.ceil((v - eps) / step) * step;
        const lastGridAtOrBelow = (v) => Math.floor((v + eps) / step) * step;

        const xStart = firstGridAtOrAbove(minX);
        const xEnd = lastGridAtOrBelow(maxX);
        const yStart = firstGridAtOrAbove(minY);
        const yEnd = lastGridAtOrBelow(maxY);

        const isMajorLine = (t) => {
            const k = Math.round(t / step);
            return k % 5 === 0; // every 0.5 if step = 0.1
        };

        let html = '<g>';

        for (let x = xStart; x <= xEnd + eps; x += step) {
            const seg = this.#clipPolygonLineInBasisCoords(polygonBasis, 'x', x);
            if (!seg) continue;

            const isMajor = isMajorLine(x);
            const opacity = isMajor ? this.dragPlaneGridMajorOpacity : this.dragPlaneGridOpacity;
            const strokeWidth = isMajor ? 1.35 : 0.8;
            const dash = isMajor ? '' : '3 5';

            const w0 = this.#planePointFromBasisCoordinates(seg[0], def);
            const w1 = this.#planePointFromBasisCoordinates(seg[1], def);
            const p0 = this.#projectPoint(w0, width, height, scale, center);
            const p1 = this.#projectPoint(w1, width, height, scale, center);

            html += this.#lineSVG(p0, p1, 'rgba(255,255,255,0.9)', strokeWidth, opacity, dash);
        }

        for (let y = yStart; y <= yEnd + eps; y += step) {
            const seg = this.#clipPolygonLineInBasisCoords(polygonBasis, 'y', y);
            if (!seg) continue;

            const isMajor = isMajorLine(y);
            const opacity = isMajor ? this.dragPlaneGridMajorOpacity : this.dragPlaneGridOpacity;
            const strokeWidth = isMajor ? 1.35 : 0.8;
            const dash = isMajor ? '' : '3 5';

            const w0 = this.#planePointFromBasisCoordinates(seg[0], def);
            const w1 = this.#planePointFromBasisCoordinates(seg[1], def);
            const p0 = this.#projectPoint(w0, width, height, scale, center);
            const p1 = this.#projectPoint(w1, width, height, scale, center);

            html += this.#lineSVG(p0, p1, 'rgba(255,255,255,0.9)', strokeWidth, opacity, dash);
        }

        html += '</g>';
        return html;
    }

    #screenDerivativesForWorldVectors(worldPoint, vecA, vecB, epsilon = 1e-3) {
        const { width, height, scale, bounds } = this.#getMetrics();
        const center = bounds.center;

        const p0 = this.#projectPoint(worldPoint, width, height, scale, center);
        const pA = this.#projectPoint(
            this.#add(worldPoint, this.#scale(vecA, epsilon)),
            width, height, scale, center
        );
        const pB = this.#projectPoint(
            this.#add(worldPoint, this.#scale(vecB, epsilon)),
            width, height, scale, center
        );

        return {
            a: {
                x: (pA.x - p0.x) / epsilon,
                y: (pA.y - p0.y) / epsilon
            },
            b: {
                x: (pB.x - p0.x) / epsilon,
                y: (pB.y - p0.y) / epsilon
            }
        };
    }

    #planeFrame(def) {
        const u = this.#normalize(def.span1);

        let v = this.#sub(def.span2, this.#scale(u, this.#dot(def.span2, u)));
        if (this.#length(v) < 1e-8) {
            v = this.#cross(def.normal, u);
        }
        v = this.#normalize(v);

        return { u, v };
    }

    #planeTo2D(point, origin, u, v) {
        const d = this.#sub(point, origin);
        return {
            x: this.#dot(d, u),
            y: this.#dot(d, v)
        };
    }

    #planeFrom2D(p, origin, u, v) {
        return this.#add(
            origin,
            this.#add(this.#scale(u, p.x), this.#scale(v, p.y))
        );
    }

    #distancePointToSegment2D(p, a, b) {
        const q = this.#closestPointOnSegment2D(p, a, b);
        return Math.hypot(p.x - q.x, p.y - q.y);
    }

    #pointInPolygon2D(point, polygon, edgeTolerance = 1e-4) {
        for (let i = 0; i < polygon.length; i += 1) {
            const a = polygon[i];
            const b = polygon[(i + 1) % polygon.length];
            if (this.#distancePointToSegment2D(point, a, b) <= edgeTolerance) {
                return true;
            }
        }

        let inside = false;
        for (let i = 0, j = polygon.length - 1; i < polygon.length; j = i++) {
            const a = polygon[i];
            const b = polygon[j];

            const intersect =
                ((a.y > point.y) !== (b.y > point.y)) &&
                point.x < ((b.x - a.x) * (point.y - a.y)) / ((b.y - a.y) || 1e-12) + a.x;

            if (intersect) inside = !inside;
        }
        return inside;
    }

    #closestPointOnSegment2D(p, a, b) {
        const ab = { x: b.x - a.x, y: b.y - a.y };
        const ap = { x: p.x - a.x, y: p.y - a.y };
        const ab2 = ab.x * ab.x + ab.y * ab.y || 1e-12;
        const t = Math.max(0, Math.min(1, (ap.x * ab.x + ap.y * ab.y) / ab2));
        return {
            x: a.x + ab.x * t,
            y: a.y + ab.y * t
        };
    }

    #clampPointToPolygon2D(point, polygon) {
        if (!polygon.length) return point;
        if (this.#pointInPolygon2D(point, polygon)) return point;

        let best = null;
        let bestDist2 = Infinity;

        for (let i = 0; i < polygon.length; i++) {
            const a = polygon[i];
            const b = polygon[(i + 1) % polygon.length];
            const q = this.#closestPointOnSegment2D(point, a, b);
            const dx = q.x - point.x;
            const dy = q.y - point.y;
            const d2 = dx * dx + dy * dy;
            if (d2 < bestDist2) {
                bestDist2 = d2;
                best = q;
            }
        }

        return best || point;
    }

    #dragValueInAlignedPlaneToPointer(pointerSvgX, pointerSvgY) {
        const def = this.#getDragPlaneWorldDefinition();
        if (!def) return;

        const anchorPointer = this.state.dragPointerAnchor;
        const anchorWorld = this.state.dragWorldAnchor;
        if (!anchorPointer || !anchorWorld) return;

        const anchorPointerSvg = this.#clientToSvgPoint(anchorPointer.x, anchorPointer.y);
        if (!anchorPointerSvg) return;

        const pointAtDragStart = this.#projectPoint(
            anchorWorld,
            this.#getMetrics().width,
            this.#getMetrics().height,
            this.#getMetrics().scale,
            this.#getMetrics().bounds.center
        );

        const grabOffsetX = anchorPointerSvg.x - pointAtDragStart.x;
        const grabOffsetY = anchorPointerSvg.y - pointAtDragStart.y;

        const targetWorld = this.#svgPointToWorldOnPlane(
            pointerSvgX - grabOffsetX,
            pointerSvgY - grabOffsetY,
            anchorWorld,
            def
        );

        const { u, v } = this.#planeFrame(def);

        const polygon3D = this.#dragPlaneIntersectionPolygon();
        if (polygon3D.length < 3) return;

        const poly2D = polygon3D.map((p) => this.#planeTo2D(p, def.origin, u, v));
        const target2D = this.#planeTo2D(targetWorld, def.origin, u, v);
        const clamped2D = this.#clampPointToPolygon2D(target2D, poly2D);
        const clampedWorld = this.#planeFrom2D(clamped2D, def.origin, u, v);

        this.value = this.#worldToBasisCoefficients(clampedWorld);

        this.draw();
        this.#emitChange();
    }

    #normalizeBasis(basis) {
        if (!Array.isArray(basis) || basis.length !== 3) {
            throw new Error('Basis must be an array of 3 vectors.');
        }

        return basis.map((entry, index) => {
            const fallbackNames = ['X', 'Y', 'Z'];
            return {
                name: entry?.name || fallbackNames[index],
                color: entry?.color || '#cccccc',
                vector: {
                    x: Number(entry?.vector?.x ?? 0),
                    y: Number(entry?.vector?.y ?? 0),
                    z: Number(entry?.vector?.z ?? 0)
                }
            };
        });
    }

    #clampUnit(v) {
        return Math.max(0, Math.min(1, Number(v) || 0));
    }

    #clampWorldPointToRgbCube(p) {
        return {
            x: Math.max(0, Math.min(1, Number(p?.x) || 0)),
            y: Math.max(0, Math.min(1, Number(p?.y) || 0)),
            z: Math.max(0, Math.min(1, Number(p?.z) || 0))
        };
    }

    #worldToBasisCoefficients(worldPoint) {
        const basisVectors = this.basis.map((b) => b.vector);
        const coeffs = this.#solve3x3(
            [
                [basisVectors[0].x, basisVectors[1].x, basisVectors[2].x],
                [basisVectors[0].y, basisVectors[1].y, basisVectors[2].y],
                [basisVectors[0].z, basisVectors[1].z, basisVectors[2].z]
            ],
            [worldPoint.x, worldPoint.y, worldPoint.z]
        );

        return {
            x: coeffs[0],
            y: coeffs[1],
            z: coeffs[2]
        };
    }

    #mixToWorld(point) {
        const [a, b, c] = this.basis;
        return {
            x: point.x * a.vector.x + point.y * b.vector.x + point.z * c.vector.x,
            y: point.x * a.vector.y + point.y * b.vector.y + point.z * c.vector.y,
            z: point.x * a.vector.z + point.y * b.vector.z + point.z * c.vector.z
        };
    }

    #rgbCubeCorners() {
        return {
            O: { x: 0, y: 0, z: 0 },
            A: { x: 1, y: 0, z: 0 },
            B: { x: 0, y: 1, z: 0 },
            C: { x: 0, y: 0, z: 1 },
            AB: { x: 1, y: 1, z: 0 },
            AC: { x: 1, y: 0, z: 1 },
            BC: { x: 0, y: 1, z: 1 },
            ABC: { x: 1, y: 1, z: 1 }
        };
    }

    #rgbCubeEdgePairs() {
        const corners = this.#rgbCubeCorners();
        return [
            [corners.O, corners.A],
            [corners.O, corners.B],
            [corners.O, corners.C],
            [corners.A, corners.AB],
            [corners.A, corners.AC],
            [corners.B, corners.AB],
            [corners.B, corners.BC],
            [corners.C, corners.AC],
            [corners.C, corners.BC],
            [corners.AB, corners.ABC],
            [corners.AC, corners.ABC],
            [corners.BC, corners.ABC]
        ];
    }

    #makeDragPlaneWorldDefinition(plane) {
        const [A, B, C] = this.basis.map((b) => b.vector);
        if (!plane) return null;

        if (plane === 'xy') {
            const origin = {
                x: this.value.z * C.x,
                y: this.value.z * C.y,
                z: this.value.z * C.z
            };
            return {
                plane,
                origin,
                span1: A,
                span2: B,
                normal: this.#cross(A, B)
            };
        }

        if (plane === 'xz') {
            const origin = {
                x: this.value.y * B.x,
                y: this.value.y * B.y,
                z: this.value.y * B.z
            };
            return {
                plane,
                origin,
                span1: A,
                span2: C,
                normal: this.#cross(A, C)
            };
        }

        const origin = {
            x: this.value.x * A.x,
            y: this.value.x * A.y,
            z: this.value.x * A.z
        };
        return {
            plane,
            origin,
            span1: B,
            span2: C,
            normal: this.#cross(B, C)
        };
    }

    #planeIntersectionPolygonForPlaneName(plane) {
        const def = this.#makeDragPlaneWorldDefinition(plane);
        if (!def) return [];

        const nLen = this.#length(def.normal);
        if (nLen < 1e-8) return [];

        const edges = this.#rgbCubeEdgePairs();
        let pts = [];

        for (const [p0, p1] of edges) {
            pts.push(...this.#intersectSegmentWithPlane(p0, p1, def.origin, def.normal));
        }

        pts = this.#uniquePoints(pts);
        if (pts.length < 3) return [];

        return this.#sortCoplanarPolygonPoints(pts, def.normal);
    }

    #planeOutlineStyle(name, principal, active) {
        if (active) {
            return {
                stroke: 'rgba(255,255,255,0.88)',
                width: 1.5,
                fill: 'rgba(255,255,255,0.06)',
                dash: '6 4'
            };
        }

        if (name === principal) {
            return {
                stroke: 'rgba(255,255,255,0.52)',
                width: 1.5,
                fill: 'rgba(255,255,255,0.025)',
                dash: '6 4'
            };
        }

        return {
            stroke: 'rgba(255,255,255,0.22)',
            width: 1.2,
            fill: 'rgba(255,255,255,0.0)',
            dash: '4 5'
        };
    }

    #getDragPlaneWorldDefinition() {
        return this.state.dragPlaneDef || null;
    }

    #worldBounds(points) {
        const vals = Object.values(points);
        let minX = Infinity, minY = Infinity, minZ = Infinity;
        let maxX = -Infinity, maxY = -Infinity, maxZ = -Infinity;

        for (const p of vals) {
            minX = Math.min(minX, p.x);
            minY = Math.min(minY, p.y);
            minZ = Math.min(minZ, p.z);
            maxX = Math.max(maxX, p.x);
            maxY = Math.max(maxY, p.y);
            maxZ = Math.max(maxZ, p.z);
        }

        const center = {
            x: (minX + maxX) * 0.5,
            y: (minY + maxY) * 0.5,
            z: (minZ + maxZ) * 0.5
        };

        const radius = Math.max(
            maxX - minX,
            maxY - minY,
            maxZ - minZ,
            1e-3
        ) * 0.5;

        return { center, radius };
    }

    #rotatePoint(worldPoint, center) {
        const cy = Math.cos(this.state.yaw);
        const sy = Math.sin(this.state.yaw);
        const cp = Math.cos(this.state.pitch);
        const sp = Math.sin(this.state.pitch);

        const x = worldPoint.x - center.x;
        const y = worldPoint.y - center.y;
        const z = worldPoint.z - center.z;

        const x1 = x * cy + z * sy;
        const y1 = y;
        const z1 = -x * sy + z * cy;

        const x2 = x1;
        const y2 = y1 * cp - z1 * sp;
        const z2 = y1 * sp + z1 * cp;

        return { x: x2, y: y2, z: z2 };
    }

    #projectPoint(worldPoint, width, height, scale, center) {
        const pr = this.#rotatePoint(worldPoint, center);
        const camera = 4.2;
        const perspective = camera / (camera - pr.z);

        return {
            x: width * 0.5 + pr.x * scale * perspective,
            y: height * 0.5 - pr.y * scale * perspective,
            z: pr.z,
            depth: pr.z
        };
    }

    #rotateCenteredVector(v) {
        const cy = Math.cos(this.state.yaw);
        const sy = Math.sin(this.state.yaw);
        const cp = Math.cos(this.state.pitch);
        const sp = Math.sin(this.state.pitch);

        const x1 = v.x * cy + v.z * sy;
        const y1 = v.y;
        const z1 = -v.x * sy + v.z * cy;

        const x2 = x1;
        const y2 = y1 * cp - z1 * sp;
        const z2 = y1 * sp + z1 * cp;

        return { x: x2, y: y2, z: z2 };
    }

    #inverseRotateCenteredVector(v) {
        const cy = Math.cos(this.state.yaw);
        const sy = Math.sin(this.state.yaw);
        const cp = Math.cos(this.state.pitch);
        const sp = Math.sin(this.state.pitch);

        const x1 = v.x;
        const y1 = v.y * cp + v.z * sp;
        const z1 = -v.y * sp + v.z * cp;

        const x2 = x1 * cy - z1 * sy;
        const y2 = y1;
        const z2 = x1 * sy + z1 * cy;

        return { x: x2, y: y2, z: z2 };
    }

    #getMetrics() {
        const viewBox = this.svg.viewBox.baseVal;
        const width = viewBox && viewBox.width ? viewBox.width : 1000;
        const height = viewBox && viewBox.height ? viewBox.height : 700;

        const corners = this.#rgbCubeCorners();
        const bounds = this.#worldBounds(corners);
        const scale = Math.min(width, height) * 0.62 * this.state.zoom / Math.max(bounds.radius * 2, 0.75);

        return {
            width,
            height,
            scale,
            bounds,
            corners
        };
    }

    #currentWorldPoint() {
        return this.#mixToWorld(this.value);
    }

    #projectedValuePoint() {
        const { width, height, scale, bounds } = this.#getMetrics();
        return this.#projectPoint(this.#currentWorldPoint(), width, height, scale, bounds.center);
    }

    #svgClientPoint(x, y) {
        const ctm = this.svg.getScreenCTM();
        if (!ctm) return null;
        const pt = new DOMPoint(x, y).matrixTransform(ctm);
        return { x: pt.x, y: pt.y };
    }

    #clientToSvgPoint(clientX, clientY) {
        const ctm = this.svg.getScreenCTM();
        if (!ctm) return null;
        const pt = new DOMPoint(clientX, clientY).matrixTransform(ctm.inverse());
        return { x: pt.x, y: pt.y };
    }

    #pointerPos(ev) {
        if (ev.touches && ev.touches.length) {
            return { x: ev.touches[0].clientX, y: ev.touches[0].clientY };
        }
        return { x: ev.clientX, y: ev.clientY };
    }

    #touchDistance(ev) {
        if (!ev.touches || ev.touches.length < 2) return null;
        const dx = ev.touches[1].clientX - ev.touches[0].clientX;
        const dy = ev.touches[1].clientY - ev.touches[0].clientY;
        return Math.hypot(dx, dy);
    }

    #isPointerNearProjectedValuePoint(ev, radius) {
        const p = this.#pointerPos(ev);
        const point = this.#projectedValuePoint();
        const screen = this.#svgClientPoint(point.x, point.y);
        if (!screen) return false;
        return Math.hypot(p.x - screen.x, p.y - screen.y) <= radius;
    }

    #isPointerNearValuePoint(ev) {
        return this.#isPointerNearProjectedValuePoint(ev, this.state.pointGrabRadius);
    }

    #isPointerNearPreviewTrigger(ev) {
        return this.#isPointerNearProjectedValuePoint(ev, this.state.previewTriggerRadius);
    }

    #dot(a, b) {
        return a.x * b.x + a.y * b.y + a.z * b.z;
    }

    #solve3x3(m, b) {
        const A = m.map((row, i) => [...row, b[i]]);

        for (let col = 0; col < 3; col += 1) {
            let pivot = col;
            for (let row = col + 1; row < 3; row += 1) {
                if (Math.abs(A[row][col]) > Math.abs(A[pivot][col])) pivot = row;
            }

            if (Math.abs(A[pivot][col]) < 1e-8) {
                return [0, 0, 0];
            }

            if (pivot !== col) {
                const tmp = A[col];
                A[col] = A[pivot];
                A[pivot] = tmp;
            }

            const div = A[col][col];
            for (let j = col; j < 4; j += 1) A[col][j] /= div;

            for (let row = 0; row < 3; row += 1) {
                if (row === col) continue;
                const factor = A[row][col];
                for (let j = col; j < 4; j += 1) {
                    A[row][j] -= factor * A[col][j];
                }
            }
        }

        return [A[0][3], A[1][3], A[2][3]];
    }

    #lineSVG(a, b, stroke, width, opacity = 1, dash = '') {
        return `
      <line x1="${a.x}" y1="${a.y}" x2="${b.x}" y2="${b.y}" stroke="${stroke}" stroke-width="${width}" stroke-opacity="${opacity}"
        ${dash ? `stroke-dasharray="${dash}"` : ''} stroke-linecap="round" />
    `;
    }

    #circleSVG(p, r, fill, stroke = 'none', strokeWidth = 0, opacity = 1) {
        return `
      <circle cx="${p.x}" cy="${p.y}" r="${r}" fill="${fill}" stroke="${stroke}" stroke-width="${strokeWidth}" fill-opacity="${opacity}" />
    `;
    }

    #parseColorToRgb(color) {
        const value = String(color || '').trim();

        if (value.startsWith('#')) {
            let hex = value.slice(1);
            if (hex.length === 3) {
                hex = hex.split('').map((c) => c + c).join('');
            }
            if (hex.length >= 6) {
                return {
                    r: parseInt(hex.slice(0, 2), 16) || 0,
                    g: parseInt(hex.slice(2, 4), 16) || 0,
                    b: parseInt(hex.slice(4, 6), 16) || 0
                };
            }
        }

        const rgbMatch = value.match(
            /rgba?\(\s*([0-9.]+)\s*,\s*([0-9.]+)\s*,\s*([0-9.]+)(?:\s*,\s*([0-9.]+))?\s*\)/i
        );
        if (rgbMatch) {
            return {
                r: Number(rgbMatch[1]) || 0,
                g: Number(rgbMatch[2]) || 0,
                b: Number(rgbMatch[3]) || 0
            };
        }

        return { r: 255, g: 255, b: 255 };
    }

    #relativeLuminance(color) {
        const { r, g, b } = this.#parseColorToRgb(color);

        const toLinear = (v) => {
            const s = v / 255;
            return s <= 0.04045 ? s / 12.92 : ((s + 0.055) / 1.055) ** 2.4;
        };

        const R = toLinear(r);
        const G = toLinear(g);
        const B = toLinear(b);

        return 0.2126 * R + 0.7152 * G + 0.0722 * B;
    }

    #textStyleForColor(color) {
        const lum = this.#relativeLuminance(color);

        if (lum < 0.15) {
            return {
                fill: '#ffffff',
                halo: color,
                haloWidth: 4.5
            };
        }

        return {
            fill: color,
            halo: 'rgba(20,22,28,0.92)',
            haloWidth: 4
        };
    }

    #textHaloForColor(color) {
        const lum = this.#relativeLuminance(color);

        if (lum < 0.33) {
            return 'rgba(255,255,255,0.92)';
        }

        return 'rgba(20,22,28,0.9)';
    }

    #textSVG(
        p,
        str,
        color,
        dx = 0,
        dy = 0,
        size = 14,
        anchor = 'middle',
        halo = null,
        haloWidth = null
    ) {
        const style = this.#textStyleForColor(color);
        const resolvedFill = style.fill;
        const resolvedHalo = halo ?? style.halo;
        const resolvedHaloWidth = haloWidth ?? style.haloWidth;

        return `
      <text x="${p.x + dx}" y="${p.y + dy}" fill="${resolvedFill}" stroke="${resolvedHalo}" stroke-width="${resolvedHaloWidth}" paint-order="stroke fill" stroke-linejoin="round" font-size="${size}" font-family="sans-serif" font-weight="600" text-anchor="${anchor}" dominant-baseline="middle" style="user-select:none">${str}</text>
    `;
    }

    #textOnSegmentSVG(
        a,
        b,
        str,
        color,
        {
            offset = 14,
            size = 13,
            halo = null,
            haloWidth = null,
            anchor = 'middle',
            weight = '700',
            side = 1
        } = {}
    ) {
        const mx = (a.x + b.x) * 0.5;
        const my = (a.y + b.y) * 0.5;

        const dx = b.x - a.x;
        const dy = b.y - a.y;
        const len = Math.hypot(dx, dy) || 1;

        const px = -dy / len;
        const py = dx / len;

        const x = mx + px * offset * side;
        const y = my + py * offset * side;

        let angle = Math.atan2(dy, dx) * 180 / Math.PI;
        if (angle > 90 || angle < -90) {
            angle += 180;
        }

        const style = this.#textStyleForColor(color);
        const resolvedFill = style.fill;
        const resolvedHalo = halo ?? style.halo;
        const resolvedHaloWidth = haloWidth ?? style.haloWidth;

        return `
      <text
        x="${x}"
        y="${y}"
        fill="${resolvedFill}"
        stroke="${resolvedHalo}"
        stroke-width="${resolvedHaloWidth}"
        paint-order="stroke fill"
        stroke-linejoin="round"
        font-size="${size}"
        font-family="sans-serif"
        font-weight="${weight}"
        text-anchor="${anchor}"
        dominant-baseline="middle"
        transform="rotate(${angle} ${x} ${y})"
        style="user-select:none"
      >${str}</text>
    `;
    }

    #distance2D(a, b) {
        return Math.hypot(a.x - b.x, a.y - b.y);
    }

    #midOffsetPoint(a, b, offset = 14, side = 1) {
        const mx = (a.x + b.x) * 0.5;
        const my = (a.y + b.y) * 0.5;

        const dx = b.x - a.x;
        const dy = b.y - a.y;
        const len = Math.hypot(dx, dy) || 1;

        const px = -dy / len;
        const py = dx / len;

        return {
            x: mx + px * offset * side,
            y: my + py * offset * side
        };
    }

    #bestLabelSideForSegment(a, b, avoidPoints = [], offset = 14) {
        const pPos = this.#midOffsetPoint(a, b, offset, 1);
        const pNeg = this.#midOffsetPoint(a, b, offset, -1);

        const clearance = (p) => {
            if (!avoidPoints.length) return Infinity;
            return Math.min(...avoidPoints.map((q) => this.#distance2D(p, q)));
        };

        return clearance(pPos) >= clearance(pNeg) ? 1 : -1;
    }

    #gradientLineDef(id, a, b, colorA, colorB) {
        return `
      <linearGradient id="${id}" gradientUnits="userSpaceOnUse" x1="${a.x}" y1="${a.y}" x2="${b.x}" y2="${b.y}">
<stop offset="0%" stop-color="${colorA}"></stop>
<stop offset="100%" stop-color="${colorB}"></stop>
</linearGradient>
    `;
    }

    #axisLabelPos(origin, end, distance = 18, side = 10) {
        const dx = end.x - origin.x;
        const dy = end.y - origin.y;
        const len = Math.hypot(dx, dy) || 1;

        const ux = dx / len;
        const uy = dy / len;

        const px = -uy;
        const py = ux;

        const sign = 1;

        return {
            x: end.x + ux * distance + px * side * sign,
            y: end.y + uy * distance + py * side * sign
        };
    }

    #originLabelPos(origin, ends, distance = 18) {
        let vx = 0;
        let vy = 0;

        for (const end of ends) {
            const dx = origin.x - end.x;
            const dy = origin.y - end.y;
            const len = Math.hypot(dx, dy) || 1;
            vx += dx / len;
            vy += dy / len;
        }

        const vlen = Math.hypot(vx, vy) || 1;
        return {
            x: origin.x + (vx / vlen) * distance,
            y: origin.y + (vy / vlen) * distance
        };
    }

    #pointMarkerSVG(p, fill = '#000', stroke = '#bbb', strokeWidth = 1.5, r = 5) {
        return `
      <circle cx="${p.x}" cy="${p.y}" r="${r}" fill="${fill}" stroke="${stroke}" stroke-width="${strokeWidth}" />
    `;
    }

    #guidePointSVG(p, color) {
        return `
      <circle cx="${p.x}" cy="${p.y}" r="3.5" fill="${color}" fill-opacity="0.9" stroke="rgba(255,255,255,0.35)" stroke-width="1" />
    `;
    }

    #guideTickSVG(p, dirX, dirY, size = 5) {
        return `
      <line x1="${p.x - dirY * size}" y1="${p.y + dirX * size}" x2="${p.x + dirY * size}" y2="${p.y - dirX * size}" stroke="rgba(255,255,255,0.25)" stroke-width="1" />
    `;
    }

    #guideArrowWithLabelSVG(
        a,
        b,
        colorStart,
        colorEnd,
        label,
        {
            opacity = 0.72,
            width = 2.4,
            dash = '4 4',
            headLen = 14,
            headWidth = 8,
            startPad = 4,
            endPad = 4,
            labelOffset = 16,
            labelSize = 12
        } = {}
    ) {
        const gradId = `guide_arrow_${Math.random().toString(36).slice(2)}`;

        const arrow = this.#gradientArrowSVG(
            a,
            b,
            colorStart,
            colorEnd,
            width,
            headLen,
            headWidth,
            opacity,
            gradId,
            startPad,
            endPad
        );

        const labelSvg = this.#textOnSegmentSVG(
            a,
            b,
            label,
            colorEnd,
            {
                offset: labelOffset,
                size: labelSize,
                weight: '700'
            }
        );

        // dashed underlay + gradient arrow + label
        return `
      <g opacity="${opacity}">
        ${this.#lineSVG(a, b, `url(#${gradId})`, Math.max(1, width - 0.6), 0.55, dash)}
      </g>
      ${arrow}
      ${labelSvg}
    `;
    }

    #arrowSVG(
        a,
        b,
        color = '#aaa',
        width = 2,
        headLen = 20,
        headWidth = 8,
        opacity = 1,
        startPad = 0,
        endPad = 0
    ) {
        const dx = b.x - a.x;
        const dy = b.y - a.y;
        const len = Math.hypot(dx, dy) || 1;

        if (len <= startPad + endPad + 1e-3) return '';

        const ux = dx / len;
        const uy = dy / len;
        const px = -uy;
        const py = ux;

        const a2 = {
            x: a.x + ux * startPad,
            y: a.y + uy * startPad
        };

        const b2 = {
            x: b.x - ux * endPad,
            y: b.y - uy * endPad
        };

        const trimmedLen = Math.hypot(b2.x - a2.x, b2.y - a2.y);
        if (trimmedLen < 1e-3) return '';

        const effectiveHeadLen = Math.min(headLen, trimmedLen * 0.6);

        const shaftEnd = {
            x: b2.x - ux * (effectiveHeadLen * 0.72),
            y: b2.y - uy * (effectiveHeadLen * 0.72)
        };

        const left = {
            x: b2.x - ux * effectiveHeadLen + px * headWidth * 0.5,
            y: b2.y - uy * effectiveHeadLen + py * headWidth * 0.5
        };

        const right = {
            x: b2.x - ux * effectiveHeadLen - px * headWidth * 0.5,
            y: b2.y - uy * effectiveHeadLen - py * headWidth * 0.5
        };

        return `
      <g opacity="${opacity}">
<line x1="${a2.x}" y1="${a2.y}" x2="${shaftEnd.x}" y2="${shaftEnd.y}" stroke="${color}" stroke-width="${width}" stroke-linecap="round" />
<polygon points="${b2.x},${b2.y} ${left.x},${left.y} ${right.x},${right.y}" fill="${color}" />
</g>
    `;
    }

    #gradientArrowSVG(
        a,
        b,
        colorStart,
        colorEnd,
        width = 4.5,
        headLen = 25,
        headWidth = 15,
        opacity = 1,
        id = '',
        startPad = 0,
        endPad = 0
    ) {
        const dx = b.x - a.x;
        const dy = b.y - a.y;
        const len = Math.hypot(dx, dy);
        if (len < 1e-3 || len <= startPad + endPad) return '';

        const ux = dx / len;
        const uy = dy / len;
        const px = -uy;
        const py = ux;

        const a2 = {
            x: a.x + ux * startPad,
            y: a.y + uy * startPad
        };

        const b2 = {
            x: b.x - ux * endPad,
            y: b.y - uy * endPad
        };

        const trimmedLen = Math.hypot(b2.x - a2.x, b2.y - a2.y);
        if (trimmedLen < 1e-3) return '';

        const gradId = id || `grad_arrow_${Math.random().toString(36).slice(2)}`;

        if (trimmedLen <= 14) {
            return `
        <defs>
<linearGradient id="${gradId}" gradientUnits="userSpaceOnUse" x1="${a2.x}" y1="${a2.y}" x2="${b2.x}" y2="${b2.y}">
<stop offset="0%" stop-color="${colorStart}"></stop>
<stop offset="100%" stop-color="${colorEnd}"></stop>
</linearGradient>
</defs>
<line x1="${a2.x}" y1="${a2.y}" x2="${b2.x}" y2="${b2.y}" stroke="url(#${gradId})" stroke-width="${width}" stroke-linecap="round" opacity="${opacity}" />
      `;
        }

        const effectiveHeadLen = Math.min(headLen, trimmedLen * 0.6);
        const shaftEnd = {
            x: b2.x - ux * (effectiveHeadLen * 0.72),
            y: b2.y - uy * (effectiveHeadLen * 0.72)
        };
        const left = {
            x: b2.x - ux * effectiveHeadLen + px * (headWidth * 0.5),
            y: b2.y - uy * effectiveHeadLen + py * (headWidth * 0.5)
        };
        const right = {
            x: b2.x - ux * effectiveHeadLen - px * (headWidth * 0.5),
            y: b2.y - uy * effectiveHeadLen - py * (headWidth * 0.5)
        };

        return `
      <defs>
<linearGradient id="${gradId}" gradientUnits="userSpaceOnUse" x1="${a2.x}" y1="${a2.y}" x2="${b2.x}" y2="${b2.y}">
<stop offset="0%" stop-color="${colorStart}"></stop>
<stop offset="100%" stop-color="${colorEnd}"></stop>
</linearGradient>
</defs>
<g opacity="${opacity}">
<line x1="${a2.x}" y1="${a2.y}" x2="${shaftEnd.x}" y2="${shaftEnd.y}" stroke="url(#${gradId})" stroke-width="${width}" stroke-linecap="round" />
<polygon points="${b2.x},${b2.y} ${left.x},${left.y} ${right.x},${right.y}" fill="${colorEnd}" />
</g>
    `;
    }

    #clipInfiniteLineToRgbCube(origin, direction, eps = 1e-8) {
        let tMin = -Infinity;
        let tMax = Infinity;

        for (const axis of ['x', 'y', 'z']) {
            const o = origin[axis];
            const d = direction[axis];

            if (Math.abs(d) < eps) {
                if (o < 0 || o > 1) return null;
                continue;
            }

            const t0 = (0 - o) / d;
            const t1 = (1 - o) / d;
            const lo = Math.min(t0, t1);
            const hi = Math.max(t0, t1);

            tMin = Math.max(tMin, lo);
            tMax = Math.min(tMax, hi);

            if (tMin > tMax) return null;
        }

        return [
            this.#add(origin, this.#scale(direction, tMin)),
            this.#add(origin, this.#scale(direction, tMax))
        ];
    }

    #worldColorHex(world) {
        const rgb = this.#clampWorldPointToRgbCube(world);
        const toHexPair = (v) => Math.round(v * 255).toString(16).padStart(2, '0');
        return `#${toHexPair(rgb.x)}${toHexPair(rgb.y)}${toHexPair(rgb.z)}`;
    }

    draw() {
        const { width, height, scale, bounds, corners } = this.#getMetrics();
        const center = bounds.center;

        let dragPlanePoly3 = [];
        let dragPlaneProj = [];
        let dragPlaneFillSvg = '';
        let dragPlaneGridSvg = '';
        let dragPlaneOutlineSvg = '';
        const dragPlaneDef = this.#getDragPlaneWorldDefinition();
        const dragPlaneCache = this.state.dragPlaneCache;

        if (
            this.state.mode === 'point' &&
            this.state.pointDragPlane &&
            dragPlaneDef &&
            dragPlaneCache &&
            dragPlaneCache.yaw === this.state.yaw &&
            dragPlaneCache.pitch === this.state.pitch &&
            dragPlaneCache.zoom === this.state.zoom &&
            dragPlaneCache.width === width &&
            dragPlaneCache.height === height &&
            dragPlaneCache.scale === scale
        ) {
            dragPlanePoly3 = dragPlaneCache.polygon3D;
            dragPlaneProj = dragPlaneCache.polygonProj;
            dragPlaneFillSvg = dragPlaneCache.fillSvg;
            dragPlaneGridSvg = dragPlaneCache.gridSvg;
            dragPlaneOutlineSvg = dragPlaneCache.outlineSvg;
        } else if (this.state.mode === 'point' && this.state.pointDragPlane && dragPlaneDef) {
            this.state.dragPlaneCache = this.#buildDragPlaneCache();
            dragPlanePoly3 = this.state.dragPlaneCache?.polygon3D || [];
            dragPlaneProj = this.state.dragPlaneCache?.polygonProj || [];
            dragPlaneFillSvg = this.state.dragPlaneCache?.fillSvg || '';
            dragPlaneGridSvg = this.state.dragPlaneCache?.gridSvg || '';
            dragPlaneOutlineSvg = this.state.dragPlaneCache?.outlineSvg || '';
        }
        const proj = {};
        for (const key of Object.keys(corners)) {
            proj[key] = this.#projectPoint(corners[key], width, height, scale, center);
        }

        const basisProj = {
            A: this.#projectPoint(this.basis[0].vector, width, height, scale, center),
            B: this.#projectPoint(this.basis[1].vector, width, height, scale, center),
            C: this.#projectPoint(this.basis[2].vector, width, height, scale, center)
        };

        const current = this.value;
        const worldPoint = this.#currentWorldPoint();
        const pointProj = this.#projectPoint(worldPoint, width, height, scale, center);

        const [basisA, basisB, basisC] = this.basis;

        const lineA = this.#clipInfiniteLineToRgbCube(worldPoint, basisA.vector);
        const lineB = this.#clipInfiniteLineToRgbCube(worldPoint, basisB.vector);
        const lineC = this.#clipInfiniteLineToRgbCube(worldPoint, basisC.vector);

        const guides = {
            a0: lineA?.[0] || worldPoint,
            a1: lineA?.[1] || worldPoint,
            b0: lineB?.[0] || worldPoint,
            b1: lineB?.[1] || worldPoint,
            c0: lineC?.[0] || worldPoint,
            c1: lineC?.[1] || worldPoint
        };

        const gp = Object.fromEntries(
            Object.entries(guides).map(([k, v]) => [k, this.#projectPoint(v, width, height, scale, center)])
        );

        const mixPath = {
            p0: this.#mixToWorld({ x: 0, y: 0, z: 0 }),
            p1: this.#mixToWorld({ x: current.x, y: 0, z: 0 }),
            p2: this.#mixToWorld({ x: current.x, y: current.y, z: 0 }),
            p3: worldPoint
        };

        const pp = Object.fromEntries(
            Object.entries(mixPath).map(([k, v]) => [k, this.#projectPoint(v, width, height, scale, center)])
        );

        const faces = [
            { name: 'ab0', points: ['O', 'A', 'AB', 'B'], color: 'rgba(255,255,255,0.04)' },
            { name: 'ac0', points: ['O', 'A', 'AC', 'C'], color: 'rgba(255,255,255,0.04)' },
            { name: 'bc0', points: ['O', 'B', 'BC', 'C'], color: 'rgba(255,255,255,0.04)' }
        ];

        const edges = [
            ['O', 'A'], ['O', 'B'], ['O', 'C'],
            ['A', 'AB'], ['A', 'AC'],
            ['B', 'AB'], ['B', 'BC'],
            ['C', 'AC'], ['C', 'BC'],
            ['AB', 'ABC'], ['AC', 'ABC'], ['BC', 'ABC']
        ];

        const faceItems = faces
            .map((face) => ({
                ...face,
                depth: face.points.reduce((sum, key) => sum + this.#rotatePoint(corners[key], center).z, 0) / face.points.length
            }))
            .sort((a, b) => a.depth - b.depth);

        const edgeItems = edges
            .map(([a, b]) => ({
                a,
                b,
                depth: (this.#rotatePoint(corners[a], center).z + this.#rotatePoint(corners[b], center).z) * 0.5
            }))
            .sort((a, b) => a.depth - b.depth);

        const valueColor = this.#basisMixColor(current);
        const c0 = this.#basisMixColor({ x: 0, y: 0, z: 0 });
        const c1 = this.#basisMixColor({ x: current.x, y: 0, z: 0 });
        const c2 = this.#basisMixColor({ x: current.x, y: current.y, z: 0 });
        const c3 = valueColor;

        const principalPlane = this.#bestScreenAlignedPlane();
        const previewPlaneNames = ['xy', 'xz', 'yz'];

        const previewPolygons = previewPlaneNames.map((name) => {
            const poly3 = this.#planeIntersectionPolygonForPlaneName(name);
            const poly2 = poly3.map((p) => this.#projectPoint(p, width, height, scale, center));
            return { name, poly3, poly2 };
        });

        let html = `
            <rect x="0" y="0" width="${width}" height="${height}" fill="url(#cube_bg)"></rect>
<defs>
<linearGradient id="cube_bg" x1="0" x2="1" y1="0" y2="0">
<stop offset="0%" stop-color="${this.background.from}"></stop>
<stop offset="100%" stop-color="${this.background.to}"></stop>
</linearGradient>
</defs>
            `;

        for (const face of faceItems) {
            const d = face.points.map((key, i) => `${i === 0 ? 'M' : 'L'} ${proj[key].x} ${proj[key].y}`).join(' ') + ' Z';
            html += `<path d="${d}" fill="${face.color}" stroke="rgba(255,255,255,0.06)" stroke-width="1"></path>`;
        }

        for (const edge of edgeItems) {
            html += this.#lineSVG(proj[edge.a], proj[edge.b], 'rgba(255,255,255,0.22)', 1.5);
        }

        // Active drag plane should sit behind guide arrows, labels, and handle.
        if (dragPlaneFillSvg) {
            html += `<g id="drag_plane_fill_layer">${dragPlaneFillSvg}</g>`;
        }

        if (dragPlaneGridSvg) {
            html += `<g id="drag_plane_grid_layer">${dragPlaneGridSvg}</g>`;
        }

        const axisColor = 'rgba(190,190,190,0.88)';
        html += this.#arrowSVG(proj.O, basisProj.A, axisColor, 2, 20, 8, 1, 0, 6);
        html += this.#arrowSVG(proj.O, basisProj.B, axisColor, 2, 20, 8, 1, 0, 6);
        html += this.#arrowSVG(proj.O, basisProj.C, axisColor, 2, 20, 8, 1, 0, 6);

        const oLabel = this.#originLabelPos(proj.O, [proj.A, proj.B, proj.C], 18);
        const aLabel = this.#axisLabelPos(proj.O, basisProj.A, 16);
        const bLabel = this.#axisLabelPos(proj.O, basisProj.B, 16);
        const cLabel = this.#axisLabelPos(proj.O, basisProj.C, 16);
        const farLabel = this.#axisLabelPos(proj.O, proj.ABC, 16);

        html += this.#pointMarkerSVG(proj.O, '#000', '#9a9a9a', 1.5, 5);
        html += this.#pointMarkerSVG(proj.A, '#f00', '#9a9a9a', 1.5, 4.5);
        html += this.#pointMarkerSVG(proj.B, '#0f0', '#9a9a9a', 1.5, 4.5);
        html += this.#pointMarkerSVG(proj.C, '#00f', '#9a9a9a', 1.5, 4.5);
        html += this.#pointMarkerSVG(proj.ABC, '#fff', '#9a9a9a', 1.5, 4.5);

        html += this.#textSVG(oLabel, 'K', '#bbbbbb', 0, 0, 12);
        html += this.#textSVG(aLabel, basisA.name, basisA.color, 0, 0, 14);
        html += this.#textSVG(bLabel, basisB.name, basisB.color, 0, 0, 14);
        html += this.#textSVG(cLabel, basisC.name, basisC.color, 0, 0, 14);
        html += this.#textSVG(farLabel, '', '#ffffff', 0, 0, 13);

        const previewOpacity = this.state.previewOpacity;
        let previewHtml = '';

        for (const poly of previewPolygons) {
            if (poly.poly2.length < 3) continue;

            const isActive = this.state.mode === 'point' && this.state.pointDragPlane === poly.name;
            if (isActive) continue;
            if (poly.name !== principalPlane) continue;

            const style = this.#planeOutlineStyle(poly.name, principalPlane, false);

            previewHtml += this.#polygonOutlineSVG(
                poly.poly2,
                style.stroke,
                style.width,
                style.fill,
                style.dash
            );
        }

        const activePreviewPlane =
            (this.state.mode === 'point' && this.state.pointDragPlane)
                ? this.state.pointDragPlane
                : (this.state.previewNear ? principalPlane : null);

        const planeGuideKeys = {
            xy: ['a', 'b'],
            xz: ['a', 'c'],
            yz: ['b', 'c']
        };

        const visibleGuideKeys = activePreviewPlane
            ? (planeGuideKeys[activePreviewPlane] || [])
            : [];

        const directedGuideEndpoints = (k0, k1, basisVector) => {
            const p0 = guides[k0];
            const p1 = guides[k1];
            return this.#dot(this.#sub(p1, p0), basisVector) >= 0
                ? { start: gp[k0], end: gp[k1], worldStart: p0, worldEnd: p1 }
                : { start: gp[k1], end: gp[k0], worldStart: p1, worldEnd: p0 };
        };

        const guideMeta = {
            a: {
                label: `Δ${basisA.name}`,
                ...directedGuideEndpoints('a0', 'a1', basisA.vector)
            },
            b: {
                label: `Δ${basisB.name}`,
                ...directedGuideEndpoints('b0', 'b1', basisB.vector)
            },
            c: {
                label: `Δ${basisC.name}`,
                ...directedGuideEndpoints('c0', 'c1', basisC.vector)
            }
        };

        const guideIsActive = this.state.mode === 'point';
        const guideOpacity = guideIsActive ? 0.94 : 0.7;
        const guideWidth = guideIsActive ? 3.4 : 2.4;
        const guideLabelSize = guideIsActive ? 13 : 12;

        const helperLinesPreviewHtml = visibleGuideKeys.map((key) => {
            const meta = guideMeta[key];
            return this.#guideArrowWithLabelSVG(
                meta.start,
                meta.end,
                this.#worldColorHex(meta.worldStart),
                this.#worldColorHex(meta.worldEnd),
                meta.label,
                {
                    opacity: guideOpacity,
                    width: guideWidth,
                    dash: '4 4',
                    headLen: guideIsActive ? 16 : 14,
                    headWidth: guideIsActive ? 10 : 8,
                    startPad: 5,
                    endPad: 5,
                    labelOffset: 18,
                    labelSize: guideLabelSize
                }
            );
        }).join('');

        const helperPointsPreviewHtml = visibleGuideKeys.map((key) => {
            return [
                this.#guidePointSVG(gp[`${key}0`], this.#worldColorHex(guides[`${key}0`])),
                this.#guidePointSVG(gp[`${key}1`], this.#worldColorHex(guides[`${key}1`]))
            ].join('');
        }).join('');

        const helperTicksPreviewHtml = (() => {
            const parts = [];

            const pushTicks = (a, b) => {
                const dx = b.x - a.x;
                const dy = b.y - a.y;
                const len = Math.hypot(dx, dy) || 1;
                const dirX = dx / len;
                const dirY = dy / len;

                parts.push(this.#guideTickSVG(a, dirX, dirY));
                parts.push(this.#guideTickSVG(b, dirX, dirY));
            };

            for (const key of visibleGuideKeys) {
                pushTicks(gp[`${key}0`], gp[`${key}1`]);
            }

            return parts.join('');
        })();

        html += `
  <g opacity="${previewOpacity}">
    ${previewHtml}
    ${helperLinesPreviewHtml}
    ${helperPointsPreviewHtml}
    ${helperTicksPreviewHtml}
  </g>
`;


        html += `
      <defs>
        ${this.#gradientLineDef('mix_grad_01', pp.p0, pp.p1, c0, c1)}
        ${this.#gradientLineDef('mix_grad_12', pp.p1, pp.p2, c1, c2)}
        ${this.#gradientLineDef('mix_grad_23', pp.p2, pp.p3, c2, c3)}
        ${this.#gradientLineDef('mix_grad_direct', pp.p0, pp.p3, c0, c3)}
        ${this.#gradientLineDef('guide_grad_a', gp.a0, gp.a1, this.#worldColorHex(guides.a0), this.#worldColorHex(guides.a1))}
        ${this.#gradientLineDef('guide_grad_b', gp.b0, gp.b1, this.#worldColorHex(guides.b0), this.#worldColorHex(guides.b1))}
        ${this.#gradientLineDef('guide_grad_c', gp.c0, gp.c1, this.#worldColorHex(guides.c0), this.#worldColorHex(guides.c1))}
      </defs>
    `;

        html += this.#gradientArrowSVG(pp.p0, pp.p3, c0, c3, 1.5, 12, 7, 0.5, 'mix_arrow_direct', 0, 10);
        html += this.#gradientArrowSVG(pp.p0, pp.p1, c0, c1, 4.5, 25, 15, 0.98, 'mix_arrow_01', 0, 5);
        html += this.#gradientArrowSVG(pp.p1, pp.p2, c1, c2, 4.5, 25, 15, 0.98, 'mix_arrow_12', 0, 5);
        html += this.#gradientArrowSVG(pp.p2, pp.p3, c2, c3, 4.5, 25, 15, 0.98, 'mix_arrow_23', 0, 5);

        const coeffTextA = this.#formatCoeff(this.basis[0].name, current.x);
        const coeffTextB = this.#formatCoeff(this.basis[1].name, current.y);
        const coeffTextC = this.#formatCoeff(this.basis[2].name, current.z);

        const offsetA = 18;
        const offsetB = 18;
        const offsetC = 18;

        const sideA = this.#bestLabelSideForSegment(pp.p0, pp.p1, [aLabel], offsetA);
        const sideB = this.#bestLabelSideForSegment(pp.p1, pp.p2, [bLabel], offsetB);
        const sideC = this.#bestLabelSideForSegment(pp.p2, pp.p3, [cLabel], offsetC);

        if (this.#segmentLength2D(pp.p0, pp.p1) > 36) {
            html += this.#textOnSegmentSVG(
                pp.p0,
                pp.p1,
                coeffTextA,
                this.basis[0].color,
                { offset: offsetA, size: 13, side: sideA }
            );
        }

        if (this.#segmentLength2D(pp.p1, pp.p2) > 36) {
            html += this.#textOnSegmentSVG(
                pp.p1,
                pp.p2,
                coeffTextB,
                this.basis[1].color,
                { offset: offsetB, size: 13, side: sideB }
            );
        }

        if (this.#segmentLength2D(pp.p2, pp.p3) > 36) {
            html += this.#textOnSegmentSVG(
                pp.p2,
                pp.p3,
                coeffTextC,
                this.basis[2].color,
                { offset: offsetC, size: 13, side: sideC }
            );
        }

        html += this.#circleSVG(pp.p1, 4, c1, 'rgba(255,255,255,0.35)', 1);
        html += this.#circleSVG(pp.p2, 4, c2, 'rgba(255,255,255,0.35)', 1);

        const pointRingRadius = 16;

        html += `
  <g id="result_point_handle">
    ${this.state.pointHover || this.state.mode === 'point'
                ? `<circle
                 cx="${pointProj.x}"
                 cy="${pointProj.y}"
                 r="${pointRingRadius}"
                 fill="rgba(255,255,255,0.08)"
                 stroke="rgba(255,255,255,0.35)"
                 stroke-width="1.25"
               />`
                : ''
            }
    <circle cx="${pointProj.x}" cy="${pointProj.y}" r="14" fill="transparent" stroke="transparent" />
    <circle cx="${pointProj.x}" cy="${pointProj.y}" r="7" fill="${valueColor}" stroke="white" stroke-width="1.75" />
  </g>
`;

        if (dragPlaneOutlineSvg) {
            html += `<g id="drag_plane_outline_layer">${dragPlaneOutlineSvg}</g>`;
        }

        this.svg.innerHTML = html;

        if (this.labelEl) {
            this.labelEl.textContent = this.state.pointHover ? 'drag point' : 'drag to rotate';
        }
    }

    #basisMixColor(point) {
        const world = this.#clampWorldPointToRgbCube(this.#mixToWorld(point));
        const toHexPair = (v) => Math.round(v * 255).toString(16).padStart(2, '0');
        return `#${toHexPair(world.x)}${toHexPair(world.y)}${toHexPair(world.z)}`;
    }

    #hexChannel(color, index) {
        const normalized = color.startsWith('#') ? color.slice(1) : color;
        const full = normalized.length === 3
            ? normalized.split('').map((ch) => ch + ch).join('')
            : normalized.padEnd(6, '0');
        return parseInt(full.slice(index * 2, index * 2 + 2), 16) || 0;
    }

    #setLabel(text) {
        if (this.labelEl) this.labelEl.textContent = text;
    }

    #emitChange() {
        if (this.onChange) {
            this.onChange({
                value: this.getValue(),
                coefficients: this.getCoefficients(),
                basis: this.getBasis(),
                worldPoint: this.#currentWorldPoint()
            });
        }
    }

    #setupInteraction() {
        const onWheel = (ev) => {
            ev.preventDefault();
            const zoomFactor = Math.exp(-ev.deltaY * 0.0015);
            this.state.zoom = Math.max(this.state.minZoom, Math.min(this.state.maxZoom, this.state.zoom * zoomFactor));
            this.draw();
        };

        const beginDrag = (ev) => {
            ev.preventDefault();

            if (ev.touches && ev.touches.length === 2) {
                this.state.mode = null;
                this.state.dragging = false;
                this.state.pinchDistance = this.#touchDistance(ev);
                this.state.pointHover = false;
                this.#setLabel('pinch to zoom');
                return;
            }

            const p = this.#pointerPos(ev);
            this.state.lastX = p.x;
            this.state.lastY = p.y;
            this.state.pinchDistance = null;

            if (this.#isPointerNearValuePoint(ev)) {
                this.state.mode = 'point';
                this.state.dragging = true;
                this.state.pointHover = true;
                this.state.previewNear = true;
                this.state.pointDragPlane = this.#bestScreenAlignedPlane();
                this.state.dragPlaneDef = this.#makeDragPlaneWorldDefinition(this.state.pointDragPlane);
                this.state.dragPlaneCache = this.#buildDragPlaneCache();
                this.state.dragPointerAnchor = { x: p.x, y: p.y };
                this.state.dragWorldAnchor = this.#currentWorldPoint();
                this.#setPreviewVisible(true);
                this.#setLabel(`drag point (${this.state.pointDragPlane})`);
                this.draw();
                return;
            }

            this.state.mode = 'rotate';
            this.state.dragging = true;
            this.state.pointHover = false;
            this.#setLabel('drag to rotate');
            this.draw();
        };

        const moveDrag = (ev) => {
            if (ev.touches && ev.touches.length === 2) {
                ev.preventDefault();
                const d = this.#touchDistance(ev);
                if (d && this.state.pinchDistance) {
                    const ratio = d / this.state.pinchDistance;
                    this.state.zoom = Math.max(this.state.minZoom, Math.min(this.state.maxZoom, this.state.zoom * ratio));
                    this.draw();
                }
                this.state.pinchDistance = d;
                return;
            }

            if (!this.state.dragging) {
                if (ev.type === 'mousemove') {
                    const hoveringPoint = this.#isPointerNearValuePoint(ev);
                    const previewNear = this.#isPointerNearPreviewTrigger(ev);

                    let needsDraw = false;

                    if (hoveringPoint !== this.state.pointHover) {
                        this.state.pointHover = hoveringPoint;
                        needsDraw = true;
                    }

                    if (previewNear !== this.state.previewNear) {
                        this.state.previewNear = previewNear;
                        this.#setPreviewVisible(previewNear);
                        needsDraw = true;
                    }

                    this.svg.style.cursor = hoveringPoint ? 'pointer' : 'grab';
                    this.#setLabel(hoveringPoint ? 'drag point' : 'drag to rotate');

                    if (needsDraw) {
                        this.draw();
                    }
                }
                return;
            }

            ev.preventDefault();
            const p = this.#pointerPos(ev);
            const dx = p.x - this.state.lastX;
            const dy = p.y - this.state.lastY;
            this.state.lastX = p.x;
            this.state.lastY = p.y;

            if (this.state.mode === 'point') {
                const svgPt = this.#clientToSvgPoint(p.x, p.y);
                if (!svgPt) return;
                this.#dragValueInAlignedPlaneToPointer(svgPt.x, svgPt.y);
                return;
            }
            if (this.state.mode === 'rotate') {
                this.state.yaw += dx * 0.005;
                this.state.pitch += dy * 0.005;
                const maxPitch = Math.PI * 0.49;
                this.state.pitch = Math.max(-maxPitch, Math.min(maxPitch, this.state.pitch));
                this.draw();
            }
        };

        const endDrag = (ev) => {
            this.state.dragging = false;
            this.state.pinchDistance = null;
            this.state.mode = null;
            this.state.pointDragPlane = null;
            this.state.dragPlaneDef = null;
            this.state.dragPlaneCache = null;
            this.state.dragPointerAnchor = null;
            this.state.dragWorldAnchor = null;
            this.state.pointHover = ev ? this.#isPointerNearValuePoint(ev) : false;
            this.state.previewNear = ev ? this.#isPointerNearPreviewTrigger(ev) : false;
            this.#setPreviewVisible(this.state.previewNear);
            this.svg.style.cursor = this.state.pointHover ? 'pointer' : 'grab';
            this.#setLabel(this.state.pointHover ? 'drag point' : 'drag to rotate');
            this.draw();
        };

        this.boundHandlers = {
            onMouseDown: beginDrag,
            onMouseMove: moveDrag,
            onMouseUp: endDrag,
            onTouchStart: beginDrag,
            onTouchMove: moveDrag,
            onTouchEnd: endDrag,
            onWheel
        };

        this.svg.addEventListener('mousedown', beginDrag);
        window.addEventListener('mousemove', moveDrag);
        window.addEventListener('mouseup', endDrag);

        this.svg.addEventListener('touchstart', beginDrag, { passive: false });
        window.addEventListener('touchmove', moveDrag, { passive: false });
        window.addEventListener('touchend', endDrag);
        window.addEventListener('touchcancel', endDrag);

        this.svg.addEventListener('wheel', onWheel, { passive: false });
        this.svg.addEventListener('mousemove', moveDrag);
    }
}