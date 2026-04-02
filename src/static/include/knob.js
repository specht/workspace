class FancyKnob {
    constructor(container, options = {}) {
        this.container = container;

        this.opts = {
            label: options.label ?? 'KNOB',
            min: options.min ?? -1,
            max: options.max ?? 1,
            step: options.step ?? 0.1,
            value: options.value ?? 0,
            color: options.color ?? '#ff8a00',
            size: options.size ?? 70,
            sensitivity: options.sensitivity ?? 0.003,
            unit: options.unit ?? '',
            decimals: options.decimals ?? null,
            bipolar: options.bipolar ?? null,
            showCenterLabel: options.showCenterLabel ?? true,
            centerLabel: options.centerLabel ?? null,
            ticks: options.ticks ?? null,
            onInput: options.onInput ?? null,
            onChange: options.onChange ?? null,
            formatValue: options.formatValue ?? null,
            horizontal: options.horizontal ?? false,

            // new:
            sweepStart: options.sweepStart ?? -135,
            sweepEnd: options.sweepEnd ?? 135,
            indicatorStyle: options.indicatorStyle ?? 'arc', // arc or dot
            showTicks: options.showTicks ?? true,
        };

        if (this.opts.bipolar === null) {
            this.opts.bipolar = this.opts.min < 0 && this.opts.max > 0;
        }

        this.value = this.clamp(this.snap(this.opts.value));
        this.dragging = false;
        this.startX = 0;
        this.startY = 0;
        this.startValue = this.value;

        this.size = this.opts.size;
        this.cx = 100;
        this.cy = 100;
        this.rArc = 74;

        this.sweepStart = this.opts.sweepStart;
        this.sweepEnd = this.opts.sweepEnd;

        this.uid1 = this.uid();
        this.uid2 = this.uid();
        this.uid3 = this.uid();

        this.build();
        this.render();
        this.attachEvents();
    }

    clamp(v) {
        return Math.min(this.opts.max, Math.max(this.opts.min, v));
    }

    snap(v) {
        const step = this.opts.step;
        const snapped = Math.round((v - this.opts.min) / step) * step + this.opts.min;
        const precision = this.getStepPrecision(step);
        return Number(snapped.toFixed(precision));
    }

    getStepPrecision(step) {
        const s = String(step);
        if (!s.includes('.')) return 0;
        return s.split('.')[1].length;
    }

    formatNumber(v) {
        if (this.opts.formatValue) return this.opts.formatValue(v, this);

        const decimals = this.opts.decimals ?? this.getStepPrecision(this.opts.step);
        const prefix = v > 0 && this.opts.bipolar ? '+' : '';
        return `${prefix}${v.toFixed(decimals)}${this.opts.unit}`;
    }

    norm(v) {
        return (v - this.opts.min) / (this.opts.max - this.opts.min);
    }

    valueToAngle(v) {
        return this.sweepStart + this.norm(v) * (this.sweepEnd - this.sweepStart);
    }

    polarToCartesian(r, angleDeg) {
        const a = (angleDeg - 90) * Math.PI / 180;
        return {
            x: this.cx + r * Math.cos(a),
            y: this.cy + r * Math.sin(a)
        };
    }

    describeArc(r, startAngle, endAngle) {
        const start = this.polarToCartesian(r, endAngle);
        const end = this.polarToCartesian(r, startAngle);
        const largeArcFlag = Math.abs(endAngle - startAngle) > 180 ? 1 : 0;
        return `M ${start.x} ${start.y} A ${r} ${r} 0 ${largeArcFlag} 0 ${end.x} ${end.y}`;
    }

    build() {
        this.root = document.createElement('div');
        this.root.className = `knob${this.opts.horizontal ? ' knob-horizontal' : ''}`;

        this.root.innerHTML = `
            <svg viewBox="0 0 200 200" aria-label="${this.opts.label}" role="slider">
                <defs>
                    <filter id="shadow-${this.uid1}" x="-50%" y="-50%" width="200%" height="200%">
                        <feDropShadow dx="0" dy="6" stdDeviation="6" flood-color="rgba(0,0,0,0.5)"/>
                    </filter>
                    <radialGradient id="knobGradOuter-${this.uid2}" cx="35%" cy="30%">
                        <stop offset="0%" stop-color="#3c4047"/>
                        <stop offset="55%" stop-color="#22262c"/>
                        <stop offset="100%" stop-color="#0e1013"/>
                    </radialGradient>
                    <radialGradient id="knobGradInner-${this.uid3}" cx="35%" cy="30%">
                        <stop offset="0%" stop-color="#2b2f35"/>
                        <stop offset="100%" stop-color="#131519"/>
                    </radialGradient>
                </defs>

                <path class="knob-focus-ring" fill="none" stroke="${this.opts.color}" stroke-width="2.5" opacity="0.65"></path>
                <path class="track" fill="none" stroke="#0f1012" stroke-width="16" stroke-linecap="round"></path>
                <path class="track-inner" fill="none" stroke="#2d3138" stroke-width="8" stroke-linecap="round" opacity="0.65"></path>
                <path class="active" fill="none" stroke="${this.opts.color}" stroke-width="9" stroke-linecap="round"></path>
                <circle class="value-dot" r="5" fill="${this.opts.color}"></circle>

                <g class="tick-layer"></g>
                <g class="label-layer"></g>

                <circle cx="100" cy="100" r="57" fill="url(#knobGradOuter-${this.uid2})" filter="url(#shadow-${this.uid1})"></circle>
                <circle cx="100" cy="100" r="44" fill="url(#knobGradInner-${this.uid3})"></circle>
                <circle cx="100" cy="100" r="18" fill="#0f1012"></circle>

                <line class="indicator-shadow" x1="100" y1="100" x2="100" y2="58" stroke="rgba(0,0,0,0.45)" stroke-width="6" stroke-linecap="round"></line>
                <line class="indicator" x1="100" y1="100" x2="100" y2="60" stroke="${this.opts.color}" stroke-width="4" stroke-linecap="round"></line>
                <circle cx="100" cy="100" r="6" fill="#08090a"></circle>
            </svg>
            <div class="knob-title"></div>
            <div class="knob-value"></div>
        `;

        this.scale = this.size / 170;

        this.container.appendChild(this.root);

        this.titleEl = this.root.querySelector('.knob-title');
        this.valueEl = this.root.querySelector('.knob-value');
        this.svg = this.root.querySelector('svg');
        this.trackEl = this.root.querySelector('.track');
        this.trackInnerEl = this.root.querySelector('.track-inner');
        this.activeEl = this.root.querySelector('.active');
        this.focusRingEl = this.root.querySelector('.knob-focus-ring');
        this.indicatorEl = this.root.querySelector('.indicator');
        this.indicatorShadowEl = this.root.querySelector('.indicator-shadow');
        this.tickLayer = this.root.querySelector('.tick-layer');
        this.labelLayer = this.root.querySelector('.label-layer');
        this.valueDotEl = this.root.querySelector('.value-dot');

        this.titleEl.textContent = this.opts.label;

        this.svg.style.width = this.size + 'px';
        this.svg.style.height = this.size + 'px';

        if (!this.opts.horizontal) {
            this.root.style.width = this.size + 'px';
        }

        const trackPath = this.describeArc(this.rArc, this.sweepStart, this.sweepEnd);
        this.trackEl.setAttribute('d', trackPath);
        this.trackInnerEl.setAttribute('d', this.describeArc(this.rArc, this.sweepStart, this.sweepEnd));
        this.focusRingEl.setAttribute('d', this.describeArc(this.rArc + 10, this.sweepStart, this.sweepEnd));

        if (this.opts.indicatorStyle === 'dot') {
            this.trackEl.style.display = 'none';
            this.trackInnerEl.style.display = 'none';
            this.activeEl.style.display = 'none';
        } else {
            this.valueDotEl.style.display = 'none';
        }        

        this.buildTicksAndLabels();
    }

    uid() {
        return Math.random().toString(36).slice(2, 9);
    }

    buildTicksAndLabels() {
        this.tickLayer.innerHTML = '';
        this.labelLayer.innerHTML = '';

        const ticks = this.opts.ticks ?? this.defaultTicks();

        for (const tick of ticks) {
            const angle = this.valueToAngle(tick.value);
            const inner = this.polarToCartesian(84, angle);
            const outer = this.polarToCartesian(92, angle);

            if (this.opts.showTicks) {
                const line = document.createElementNS('http://www.w3.org/2000/svg', 'line');
                line.setAttribute('x1', inner.x);
                line.setAttribute('y1', inner.y);
                line.setAttribute('x2', outer.x);
                line.setAttribute('y2', outer.y);
                line.setAttribute('stroke', tick.major ? '#f0f0f0' : '#9ea4ad');
                line.setAttribute('stroke-width', tick.major ? '2' : '2');
                line.setAttribute('stroke-linecap', 'round');
                line.setAttribute('opacity', tick.major ? '0.75' : '0.35');
                this.tickLayer.appendChild(line);
            }

            if (tick.label != null) {
                const p = this.polarToCartesian(108, angle);
                const text = document.createElementNS('http://www.w3.org/2000/svg', 'text');
                text.setAttribute('x', p.x);
                text.setAttribute('y', p.y + 4);
                text.setAttribute('text-anchor', 'middle');
                text.setAttribute('class', `knob-label ${tick.center ? 'center' : ''}`);
                text.textContent = tick.label;
                this.labelLayer.appendChild(text);
            }
        }
    }

    defaultTicks() {
        if (this.opts.bipolar) {
            const center = this.opts.centerLabel ?? this.formatTick(0);
            return [
                { value: this.opts.min, label: this.formatTick(this.opts.min), major: true },
                { value: (this.opts.min + 0) / 2, label: this.formatTick((this.opts.min + 0) / 2), major: true },
                { value: 0, label: this.opts.showCenterLabel ? center : null, major: true, center: true },
                { value: (this.opts.max + 0) / 2, label: this.formatTick((this.opts.max + 0) / 2), major: true },
                { value: this.opts.max, label: this.formatTick(this.opts.max), major: true }
            ];
        }

        return [
            { value: this.opts.min, label: this.formatTick(this.opts.min), major: true },
            { value: this.opts.min + (this.opts.max - this.opts.min) * 0.25, label: this.formatTick(this.opts.min + (this.opts.max - this.opts.min) * 0.25), major: false },
            { value: this.opts.min + (this.opts.max - this.opts.min) * 0.5, label: this.formatTick(this.opts.min + (this.opts.max - this.opts.min) * 0.5), major: true, center: true },
            { value: this.opts.min + (this.opts.max - this.opts.min) * 0.75, label: this.formatTick(this.opts.min + (this.opts.max - this.opts.min) * 0.75), major: false },
            { value: this.opts.max, label: this.formatTick(this.opts.max), major: true }
        ];
    }

    formatTick(v) {
        const decimals = Math.min(1, this.getStepPrecision(this.opts.step));
        const rounded = Number(v.toFixed(decimals));

        if (this.opts.unit === '%') return `${rounded}%`;
        return String(rounded);
    }

    attachEvents() {
        this.svg.addEventListener('pointerdown', (e) => {
            this.dragging = true;
            this.startX = e.clientX;
            this.startY = e.clientY;
            this.startValue = this.value;
            this.root.classList.add('dragging');
            this.svg.setPointerCapture(e.pointerId);
        });

        this.svg.addEventListener('pointermove', (e) => {
            if (!this.dragging) return;

            const deltaY = this.startY - e.clientY;
            const deltaX = e.clientX - this.startX;

            const delta = (Math.abs(deltaY) > Math.abs(deltaX))
                ? deltaY + deltaX * 0.3
                : deltaX + deltaY * 0.3;

            const raw = this.startValue + delta * this.opts.sensitivity * (this.opts.max - this.opts.min);

            this.setValue(raw, true);
        });

        const stopDrag = () => {
            if (!this.dragging) return;
            this.dragging = false;
            this.root.classList.remove('dragging');
            if (typeof this.opts.onChange === 'function') {
                this.opts.onChange(this.value, this);
            }
        };

        this.svg.addEventListener('pointerup', stopDrag);
        this.svg.addEventListener('pointercancel', stopDrag);
        this.svg.addEventListener('dblclick', () => {
            const reset = this.opts.bipolar ? 0 : this.opts.min;
            this.setValue(reset, true);
            if (typeof this.opts.onChange === 'function') {
                this.opts.onChange(this.value, this);
            }
        });
    }

    getActiveColor() {
        if (!this.opts.bipolar) return this.opts.color;
        return this.opts.color;
    }

    mix(a, b, t) {
        const c1 = this.hexToRgb(a);
        const c2 = this.hexToRgb(b);
        const r = Math.round(c1.r + (c2.r - c1.r) * t);
        const g = Math.round(c1.g + (c2.g - c1.g) * t);
        const b2 = Math.round(c1.b + (c2.b - c1.b) * t);
        return `rgb(${r}, ${g}, ${b2})`;
    }

    hexToRgb(hex) {
        const h = hex.replace('#', '');
        const full = h.length === 3 ? h.split('').map(x => x + x).join('') : h;
        const num = parseInt(full, 16);
        return {
            r: (num >> 16) & 255,
            g: (num >> 8) & 255,
            b: num & 255
        };
    }

    render() {
        const angle = this.valueToAngle(this.value);
        const color = this.getActiveColor();
        const dotPos = this.polarToCartesian(this.rArc, angle);
        this.valueDotEl.setAttribute('cx', dotPos.x);
        this.valueDotEl.setAttribute('cy', dotPos.y);
        this.valueDotEl.setAttribute('fill', color);

        if (this.opts.indicatorStyle === 'arc') {
            if (this.opts.bipolar) {
                const zeroAngle = this.valueToAngle(0);
                if (this.value >= 0) {
                    this.activeEl.setAttribute('d', this.describeArc(this.rArc, zeroAngle, angle));
                } else {
                    this.activeEl.setAttribute('d', this.describeArc(this.rArc, angle, zeroAngle));
                }
            } else {
                this.activeEl.setAttribute('d', this.describeArc(this.rArc, this.sweepStart, angle));
            }
        }

        this.activeEl.setAttribute('stroke', color);
        this.indicatorEl.setAttribute('stroke', color);
        this.focusRingEl.setAttribute('stroke', this.opts.color);

        this.indicatorEl.setAttribute('transform', `rotate(${angle} 100 100)`);
        this.indicatorShadowEl.setAttribute('transform', `rotate(${angle} 100 100)`);

        this.valueEl.textContent = this.formatNumber(this.value);

        this.svg.setAttribute('aria-valuemin', this.opts.min);
        this.svg.setAttribute('aria-valuemax', this.opts.max);
        this.svg.setAttribute('aria-valuenow', this.value);
    }

    setValue(v, fireInput = false) {
        const next = this.clamp(this.snap(v));
        if (next === this.value) return;
        this.value = next;
        this.render();
        if (fireInput && typeof this.opts.onInput === 'function') {
            this.opts.onInput(this.value, this);
        }
    }

    getValue() {
        return this.value;
    }

    updateOptions(nextOpts = {}) {
        const hadExplicitBipolar = Object.prototype.hasOwnProperty.call(nextOpts, 'bipolar');

        this.opts = {
            ...this.opts,
            ...nextOpts
        };

        if (hadExplicitBipolar) {
            this.opts.bipolar = nextOpts.bipolar;
        } else if ('min' in nextOpts || 'max' in nextOpts) {
            this.opts.bipolar = this.opts.min < 0 && this.opts.max > 0;
        }

        this.value = this.clamp(this.snap(
            Object.prototype.hasOwnProperty.call(nextOpts, 'value')
                ? nextOpts.value
                : this.value
        ));

        this.buildTicksAndLabels();
        this.render();
    }

    setRange(min, max, value = this.value, extraOpts = {}) {
        this.updateOptions({
            min,
            max,
            value,
            ...extraOpts
        });
    }
}