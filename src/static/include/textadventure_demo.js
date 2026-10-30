// The text adventure to play on its page before building it
// (content/textadventure): the same classes as the activity's solution
// (loesung/adventure.rb), in JavaScript, and the night in the school
// (starter/schule.rb). <div class='adventure-demo'></div> becomes a small
// terminal: what one types after »>« goes to Game.handle, its answer below.
(function () {
    'use strict';

    class Room {
        constructor(name, description) {
            this.name = name;
            this.description = description;
            this.exits = new Map();
            this.locks = new Map();
            this.items = [];
        }
        connect(direction, room) { this.exits.set(direction, room); }
        exitTo(direction) { return this.exits.get(direction) || null; }
        directions() { return [...this.exits.keys()]; }
        addItem(item) { this.items.push(item); }
        take(name) {
            const item = this.items.find((i) => i.name === name);
            if (item) this.items.splice(this.items.indexOf(item), 1);
            return item || null;
        }
        lock(direction, keyName) { this.locks.set(direction, keyName); }
        isLocked(direction) { return this.locks.has(direction); }
        unlock(keyName) {
            const doors = [...this.locks].filter(([, key]) => key === keyName).map(([d]) => d);
            doors.forEach((d) => this.locks.delete(d));
            return doors.length > 0;
        }
        isDark() { return false; }
        isGoal() { return false; }
        describe() {
            const lines = [this.name, this.description];
            if (this.items.length) lines.push('Hier liegt: ' + this.items.map((i) => i.name).join(', '));
            const exits = this.directions().map((d) => (this.isLocked(d) ? d + ' (verschlossen)' : d));
            lines.push('Ausgänge: ' + (exits.length ? exits.join(', ') : 'keine'));
            return lines.join('\n');
        }
    }

    class DarkRoom extends Room {
        isDark() { return true; }
    }

    class GoalRoom extends Room {
        isGoal() { return true; }
    }

    class Item {
        constructor(name, description) {
            this.name = name;
            this.description = description;
        }
        use() { return 'Damit kannst du hier nichts anfangen.'; }
        shines() { return false; }
    }

    class Key extends Item {
        use(player) {
            return player.room.unlock(this.name) ? 'Du schließt die Tür auf.' : 'Hier passt ' + this.name + ' in kein Schloss.';
        }
    }

    class Food extends Item {
        use(player) {
            player.remove(this);
            return 'Du isst ' + this.name + '. ' + this.description;
        }
    }

    class Lamp extends Item {
        use() { return this.name + ' leuchtet schon.'; }
        shines() { return true; }
    }

    class Player {
        constructor(room) {
            this.room = room;
            this.bagItems = [];
        }
        look() {
            if (this.room.isDark() && !this.hasLight()) return 'Es ist stockdunkel. Du siehst nichts.';
            return this.room.describe();
        }
        go(direction) {
            const target = this.room.exitTo(direction);
            if (!target) return 'Da geht es nicht weiter.';
            if (this.room.isLocked(direction)) return 'Die Tür nach ' + direction + ' ist verschlossen.';
            this.room = target;
            return this.look();
        }
        take(name) {
            if (this.room.isDark() && !this.hasLight()) return 'Im Dunkeln findest du nichts.';
            const item = this.room.take(name);
            if (!item) return 'Hier liegt nichts mit dem Namen ' + name + '.';
            this.bagItems.push(item);
            return 'Du nimmst ' + item.name + '.';
        }
        remove(item) { this.bagItems.splice(this.bagItems.indexOf(item), 1); }
        hasLight() { return this.bagItems.some((i) => i.shines()); }
        bag() {
            if (!this.bagItems.length) return 'Deine Tasche ist leer.';
            return 'In deiner Tasche: ' + this.bagItems.map((i) => i.name).join(', ');
        }
        use(name) {
            const item = this.bagItems.find((i) => i.name === name);
            return item ? item.use(this) : 'Du hast nichts mit dem Namen ' + name + '.';
        }
        examine(name) {
            const item = this.bagItems.concat(this.room.items).find((i) => i.name === name);
            return item ? item.description : 'Hier gibt es nichts mit dem Namen ' + name + '.';
        }
    }

    class Game {
        constructor(player) {
            this.player = player;
            this.over = false;
        }
        start() { return this.player.look(); }
        isWon() { return this.player.room.isGoal(); }
        handle(line) {
            const text = line.trim().toLowerCase();
            const space = text.indexOf(' ');
            const verb = space < 0 ? text : text.slice(0, space);
            const word = space < 0 ? '' : text.slice(space + 1);
            let answer;
            if (verb === 'gehe') answer = this.player.go(word);
            else if (verb === 'nimm') answer = this.player.take(word);
            else if (verb === 'benutze') answer = this.player.use(word);
            else if (verb === 'untersuche') answer = this.player.examine(word);
            else if (verb === 'schau') answer = this.player.look();
            else if (verb === 'tasche') answer = this.player.bag();
            else if (verb === 'ende') {
                this.over = true;
                answer = 'Bis bald.';
            } else answer = 'Das verstehe ich nicht. Befehle: gehe, nimm, benutze, untersuche, schau, tasche, ende';
            if (this.isWon()) this.over = true;
            return answer;
        }
    }

    // the night in the school, as starter/schule.rb
    function buildSchool() {
        const classroom = new Room('Raum 204', 'Du bist über deinem Heft eingeschlafen. Draußen ist es dunkel, die Schule ist still.');
        const hall = new Room('Flur', 'Nur die grünen Notausgangsschilder leuchten.');
        const staff = new Room('Lehrerzimmer', 'Stapel von Heften, eine kalte Kaffeemaschine.');
        const stairs = new Room('Treppenhaus', 'Deine Schritte hallen.');
        const entrance = new Room('Eingangshalle', 'Hinter der großen Glastür liegt der Schulhof.');
        const caretaker = new Room('Hausmeisterraum', 'Werkzeug an der Wand, ein Radio, das leise rauscht.');
        const cellar = new DarkRoom('Keller', 'Heizungsrohre, alte Stühle und ein Schlüsselbrett.');
        const outside = new GoalRoom('Schulhof', 'Kühle Nachtluft. Du bist draußen. Geschafft!');
        classroom.connect('westen', hall);
        hall.connect('osten', classroom);
        hall.connect('norden', staff);
        staff.connect('süden', hall);
        hall.connect('unten', stairs);
        stairs.connect('oben', hall);
        stairs.connect('unten', entrance);
        entrance.connect('oben', stairs);
        entrance.connect('westen', caretaker);
        caretaker.connect('osten', entrance);
        entrance.connect('unten', cellar);
        cellar.connect('oben', entrance);
        entrance.connect('süden', outside);
        hall.lock('norden', 'lehrerschlüssel');
        entrance.lock('süden', 'haustürschlüssel');
        classroom.addItem(new Item('zettel', '»Der Hausmeister lässt seine Taschenlampe nie aus dem Raum.«'));
        classroom.addItem(new Food('apfel', 'Knackig und süß.'));
        caretaker.addItem(new Lamp('taschenlampe', 'Eine schwere Taschenlampe. Sie leuchtet.'));
        cellar.addItem(new Key('lehrerschlüssel', 'Am Anhänger steht: Lehrerzimmer.'));
        staff.addItem(new Key('haustürschlüssel', 'Ein großer Schlüssel mit einem roten Anhänger.'));
        return new Player(classroom);
    }

    function el(tag, className, text) {
        const node = document.createElement(tag);
        if (className) node.className = className;
        if (text !== undefined) node.textContent = text;
        return node;
    }

    // An answer in colour, as the activity's step »Farbe im Terminal«
    // paints it (paint in loesung/colors.rb): the room's name (the first
    // line of an answer with »Ausgänge:«), the items after »Hier liegt:«
    // and »In deiner Tasche:«, the exits after »Ausgänge:«. The game's text
    // stays as it is; only the display changes.
    const LISTS = [['Hier liegt: ', 'item'], ['In deiner Tasche: ', 'item'], ['Ausgänge: ', 'exit']];

    function paint(text) {
        const lines = text.split('\n');
        const room = lines.some((line) => line.startsWith('Ausgänge: '));
        const out = document.createDocumentFragment();
        lines.forEach((line, i) => {
            if (i > 0) out.append('\n');
            const list = LISTS.find(([label]) => line.startsWith(label));
            if (i === 0 && room) {
                out.append(el('span', 'adventure-room', line));
            } else if (list && line !== 'Ausgänge: keine') {
                out.append(list[0]);
                line.slice(list[0].length).split(', ').forEach((name, k) => {
                    if (k > 0) out.append(', ');
                    out.append(el('span', 'adventure-' + list[1], name));
                });
            } else {
                out.append(line);
            }
        });
        return out;
    }

    function mount(box) {
        const screen = el('div', 'adventure-screen');
        screen.setAttribute('role', 'log');
        screen.setAttribute('aria-live', 'polite');
        // plain: an input row without the frame of a form (default.css)
        const form = el('form', 'adventure-line plain');
        const prompt = el('span', 'adventure-prompt', '>');
        prompt.setAttribute('aria-hidden', 'true');
        const input = el('input', 'adventure-input');
        input.type = 'text';
        input.autocomplete = 'off';
        input.spellcheck = false;
        input.setAttribute('autocapitalize', 'off');
        input.setAttribute('aria-label', 'Befehl, zum Beispiel gehe westen');
        input.placeholder = 'gehe westen';
        form.append(prompt, input);
        const restart = el('button', 'btn btn-sm btn-outline-secondary adventure-restart');
        restart.type = 'button';
        restart.innerHTML = "<i class='bi bi-arrow-counterclockwise' aria-hidden='true'></i>Neu anfangen";
        const bar = el('div', 'adventure-bar');
        bar.append(el('span', 'adventure-help', 'Befehle: gehe, nimm, benutze, untersuche, schau, tasche'), restart);
        box.append(screen, form, bar);

        let game;
        const print = (text, kind) => {
            const line = el('div', 'adventure-' + kind);
            if (kind === 'answer') line.append(paint(text));
            else line.textContent = text;
            screen.appendChild(line);
            screen.scrollTop = screen.scrollHeight;
        };
        const begin = () => {
            screen.replaceChildren();
            game = new Game(buildSchool());
            print(game.start(), 'answer');
            input.disabled = false;
        };
        form.addEventListener('submit', (e) => {
            e.preventDefault();
            const line = input.value;
            input.value = '';
            if (!line.trim() || game.over) return;
            print('> ' + line.trim(), 'command');
            print(game.handle(line), 'answer');
            if (game.over) {
                input.disabled = true;
                print(game.isWon() ? 'Ende: Du bist draußen. Und jetzt baust du das Spiel selbst.' : 'Ende.', 'end');
            }
        });
        restart.addEventListener('click', () => {
            begin();
            input.focus();
        });
        begin();
    }

    function start() {
        document.querySelectorAll('.adventure-demo').forEach((box) => {
            if (box.dataset.ready) return;
            box.dataset.ready = '1';
            mount(box);
        });
    }
    if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', start);
    else start();
})();
