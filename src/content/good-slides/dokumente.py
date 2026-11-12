# The presentations of »Eine Folie, ein Gedanke«: a Referat about the cranes
# of the Kranichsee as someone in the 7b put it together, the sentences of
# the trip newspaper's article on the slides in 18 pt under titles of one
# word, one slide with two ideas (the starting file); the same Referat as
# it should be, one idea a slide, keywords, titles that say something, the
# sentences in the notes (the
# solution, task-examples.yaml); and one with keywords but the sentences
# deleted instead of moved (a stage that must fail). Each as .pptx and .odp
# (src/office/deck.py). Run in the office image:
#
#     src/scripts/office python3 src/content/good-slides/dokumente.py

import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, '..', '..', 'office'))
from deck import Deck, slide_pictures  # noqa: E402

STATIC = os.path.join(HERE, '..', '..', 'static', 'good-slides')
STARTER = '01 Kraniche'

# the sentences, from the trip newspaper's article (newspaper-page): each a
# bullet of the starting file, as the Referat had them; »Zug« holds two
# ideas (the migration and the calls), which the solution splits
SENTENCES = {
    'Allgemeines': [
        'Ein Kranich steht gut einen Meter hoch, und seine Flügel sind von Spitze zu Spitze etwa zwei Meter breit.',
        'Erkennen kann man ihn am roten Fleck auf dem Kopf und am schwarz-weißen Hals.',
        'Im Flug streckt er den Hals lang nach vorn, ganz anders als ein Reiher.',
    ],
    'Zug': [
        'Im Herbst machen am Kranichsee Tausende Kraniche Rast.',
        'Sie kommen aus Skandinavien und aus dem Baltikum und fliegen weiter nach Spanien oder Nordafrika.',
        'Tagsüber suchen sie auf den abgeernteten Feldern nach Körnern, abends kommen sie zum Schlafen ins flache Wasser am Ufer.',
        'Dort sind sie sicher, weil kein Fuchs sie im Wasser ungesehen erreicht.',
        'Wenn die Kraniche am Abend über den See fliegen, hört man sie schon von Weitem.',
        'Sie rufen laut und trompetend, und die Schwärme ziehen in langen Ketten und Keilen über den Himmel.',
        'An manchen Abenden sind es mehr als zehntausend Vögel.',
    ],
    'Schutz': [
        'Früher brüteten in Deutschland nur noch wenige Kraniche.',
        'Seit Moore und Feuchtwiesen geschützt werden, sind es wieder viele Tausend Paare.',
    ],
}

# the slides as they should be: one idea each, a title that says it,
# keywords; in the notes a few cues of what to tell (a cue card, not a text
# to read out)
GOOD = [
    ('Der Kranich ist ein großer Vogel', ['gut 1 m hoch', '2 m Flügelspannweite', 'roter Fleck, schwarz-weißer Hals'],
     SENTENCES['Allgemeines'], ['Hals im Flug lang nach vorn', 'anders als der Reiher']),
    ('Kraniche ziehen im Herbst nach Süden', ['Rast am Kranichsee', 'aus Skandinavien nach Spanien', 'nachts im flachen Wasser'],
     SENTENCES['Zug'][:4], ['Tausende rasten am See', 'Skandinavien, Baltikum: weiter nach Spanien, Nordafrika',
                            'tagsüber Körner auf den Feldern', 'im Wasser sicher vor dem Fuchs']),
    ('Man hört Kraniche, bevor man sie sieht', ['laute, trompetende Rufe', 'Ketten und Keile am Himmel', 'bis zu 10.000 Vögel'],
     SENTENCES['Zug'][4:], ['abends über dem See, von Weitem zu hören']),
    ('Schutzgebiete brachten die Kraniche zurück', ['früher nur wenige Paare', 'heute viele Tausend'],
     SENTENCES['Schutz'], ['Moore und Feuchtwiesen geschützt']),
]


def referat(kind):
    """kind: 'starter', 'good', 'deleted' (keywords, nothing in the notes)
    or 'script' (the whole sentences in the notes, a text to read out)"""
    deck = Deck()
    deck.slide('title', 'Kraniche', subtitle='Ein Referat der Klasse 7b')
    if kind == 'starter':
        for name, sentences in SENTENCES.items():
            deck.slide('content', name, sentences).format_body(size=18)
    else:
        for title, keywords, sentences, cues in GOOD:
            notes = {'good': '\n'.join(cues), 'script': ' '.join(sentences)}.get(kind)
            deck.slide('content', title, keywords, notes=notes)
    deck.slide('title_only', 'Danke für die Aufmerksamkeit!')
    return deck


if __name__ == '__main__':
    os.makedirs(STATIC, exist_ok=True)
    referat('starter').save(os.path.join(STATIC, STARTER))
    referat('good').save(os.path.join(HERE, 'stufen', 'folien'))
    referat('deleted').save(os.path.join(HERE, 'stufen', 'geloescht'))
    referat('script').save(os.path.join(HERE, 'stufen', 'abgelesen'))
    # the whole Referat as the slide sorter shows it, slide by slide
    slide_pictures(os.path.join(STATIC, STARTER + '.pptx'), {n: os.path.join(HERE, 'bilder', f'vorlage-{n}.svg') for n in range(1, 6)})
    slide_pictures(os.path.join(HERE, 'stufen', 'folien.pptx'), {3: os.path.join(HERE, 'bilder', 'zug.svg')})
    print('written: static/good-slides/01 Kraniche.pptx/.odp, stufen/folien.*, stufen/geloescht.*, bilder/*.svg')
