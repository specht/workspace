from search import *

pages = crawl('/index.html', fetch_page)
print(f'{len(pages)} Seiten gefunden:', ', '.join(pages))
index = build_index(pages)
counts = incoming(pages)

while True:
    query = input('Suche: ')
    if not query.strip():
        break
    results = rank_by_links(search(index, query), counts)
    if not results:
        print('  Nichts gefunden.')
    for path in results:
        print(' ', path)
