from search import *

pages = crawl('/index.html', fetch_page)
print(f'{len(pages)} Seiten gefunden:', ', '.join(pages))
index = build_index(pages)

while True:
    query = input('Suche: ')
    if not query.strip():
        break
    results = search(index, query)
    if not results:
        print('  Nichts gefunden.')
    for path in results:
        print(' ', path)
