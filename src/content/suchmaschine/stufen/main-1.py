from search import *

pages = crawl('/index.html', fetch_page)
print(f'{len(pages)} Seiten gefunden:', ', '.join(pages))
