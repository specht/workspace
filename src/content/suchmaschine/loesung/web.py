import sys

from webcrawler import *

start = sys.argv[1] if len(sys.argv) > 1 else 'http://localhost:8000/index.html'
pages = polite_crawl(start)
print(f'{len(pages)} Seiten geholt')
