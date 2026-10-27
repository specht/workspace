import re
import urllib.error
import urllib.request

# Wo die Website läuft, die die Suchmaschine durchsucht
SITE = 'http://localhost:8000'


def fetch_page(path):
    """Eine Seite der Website als String, oder None, wenn es sie nicht gibt"""
    try:
        with urllib.request.urlopen(SITE + path) as response:
            return response.read().decode()
    except urllib.error.HTTPError:
        return None


def links(html):
    """Die Ziele der Links einer Seite, die auf derselben Website bleiben"""
    return re.findall(r'<a href="(/[^"#]*)"', html)


def words(html):
    """Die Wörter einer Seite, klein geschrieben, ohne die Tags"""
    return re.findall(r'[^\W\d_]+', re.sub(r'<[^>]*>', ' ', html).lower())


def crawl(start, fetch):
    """Alle Seiten, die von start aus über Links erreichbar sind, als dict von
    Pfad zu HTML; fetch holt eine Seite"""
    pages = {}
    queue = [start]
    seen = [start]
    while queue:
        path = queue.pop(0)
        html = fetch(path)
        if html is None:
            continue
        pages[path] = html
        for link in links(html):
            if link not in seen:
                seen.append(link)
                queue.append(link)
    return pages


def build_index(pages):
    """Für jedes Wort die Seiten, auf denen es steht"""
    index = {}
    for path, html in pages.items():
        for word in words(html):
            if word not in index:
                index[word] = []
            index[word].append(path)
    return index


def search(index, query):
    """Die Seiten, auf denen jedes Wort der Anfrage steht, nach dem Alphabet"""
    results = None
    for word in words(query):
        found = index.get(word, [])
        results = found if results is None else [path for path in results if path in found]
    return sorted(results or [])


def rank_by_count(results, pages, word):
    """Die Treffer, die Seite mit dem Wort am häufigsten zuerst"""
    return sorted(results, key=lambda path: (-words(pages[path]).count(word), path))


def incoming(pages):
    """Für jede Seite, wie viele andere Seiten auf sie verlinken"""
    counts = {}
    for path, html in pages.items():
        for link in dict.fromkeys(links(html)):
            if link != path and link in pages:
                counts[link] = counts.get(link, 0) + 1
    return counts


def rank_by_links(results, counts):
    """Die Treffer, die Seite mit den meisten eingehenden Links zuerst"""
    return sorted(results, key=lambda path: (-counts.get(path, 0), path))
