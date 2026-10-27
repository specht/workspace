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
