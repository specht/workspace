import re
import time
import urllib.error
import urllib.request
from urllib.parse import urldefrag, urljoin, urlparse

# Wer der Crawler ist; viele Websites schreiben das in ihr Protokoll
USER_AGENT = 'Hackschule-Crawler (Informatikkurs, Hackschule Workspace)'
# Höchstens so viele Seiten, und so viele Sekunden Pause vor jeder
MAX_PAGES = 30
PAUSE = 1


def href_targets(html):
    """Die Ziele aller Links einer Seite, so wie sie im HTML stehen"""
    return re.findall(r'<a\s[^>]*?href\s*=\s*["\']([^"\']*)["\']', html, re.IGNORECASE)


def absolute_links(html, base):
    """Die Links einer Seite als ganze Adressen, nur auf ihrer eigenen Website,
    ohne #…, jede nur einmal; base ist die Adresse der Seite"""
    result = []
    for target in href_targets(html):
        url = urldefrag(urljoin(base, target))[0]
        parts = urlparse(url)
        if parts.scheme not in ('http', 'https') or parts.netloc != urlparse(base).netloc:
            continue
        if url not in result:
            result.append(url)
    return result


def robots_rules(text):
    """Was robots.txt allen Crawlern verbietet (User-agent: *), als Liste von
    Anfängen von Pfaden; vereinfacht, RFC 9309 kennt noch mehr"""
    rules = []
    applies = False
    for line in text.splitlines():
        line = line.split('#', 1)[0]
        if ':' not in line:
            continue
        key, value = (part.strip() for part in line.split(':', 1))
        if key.lower() == 'user-agent':
            applies = value == '*'
        elif key.lower() == 'disallow' and applies and value:
            rules.append(value)
    return rules


def allowed(rules, path):
    """Ob der Crawler den Pfad holen darf: Keine Regel ist sein Anfang"""
    return path not in rules


def get(url):
    """Eine Anfrage mit dem User-Agent des Crawlers"""
    request = urllib.request.Request(url, headers={'User-Agent': USER_AGENT})
    return urllib.request.urlopen(request, timeout=10)


def fetch_robots(start):
    """robots.txt der Website, oder nichts, wenn es keine gibt"""
    try:
        with get(urljoin(start, '/robots.txt')) as response:
            return response.read().decode('utf-8', errors='replace')
    except Exception:
        return ''


def fetch_url(url):
    """Eine HTML-Seite als (Adresse, HTML), oder None; urlopen folgt
    Weiterleitungen, die Seite muss aber auf derselben Website bleiben"""
    try:
        with get(url) as response:
            address = response.geturl()
            if urlparse(address).netloc != urlparse(url).netloc:
                return None
            if response.headers.get_content_type() != 'text/html':
                return None
            return address, response.read().decode('utf-8', errors='replace')
    except Exception:
        return None


def request_path(url):
    """Der Pfad einer Adresse mit dem Teil hinter ?, wie robots.txt ihn meint"""
    parts = urlparse(url)
    return (parts.path or '/') + ('?' + parts.query if parts.query else '')


def polite_crawl(start, max_pages=MAX_PAGES):
    """Höchstens max_pages Seiten der Website von start, mit Pause und nach
    robots.txt, als dict von Adresse zu HTML"""
    rules = robots_rules(fetch_robots(start))
    print('robots.txt verbietet:', ', '.join(rules) if rules else 'nichts')
    pages = {}
    queue = [start]
    seen = [start]
    while queue and len(pages) < max_pages:
        url = queue.pop(0)
        if not allowed(rules, request_path(url)):
            print('übersprungen:', url)
            continue
        time.sleep(PAUSE)
        found = fetch_url(url)
        if found is None:
            continue
        address, html = found
        print('geholt:', address)
        pages[address] = html
        for link in absolute_links(html, address):
            if link not in seen:
                seen.append(link)
                queue.append(link)
    return pages
