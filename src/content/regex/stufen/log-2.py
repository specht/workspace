# Das Logfile des Webservers, gelesen mit regulären Ausdrücken
import re
from collections import Counter

with open('access.log') as f:
    lines = f.read().splitlines()
line = lines[3]

print(line)
print(re.search(r'HTTP', line))
print(re.search(r'" 404 ', line))
print(sum(1 for l in lines if re.search(r'" 404 ', l)))

m = re.search(r'"(GET|POST) (\S+) ', line)
print(m.group(0))
print(m.group(1))
print(m.group(2))


# Der Status einer Zeile als Zahl, oder None, wenn die Zeile keinen hat
def status(line):
    m = re.search(r'" (\d{3}) ', line)
    if m is None:
        return None
    return int(m.group(1))


print(Counter(status(l) for l in lines))
