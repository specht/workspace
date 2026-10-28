# Das Logfile des Webservers, gelesen mit regulären Ausdrücken
import re

with open('access.log') as f:
    lines = f.read().splitlines()
line = lines[3]

print(line)
print(re.search(r'HTTP', line))
print(re.search(r'" 404 ', line))
print(sum(1 for l in lines if re.search(r'" 404 ', l)))
