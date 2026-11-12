"""What the runner may ask Asterisk through ARI (docs/plan-telefonspiel.md):
the Telefonspiel only answers calls. It never makes one (no channel
originated, nothing dialled, no call sent back into the dialplan), never
records and never listens in (the maintainer, 12 November 2026). Asterisk
has the matching modules off as well (src/phone/asterisk/modules.conf).
    python3 src/phone/test/test_ari_guard.py"""
import re

# a channel of its own: originating a call, the one way ARI makes one
_ORIGINATE = re.compile(r'^/channels(/create|/[^/]+)?/?$')
# on a channel: dialling it out, sending it on in the dialplan; on any:
# recordings and snooping
_FORBIDDEN = re.compile(r'/(dial|continue|move|redirect|record|recordings|snoop)\b', re.I)


def allowed(method, path):
    """whether the runner may send this request"""
    path = path.split('?', 1)[0]
    if _FORBIDDEN.search(path):
        return False
    if method.upper() == 'POST' and _ORIGINATE.match(path) and not path.rstrip('/').endswith('/externalMedia'):
        return False
    return True
