"""Moves a drawing script into the drawings' Docker image (docker/drawings),
so that what it makes does not depend on the machine it runs on.

    import drawings_container; drawings_container.ensure()

At the top of a script (diagram_labels.py does it for every script that
imports it): outside the container, the script starts again inside it,
through src/scripts/drawings, with the same arguments, and this process ends
with its exit code. Inside, ensure() does nothing.
"""

import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
REPO = os.path.dirname(os.path.dirname(HERE))


def ensure():
    if os.environ.get('DRAWINGS_CONTAINER') == '1':
        return
    script = os.path.abspath(sys.argv[0])
    if not script.startswith(REPO + os.sep):
        sys.exit(f'{script}: a drawing script belongs in the repository, to run in docker/drawings')
    inside = '/repo' + script[len(REPO):]
    args = ['/repo' + a[len(REPO):] if a.startswith(REPO + os.sep) else a for a in sys.argv[1:]]
    wrapper = os.path.join(HERE, 'drawings')
    sys.stdout.flush()
    os.execv(wrapper, [wrapper, 'python3', inside] + args)
