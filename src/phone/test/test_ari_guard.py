"""The runner never makes a call, records or listens in (runner/ari_guard.py).
    python3 src/phone/test/test_ari_guard.py"""
import os
import sys
import unittest

sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), 'runner'))
from ari_guard import allowed  # noqa: E402


class Guard(unittest.TestCase):
    def test_what_a_call_needs(self):
        for method, path in [('POST', '/channels/123.4/answer'), ('POST', '/channels/123.4/mute'),
                             ('POST', '/channels/externalMedia'), ('POST', '/bridges'),
                             ('POST', '/bridges/b1/addChannel'), ('DELETE', '/channels/123.4'),
                             ('DELETE', '/bridges/b1')]:
            self.assertTrue(allowed(method, path), path)

    def test_never_a_call_out(self):
        for method, path in [('POST', '/channels'), ('POST', '/channels/'), ('POST', '/channels/create'),
                             ('POST', '/channels/my-own-id'), ('POST', '/channels/123.4/dial'),
                             ('POST', '/channels/123.4/continue'), ('POST', '/channels/123.4/move'),
                             ('POST', '/channels/123.4/redirect'), ('POST', '/channels?endpoint=PJSIP/0301234@trunk')]:
            self.assertFalse(allowed(method, path), path)

    def test_never_recording_or_listening(self):
        for method, path in [('POST', '/channels/123.4/record'), ('POST', '/bridges/b1/record'),
                             ('GET', '/recordings/stored'), ('POST', '/channels/123.4/snoop')]:
            self.assertFalse(allowed(method, path), path)


if __name__ == '__main__':
    unittest.main()
