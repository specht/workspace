import builtins

def _sum(iterable, start=0):
    raise RuntimeError("Use of sum() is disabled for this exercise.")

builtins.sum = _sum
