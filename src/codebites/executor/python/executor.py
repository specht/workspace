import json
import os
import sys
import threading
import traceback
import importlib.util
from types import ModuleType
from typing import Any, Dict, Optional, Tuple

# ---- Protocol output: duplicate ORIGINAL stdout fd before any redirection ----
_PROTO_FD = os.dup(1)
_PROTO_OUT = os.fdopen(_PROTO_FD, "w", buffering=1, encoding="utf-8", errors="replace")

def emit(obj: Dict[str, Any]) -> None:
    _PROTO_OUT.write(json.dumps(obj, ensure_ascii=False) + "\n")
    _PROTO_OUT.flush()

def safe_json_value(x: Any) -> Any:
    try:
        json.dumps(x)
        return x
    except Exception:
        return {"__repr__": repr(x)}

def extract_location_from_tb(tb_text: str) -> Optional[Dict[str, Any]]:
    # Try to find a /workspace/submission.py line reference
    for line in tb_text.splitlines()[::-1]:
        if "/workspace/submission.py" in line and "line" in line:
            # e.g. File "/workspace/submission.py", line 12, in two_sum
            try:
                parts = line.split(",")
                file_part = parts[0].strip()
                line_part = parts[1].strip()
                filename = file_part.split('"')[1]
                lineno = int(line_part.split()[1])
                return {"file": os.path.basename(filename), "line": lineno}
            except Exception:
                return None
    return None

def start_reader(fd: int, stream: str, index: int) -> threading.Thread:
    def run():
        try:
            while True:
                chunk = os.read(fd, 4096)
                if not chunk:
                    break
                emit({
                    "event": "output",
                    "stream": stream,
                    "text": chunk.decode("utf-8", "replace"),
                    "index": index
                })
        finally:
            try:
                os.close(fd)
            except Exception:
                pass

    t = threading.Thread(target=run, daemon=True)
    t.start()
    return t

class LiveCapture:
    """
    Truly-live capture of stdout/stderr from user code by redirecting fd 1/2 to pipes.
    Protocol messages are written to _PROTO_OUT (a dup of original stdout), so they are NOT captured.
    """
    def __init__(self, index: int):
        self.index = index
        self.saved_out = None
        self.saved_err = None
        self.t_out = None
        self.t_err = None
        self.out_r = None
        self.err_r = None
        self.out_w = None
        self.err_w = None

    def __enter__(self):
        self.out_r, self.out_w = os.pipe()
        self.err_r, self.err_w = os.pipe()

        # Save current fds
        self.saved_out = os.dup(1)
        self.saved_err = os.dup(2)

        # Redirect stdout/stderr to writers
        os.dup2(self.out_w, 1)
        os.dup2(self.err_w, 2)

        # Close writers in this process; fds 1/2 now refer to them
        os.close(self.out_w)
        os.close(self.err_w)

        # Start readers on the read ends
        self.t_out = start_reader(self.out_r, "stdout", self.index)
        self.t_err = start_reader(self.err_r, "stderr", self.index)

        return self

    def __exit__(self, exc_type, exc, tb):
        # Restore stdout/stderr fds
        if self.saved_out is not None:
            os.dup2(self.saved_out, 1)
            os.close(self.saved_out)
        if self.saved_err is not None:
            os.dup2(self.saved_err, 2)
            os.close(self.saved_err)

        # Wait for readers to drain/exit
        if self.t_out:
            self.t_out.join(timeout=0.2)
        if self.t_err:
            self.t_err.join(timeout=0.2)

def load_submission(code: str) -> ModuleType:
    path = "/workspace/submission.py"
    with open(path, "w", encoding="utf-8") as f:
        f.write(code)

    spec = importlib.util.spec_from_file_location("submission", path)
    if spec is None or spec.loader is None:
        raise RuntimeError("Could not load submission module")

    mod = importlib.util.module_from_spec(spec)
    # Capture any top-level output during import so it doesn't break the JSON protocol.
    with LiveCapture(-1):
        spec.loader.exec_module(mod)  # type: ignore
    return mod

def handle_fatal(e: BaseException) -> None:
    tb = traceback.format_exc()
    emit({
        "event": "fatal",
        "error": {
            "type": e.__class__.__name__,
            "message": str(e),
            "location": extract_location_from_tb(tb)
        }
    })

def main() -> None:
    submission_mod: Optional[ModuleType] = None
    fn_name: Optional[str] = None

    for line in sys.stdin:
        line = line.strip()
        if not line:
            continue

        try:
            msg = json.loads(line)
        except json.JSONDecodeError:
            continue

        cmd = msg.get("cmd")

        if cmd == "init":
            try:
                files = msg.get("files") or {}
                submission_code = files.get("submission")
                if submission_code is None:
                    raise RuntimeError("missing files.submission")

                entry = msg.get("entry")
                if isinstance(entry, dict):
                    fn_name = entry.get("name")
                else:
                    fn_name = entry

                if not fn_name:
                    raise RuntimeError("missing entry/name")

                submission_mod = load_submission(submission_code)
                emit({"event": "ready"})
            except BaseException as e:
                handle_fatal(e)

        elif cmd == "case":
            if submission_mod is None or fn_name is None:
                emit({"event": "fatal", "error": {"type": "NotInitialized", "message": "init not received", "location": None}})
                continue

            idx = msg.get("index")
            args = msg.get("args") or []

            try:
                fn = getattr(submission_mod, fn_name)
                with LiveCapture(int(idx) if idx is not None else -1):
                    result = fn(*args)

                emit({
                    "event": "case",
                    "index": idx,
                    "ok": True,
                    "result": safe_json_value(result),
                    "error": None
                })
            except BaseException as e:
                tb = traceback.format_exc()
                emit({
                    "event": "case",
                    "index": idx,
                    "ok": False,
                    "result": None,
                    "error": {
                        "type": e.__class__.__name__,
                        "message": str(e),
                        "location": extract_location_from_tb(tb)
                    }
                })

        elif cmd == "stop":
            break

if __name__ == "__main__":
    main()