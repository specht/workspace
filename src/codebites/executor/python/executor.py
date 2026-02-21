#!/usr/bin/env python3
import json
import sys
import traceback
import importlib.util
from types import ModuleType
from typing import Any, Dict, List, Optional, Tuple

SUB_FILE = "submission.py"

def extract_location(tb) -> Optional[Dict[str, Any]]:
    """
    Prefer the frame in submission.py; otherwise use the last frame.
    """
    frames = traceback.extract_tb(tb)
    if not frames:
        return None
    preferred = None
    for f in frames:
        if f.filename.endswith(SUB_FILE):
            preferred = f
            break
    f = preferred or frames[-1]
    return {"file": f.filename.split("/")[-1], "line": f.lineno}

def safe_json_value(x: Any) -> Any:
    """
    Ensure result is JSON serializable. If not, fall back to repr().
    """
    try:
        json.dumps(x)
        return x
    except Exception:
        return {"__repr__": repr(x)}

def load_submission() -> ModuleType:
    spec = importlib.util.spec_from_file_location("submission", f"/workspace/submission.py")
    if spec is None or spec.loader is None:
        raise RuntimeError("Could not create import spec for submission")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module

def main() -> None:
    req = json.loads(sys.stdin.read() or "{}")

    submission_code = (req.get("files") or {}).get("submission", "")
    entry = req.get("entry") or {}
    function_name = entry.get("name")
    cases = req.get("cases") or []

    resp: Dict[str, Any] = {"runs": [], "error": None}

    # Write student's code to a real file so tracebacks point to submission.py:<line>
    try:
        with open(f"/workspace/submission.py", "w", encoding="utf-8") as f:
            f.write(submission_code)
    except Exception as e:
        resp["error"] = {"type": e.__class__.__name__, "message": str(e), "location": None}
        print(json.dumps(resp))
        return

    # Import student module
    try:
        mod = load_submission()
    except SyntaxError as e:
        resp["error"] = {
            "type": "SyntaxError",
            "message": str(e),
            "location": {"file": SUB_FILE, "line": getattr(e, "lineno", None)},
        }
        print(json.dumps(resp))
        return
    except Exception as e:
        tb = sys.exc_info()[2]
        resp["error"] = {
            "type": e.__class__.__name__,
            "message": str(e),
            "location": extract_location(tb),
        }
        print(json.dumps(resp))
        return

    # Check function exists
    if not function_name or not hasattr(mod, function_name):
        resp["error"] = {
            "type": "MissingFunction",
            "message": f"Function {function_name} is not defined in {SUB_FILE}",
            "location": {"file": SUB_FILE, "line": 1},
        }
        print(json.dumps(resp))
        return

    fn = getattr(mod, function_name)

    # Execute cases
    for c in cases:
        args = c.get("args") or []
        try:
            result = fn(*args)
            resp["runs"].append({"ok": True, "result": safe_json_value(result)})
        except Exception as e:
            tb = sys.exc_info()[2]
            resp["runs"].append({
                "ok": False,
                "error": {
                    "type": e.__class__.__name__,
                    "message": str(e),
                    "location": extract_location(tb),
                },
            })

    print(json.dumps(resp))

if __name__ == "__main__":
    main()