"""conftest.py - ハイフン入りスクリプトをモジュールとして import できるよう登録する"""

import importlib.util
import sys
from pathlib import Path

_SCRIPTS_DIR = Path(__file__).parent

_HYPHEN_SCRIPTS = [
    ("scan_review", "scan-review.py"),
]

for _module_name, _filename in _HYPHEN_SCRIPTS:
    if _module_name not in sys.modules:
        _spec = importlib.util.spec_from_file_location(
            _module_name, _SCRIPTS_DIR / _filename
        )
        if _spec and _spec.loader:
            _mod = importlib.util.module_from_spec(_spec)
            sys.modules[_module_name] = _mod
            _spec.loader.exec_module(_mod)
