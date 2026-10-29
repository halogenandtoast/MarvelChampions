#!/usr/bin/env python3
"""Print a MarvelCDB pack's cards from frontend/public/data/cards.json, for planning.

usage: pack.py <pack code | hero name>          every card, grouped by set
       pack.py <pack> <code> [<code> ...]       only those cards

A reprint shows `= <code>` and whether that code is already in the engine (grep of
src/Marvel); text is MarvelCDB's, icons as [name].
"""
import json
import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[3]
data = json.loads((ROOT / "frontend/public/data/cards.json").read_text())
want = sys.argv[1].lower()
pack = next(
    (p for p in data["packs"] if want in (p["code"].lower(), p["name"].lower())), None
)
if pack is None:
    sys.exit("no pack " + want + "; packs: " + " ".join(p["code"] for p in data["packs"]))
only = set(sys.argv[2:])
cards = [c for c in data["cards"] if c["packCode"] == pack["code"] and (not only or c["code"] in only)]


def implemented(code):
    return subprocess.run(["grep", "-rqF", '"' + code + '"', str(ROOT / "src/Marvel")]).returncode == 0


print(f"# {pack['name']} ({pack['code']}), {len(cards)} cards")
for group in sorted({(c.get("setCode") or c.get("faction") or "") for c in cards}):
    print(f"\n## {group}")
    for c in cards:
        if (c.get("setCode") or c.get("faction") or "") != group:
            continue
        stats = " ".join(
            f"{k}={c[k]}"
            for k in ("cost", "thwart", "attack", "defense", "recover", "handSize", "health",
                      "scheme", "boost", "threat", "baseThreat", "escalationThreat", "quantity")
            if c.get(k) is not None
        )
        res = " ".join(f"{k}:{v}" for k, v in (c.get("resources") or {}).items())
        dup = c.get("duplicateOf")
        dup = f" = {dup} ({'in engine' if implemented(dup) else 'NOT in engine'})" if dup else ""
        print(f"\n{c['code']} {c['name']}{' — ' + c['subname'] if c.get('subname') else ''}"
              f" [{c['type']}{', unique' if c.get('unique') else ''}]{dup}")
        print(f"  {stats} {res} traits: {', '.join(c.get('traits') or [])}".rstrip())
        if c.get("text"):
            print("  " + c["text"].replace("\n", "\n  "))
