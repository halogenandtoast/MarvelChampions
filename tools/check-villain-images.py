#!/usr/bin/env python3
"""Check every bundled villain face for missing or duplicated stage images.

This catches duplicate scans, not a unique scan assigned to the wrong stage;
printed stage numbers still need visual review when importing new artwork.
"""
import hashlib
import json
from collections import defaultdict
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent


def main():
    cards = json.loads((ROOT / "frontend/public/data/cards.json").read_text())["cards"]
    images = ROOT / "frontend/public/images/cards"
    hashes = defaultdict(list)
    problems = []
    checked = 0
    for card in cards:
        if card["type"] != "villain":
            continue
        checked += 1
        label = f"{card['code']}: {card['name']} ({card.get('stage', '?')})"
        image = images / card["image"]
        if not image.exists():
            problems.append(f"Missing {label}")
            continue
        hashes[hashlib.sha256(image.read_bytes()).hexdigest()].append(label)
    for labels in hashes.values():
        if len(labels) > 1:
            problems.append("Identical villain images: " + "; ".join(labels))
    print(f"Checked {checked} villain faces; {len(problems)} problems.")
    for problem in problems:
        print(problem)
    return bool(problems)


if __name__ == "__main__":
    raise SystemExit(main())
