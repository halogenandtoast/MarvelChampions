#!/usr/bin/env python3
"""Check bundled scheme A/B images for accidentally duplicated artwork."""
import json
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent


def main():
    cards = json.loads((ROOT / "frontend/public/data/cards.json").read_text())["cards"]
    images = ROOT / "frontend/public/images/cards"
    schemes = {
        c["code"].removesuffix("a").removesuffix("b"): c["name"]
        for c in cards
        if c["type"] in {"main_scheme", "side_scheme", "player_side_scheme"}
    }
    checked = 0
    duplicates = []
    for code, name in sorted(schemes.items()):
        front, back = (images / f"{code}{face}.avif" for face in "ab")
        if front.exists() and back.exists():
            checked += 1
            if front.read_bytes() == back.read_bytes():
                duplicates.append(f"{code}: {name}")
    print(f"Checked {checked} scheme image pairs; {len(duplicates)} identical pairs.")
    for duplicate in duplicates:
        print(duplicate)
    return bool(duplicates)


if __name__ == "__main__":
    raise SystemExit(main())
