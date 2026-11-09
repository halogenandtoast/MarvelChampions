#!/usr/bin/env python3
"""Build the card errata (frontend/public/data/errata.json) from the Rules Reference.

usage: tools/generate-errata.py

Reads Appendix V of docs/rules/faq-errata.md: each `### <pack>` heading names a
pack, each `#### <name> (#26)` heading a card in it by its position, and the
paragraphs under it are the errata. Cards are found in frontend/public/data/cards.json
by pack and position. Rulebook errata (`#### Rulebook Pg. …`) have no card and are
skipped. A card whose number names another card (Radiation Exposure is printed #153,
not #171A) is found by its name in the pack; one found neither way is an error.
"""
import json
import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
RULES = ROOT / "docs" / "rules" / "faq-errata.md"
CARDS = ROOT / "frontend" / "public" / "data" / "cards.json"
OUT = ROOT / "frontend" / "public" / "data" / "errata.json"

PAGE = re.compile(r"<!-- RR v1\.8 p\.(\d+) -->")
CARD_HEADING = re.compile(r"^(.*?) \((#[^)]*)\)$")
NUMBER = re.compile(r"(\d+)([AB])?")


def normalize(pack: str) -> str:
    """Pack names as Appendix V writes them and as MarvelCDB does, made comparable."""
    name = pack.replace("’", "'").lower()
    name = re.sub(r" (hero pack|expansion|scenario pack)$", "", name)
    name = re.sub(r"^the ", "", name)
    return re.sub(r"[^a-z0-9]", "", name)


def positions(numbers: str) -> list[tuple[int, str]]:
    """`#26`, `#1A`, `#57, #147`, `#73-74` as (position, side)."""
    found = []
    for part in numbers.replace("#", "").split(","):
        part = part.strip()
        if "-" in part:
            low, high = part.split("-")
            found += [(n, "") for n in range(int(low), int(high) + 1)]
        else:
            match = NUMBER.fullmatch(part)
            if not match:
                raise SystemExit(f"errata: cannot read card number {part!r}")
            found.append((int(match[1]), (match[2] or "").lower()))
    return found


def comparable(name: str) -> str:
    return re.sub(r"[^a-z0-9]", "", name.lower())


def to_card_text(markdown: str) -> str:
    """The light HTML of MarvelCDB card text, which the client's card text parser reads."""
    text = re.sub(r"\*\*(.+?)\*\*", r"<b>\1</b>", markdown)
    return re.sub(r"\*(.+?)\*", r"<i>\1</i>", text, flags=re.S)


def main() -> None:
    index = json.loads(CARDS.read_text())
    packs = {normalize(p["name"]): p["code"] for p in index["packs"]}
    by_position: dict[tuple[str, int], list[str]] = {}
    by_name: dict[tuple[str, str], list[str]] = {}
    names = {}
    for card in index["cards"]:
        names[card["code"]] = {comparable(card["name"]), comparable(card.get("backName") or "")}
        by_name.setdefault((card["packCode"], comparable(card["name"])), []).append(card["code"])
        if card.get("position") is not None:
            by_position.setdefault((card["packCode"], card["position"]), []).append(card["code"])

    section = RULES.read_text().split("## Appendix V: Errata", 1)[1]
    entries: list[tuple[str, str, int, list[str]]] = []
    pack = None
    page = None
    for line in section.splitlines():
        if (match := PAGE.fullmatch(line.strip())):
            page = int(match[1])
        elif line.startswith("## "):
            break
        elif line.startswith("### "):
            title = line[4:].strip()
            pack = packs.get(normalize(title))
            if pack is None:
                raise SystemExit(f"errata: no pack named {title!r}")
        elif line.startswith("#### "):
            entries.append((pack, line[5:].strip(), page, []))
        elif entries and not line.startswith("<!--"):
            entries[-1][3].append(line)

    errata = {}
    for pack, heading, page, body in entries:
        if heading.startswith("Rulebook"):
            continue
        match = CARD_HEADING.fullmatch(heading)
        if not match:
            raise SystemExit(f"errata: cannot read heading {heading!r}")
        text = to_card_text("\n".join(line for line in body if line.strip()))
        for position, side in positions(match[2]):
            codes = by_position.get((pack, position), [])
            if side:
                codes = [c for c in codes if c.endswith(side)]
            else:
                # a card printed on both sides with one errata: its unlettered record, else each face
                plain = [c for c in codes if not c[-1].isalpha()]
                codes = plain or codes
            if not any(comparable(match[1]) in names[c] for c in codes):
                # Appendix V titles some cards as since retitled (Yellow Jacket, M.O.D.O.K.)
                codes = by_name.get((pack, comparable(match[1])), codes)
            if not codes:
                raise SystemExit(f"errata: no card at {pack} #{position}{side.upper()} for {heading!r}")
            for code in codes:
                errata[code] = {"name": match[1], "text": text, "page": page}

    OUT.write_text(json.dumps({"source": "Rules Reference v1.8", "cards": dict(sorted(errata.items()))}, ensure_ascii=False, indent=1) + "\n")
    print(f"wrote {len(errata)} errata to {OUT.relative_to(ROOT)}", file=sys.stderr)


if __name__ == "__main__":
    main()
