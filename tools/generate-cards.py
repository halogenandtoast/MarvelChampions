#!/usr/bin/env python3
"""Build the card database (frontend/public/data/cards.json) from MarvelCDB.

usage: tools/generate-cards.py [cards.json packs.json]

With no arguments the MarvelCDB public API is fetched. The other face of a
double-sided card (alter-ego, villain stages, main scheme B sides) only comes
nested under `linked_card`, so it is lifted into a card of its own here.
The server reads this file to resolve decks; the card browser reads it too.
"""
import json
import re
import sys
import urllib.request
from datetime import datetime, timezone
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
OUT = ROOT / "frontend" / "public" / "data" / "cards.json"
API = "https://marvelcdb.com/api/public"

LANDSCAPE = {"main_scheme", "side_scheme", "player_side_scheme"}
# Open identities occupy two card faces. Archangel reads across the spread;
# Ant-Man and Wasp turn the spread upright.
FOLD_OUT_ORIENTATIONS = {"12001c": "portrait", "13001c": "portrait", "42001c": "landscape"}

# MarvelCDB scans filed under the other face's name: its 07001b.png is Breakout's 1A side,
# and its 07001.png the 1B side. The HD export (frontend/public/images) has them right.
REMOTE_IMAGE_FIXES = {
    "07001a": "/bundles/cards/07001b.png",
    "07001b": "/bundles/cards/07001.png",
}


def fetch(url):
    req = urllib.request.Request(url, headers={"User-Agent": "marvel-champions-importer"})
    with urllib.request.urlopen(req) as r:
        return json.load(r)


def html_to_text(s):
    """MarvelCDB text is HTML with icon spans; keep <b>/<i>, turn icons into [name]."""
    if s is None:
        return None
    s = re.sub(r'<span class="icon-([a-z_]+)"[^>]*></span>', r"[\1]", s)
    s = re.sub(r"</?p>", "\n", s)
    s = re.sub(r"<br\s*/?>", "\n", s)
    return re.sub(r"\n{2,}", "\n", s).strip()


def traits(s):
    # Periods inside acronyms belong to the trait, rather than separating traits.
    protected = re.sub(r"(?:\b[A-Z]\.){2,}", lambda m: m.group().replace(".", "\0") + ".", s or "")
    return [t.strip().replace("\0", ".") for t in protected.split(".") if t.strip()]


def card(c, pack_order):
    # MarvelCDB mislabels High Ground; its printed card is a treachery.
    if c["code"] == "40154":
        c = {**c, "type_code": "treachery", "type_name": "Treachery"}
    # Printed A Good Workout adds damage to the same enemy.
    if c["code"] == "40040":
        c = {**c, "text": c.get("text", "").replace("1 additional damage to an enemy", "1 additional damage to that enemy")}
    # Verified against the existing printed Paladin's Pistol image.
    if c["code"] == "56118":
        c = {**c, "text": c.get("text", "").replace("Attack to Paladin.", "Attach to Paladin.")}
    if c["code"] == "56158":
        c = {**c, "text": c.get("text", "").replace("enmy team", "enemy team")}
    t = c["type_code"]
    orientation = FOLD_OUT_ORIENTATIONS.get(c["code"], "landscape" if t in LANDSCAPE else "portrait")
    out = {
        "code": c["code"],
        "name": c["name"],
        "subname": c.get("subname"),
        "packCode": c["pack_code"],
        "packName": c.get("pack_name"),
        "packOrder": pack_order.get(c["pack_code"]),
        "position": c.get("position"),
        "setCode": c.get("card_set_code"),
        "setName": c.get("card_set_name"),
        "setType": c.get("card_set_type_name_code"),
        "setPosition": c.get("set_position"),
        "type": t,
        "typeName": c.get("type_name"),
        "faction": c.get("faction_code"),
        "factionName": c.get("faction_name"),
        "cost": c.get("cost"),
        "resources": {
            k: c.get("resource_" + k)
            for k in ("energy", "physical", "mental", "wild")
            if c.get("resource_" + k)
        },
        # Daredevil's printed THW is 2; MarvelCDB currently lists 1.
        "thwart": 2 if c["code"] == "60001a" else c.get("thwart"),
        "thwartCost": c.get("thwart_cost"),
        "attack": c.get("attack"),
        "attackCost": c.get("attack_cost"),
        "defense": c.get("defense"),
        "recover": c.get("recover"),
        "scheme": c.get("scheme"),
        "health": c.get("health"),
        "healthPerHero": c.get("health_per_hero", False),
        "handSize": c.get("hand_size"),
        "boost": c.get("boost"),
        "boostStar": c.get("boost_star", False),
        "threat": c.get("threat"),
        "threatFixed": c.get("threat_fixed", False),
        "baseThreat": c.get("base_threat"),
        "baseThreatFixed": c.get("base_threat_fixed", False),
        "escalationThreat": c.get("escalation_threat"),
        "escalationThreatFixed": c.get("escalation_threat_fixed", False),
        "acceleration": c.get("scheme_acceleration"),
        "crisis": c.get("scheme_crisis"),
        "hazard": c.get("scheme_hazard"),
        "amplify": c.get("scheme_amplify"),
        "stage": c.get("stage"),
        "traits": traits(c.get("real_traits") or c.get("traits")),
        "text": html_to_text(c.get("text")),
        "flavor": html_to_text(c.get("flavor")),
        "backName": c.get("back_name"),
        "backText": html_to_text(c.get("back_text")),
        "backFlavor": html_to_text(c.get("back_flavor")),
        "quantity": c.get("quantity", 1),
        "deckLimit": c.get("deck_limit"),
        "unique": c.get("is_unique", False),
        "permanent": c.get("permanent", False),
        "hidden": c.get("hidden", False),
        "doubleSided": c.get("double_sided", False),
        "linkedTo": c.get("linked_to_code") or c.get("linked_code"),
        "duplicateOf": c.get("duplicate_of_code"),
        "illustrator": c.get("illustrator"),
        "image": c["code"] + ".avif",
        "remoteImage": REMOTE_IMAGE_FIXES.get(c["code"], c.get("imagesrc")),
        "remoteBackImage": c.get("backimagesrc"),
        "orientation": orientation,
        "aspectRatio": (126 / 88 if orientation == "landscape" else 88 / 126) if c["code"] in FOLD_OUT_ORIENTATIONS else None,
        "deckRequirements": c.get("deck_requirements"),
        "deckOptions": c.get("deck_options"),
    }
    # Rules Reference v1.8, p.70: Synthezoid Smackdown errata.
    if c["code"] == "57076":
        out["text"] = out["text"].replace("Hero Interrupt", "Hero Action")
    if c["code"] == "57065" and "Limit once per phase" not in out["text"]:
        out["text"] += " (Limit once per phase.)"
    return {k: v for k, v in out.items() if v not in (None, {}, [])} | {"traits": out["traits"]}


def main(argv):
    if len(argv) == 3:
        raw = json.loads(Path(argv[1]).read_text())
        packs = json.loads(Path(argv[2]).read_text())
    else:
        packs = fetch(API + "/packs/")
        raw = fetch(API + "/cards/?encounter=1")
    pack_order = {p["code"]: p["position"] for p in packs}
    cards = {}
    for c in raw:
        cards.setdefault(c["code"], card(c, pack_order))
        linked = c.get("linked_card")
        if linked:
            lc = card(linked, pack_order)
            lc["linkedTo"] = c["code"]
            cards.setdefault(linked["code"], lc)
    out = {
        "version": 1,
        "generatedAt": datetime.now(timezone.utc).isoformat(timespec="seconds"),
        "packs": [
            {"code": p["code"], "name": p["name"], "order": p["position"], "released": p.get("available"), "cardCount": p.get("known")}
            for p in sorted(packs, key=lambda p: p["position"])
        ],
        "cards": sorted(cards.values(), key=lambda c: (c.get("packOrder") or 999, c.get("position") or 0, c["code"])),
    }
    # Locally imported packs remain available when the MarvelCDB index is rebuilt.
    for path in sorted((ROOT / "data" / "cards").glob("*.json")):
        custom = json.loads(path.read_text())
        existing = {c["code"] for c in out["cards"]}
        collisions = existing.intersection(c["code"] for c in custom["cards"])
        if collisions:
            raise ValueError(f"Duplicate card codes in {path}: {sorted(collisions)}")
        out["packs"].extend(custom["packs"])
        out["cards"].extend(custom["cards"])
    # Supplements may add encounter cards to an existing MarvelCDB release.
    # Count faces in the final index so its release filters include that content.
    for pack in out["packs"]:
        if pack["code"] in {"fne", "shadowland"}:
            pack["cardCount"] = sum(c["packCode"] == pack["code"] for c in out["cards"])
    OUT.write_text(json.dumps(out, ensure_ascii=False, separators=(",", ":")))
    print(f"wrote {len(out['cards'])} cards in {len(out['packs'])} packs to {OUT}")


if __name__ == "__main__":
    main(sys.argv)
