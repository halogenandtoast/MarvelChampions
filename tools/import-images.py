#!/usr/bin/env python3
"""Import HD card scans (Tabletop Simulator export) as web AVIFs keyed by marvelcdb code.

Usage:
  python3 -m venv /tmp/mc-venv && /tmp/mc-venv/bin/pip install Pillow imagehash numpy
  curl -o cards.json 'https://marvelcdb.com/api/public/cards/?encounter=1'
  /tmp/mc-venv/bin/python -I tools/import-images.py MarvelHD.zip cards.json \
      [--ref-dir images/mcdb-ref] [--threshold 110] [--strict 60] [--jobs 20] [--force]

Pipeline (every stage is cached, so reruns are cheap):
  1. extract ZIP to images/hd/ (skipped if images/hd/MarvelHD exists)
  2. download marvelcdb reference images to --ref-dir (polite: 4 at a time)
  3. perceptual-hash every crop (MarvelHD/cropped/*) and single-card Face/Back file,
     at 4 rotations, and match each to the nearest marvelcdb reference
     (best candidate per code wins, distance must be <= --threshold)
  4. write images/manifest.json (code -> source, rotation, distance)
  5. encode frontend/public/images/cards/<code>.avif (longest side 840, q55),
     plus back-player/back-encounter/back-villain.avif
Requires Pillow built with AVIF (Pillow >= 11.3 wheels).
"""
import argparse, hashlib, json, os, re, subprocess, sys, zipfile
from multiprocessing import Pool
from pathlib import Path

import imagehash
import numpy as np
from PIL import Image

Image.MAX_IMAGE_PIXELS = None
ROOT = Path(__file__).resolve().parent.parent
HD = ROOT / "images" / "hd"
MANIFEST = ROOT / "images" / "manifest.json"
OUT = ROOT / "frontend" / "public" / "images" / "cards"
MCDB = "https://marvelcdb.com"
HASH = 16  # phash bits = HASH**2
MAX_SIDE = 840
QUALITY = 55
# ponytail: backs picked by hand from the TTS export; re-check if the export changes
BACKS = {
    "back-player": "MarvelHD/007_Hawkeye_s_Hero_Deck_BackURL.png",
    "back-encounter": "MarvelHD/003_Hawkeye_s_Nemesis_Set_BackURL.png",
    "back-villain": "MarvelHD/cropped/347_Rhino_Villain_FaceURL_20.png",  # hidden slot of a villain sheet
}
# ponytail: codes matching can't find, picked by hand: [source, rotation]. Mostly cards
# marvelcdb has no scan of (Ironheart's Version 2 and 3, the NeXt Evolution, Age of
# Apocalypse, Agents of S.H.I.E.L.D., Trickster Takeover and Civil War scenario cards), and
# a few whose scan is the wrong card (marvelcdb's 12001c.png is Tiny Ant-Man, not Giant)
OVERRIDES = json.loads((ROOT / "tools" / "image-overrides.json").read_text())
# ponytail: side schemes whose marvelcdb reference is a portrait scan, so the match
# comes out upright; their rotation, picked by hand
ROTATIONS = {"05026": 90, "11045": 90}


def walk(card):
    yield card
    if card.get("linked_card"):
        yield from walk(card["linked_card"])


def explicit_main_scheme_faces(card):
    return (
        card["type_code"] == "main_scheme"
        and Path(card.get("imagesrc", "")).stem == card["code"] + "a"
        and bool(card.get("backimagesrc"))
    )


def load_cards(path):
    cards = {}
    for c in json.load(open(path)):
        for x in walk(c):
            cards.setdefault(x["code"], x)
    # url -> codes that use that image
    refs = {}
    for code, c in cards.items():
        # Newer scans name the printed A/B faces explicitly. The bare engine
        # code still denotes the playable B face, not imagesrc's setup face.
        if explicit_main_scheme_faces(c):
            refs.setdefault(c["imagesrc"], set()).add(code + "a")
            refs.setdefault(c["backimagesrc"], set()).update([code, code + "b"])
            continue
        if c.get("imagesrc"):
            refs.setdefault(c["imagesrc"], set()).add(code)
        if c.get("backimagesrc"):
            refs.setdefault(c["backimagesrc"], set()).add(Path(c["backimagesrc"]).stem)
    # codes with no image of their own: reprints and 'a' sides of schemes
    have = {code: url for url, codes in refs.items() for code in codes}
    aliases = {}
    for code, c in cards.items():
        if code in have:
            continue
        src = c.get("duplicate_of_code")
        if src in have:
            aliases[code] = src
        elif code.endswith("a") and code[:-1] in have:
            # main schemes: X.png is the B side, X's backimagesrc is the A side (X's back_text == Xa's text)
            base = cards.get(code[:-1], {})
            aliases[code] = Path(base["backimagesrc"]).stem if base.get("backimagesrc") else code[:-1]
    return cards, refs, aliases


def extract(zip_path):
    if (HD / "MarvelHD").exists():
        return
    with zipfile.ZipFile(zip_path) as z:
        for n in z.namelist():
            if n.startswith("__MACOSX") or n.endswith(".DS_Store"):
                continue
            z.extract(n, HD)


def download_refs(refs, ref_dir):
    # ponytail: curl, not urllib -- marvelcdb stalls fresh python TLS connections; curl reuses them
    ref_dir.mkdir(parents=True, exist_ok=True)
    todo = [u for u in refs if not (ref_dir / Path(u).name).exists()]
    if not todo:
        return
    cfg = ref_dir / "curl.cfg"
    cfg.write_text("".join(f'url = "{MCDB}{u}"\noutput = "{ref_dir / Path(u).name}"\n' for u in todo))
    # --fail: 404s (a few cards) write nothing
    subprocess.run(["curl", "-s", "--fail", "--retry", "3", "--parallel", "--parallel-max", "4",
                    "-A", "marvel-champions-image-import", "-K", str(cfg)])
    cfg.unlink()


def phash(img):
    return imagehash.phash(img.convert("L"), hash_size=HASH).hash.flatten()


ROTS = (0, 90, 180, 270)  # PIL counter-clockwise degrees
THUMB = (96, 134)  # small grayscale copy for the fine re-rank (portrait; transposed for landscape)


def hash_file(path):
    """-> (path, w, h, phash bits [4, HASH*HASH], thumbs [4, 96*134]) for each rotation, or None."""
    try:
        img = Image.open(path)
        img.draft("RGB", (512, 512))
        w, h = img.size
        img = img.convert("L")
        img.thumbnail((512, 512))
        hs, ts = [], []
        for r in ROTS:
            im = img.rotate(r, expand=True)
            hs.append(phash(im))
            ts.append(np.asarray(im.resize(THUMB if im.width < im.height else THUMB[::-1], Image.BILINEAR)).ravel())
        return str(path), w, h, np.array(hs), np.array(ts, dtype=np.uint8)
    except Exception as e:
        print("hash failed", path, e, file=sys.stderr)
        return None


def features(paths, cache_file, jobs):
    """{path: (w, h, hashes, thumbs)}, cached in an .npz keyed by path."""
    feats = {}
    if cache_file.exists():
        z = np.load(cache_file)
        feats = {p: (w, h, hs, ts) for p, (w, h), hs, ts in zip(z["paths"], z["size"], z["hashes"], z["thumbs"])}
    todo = [str(p) for p in paths if str(p) not in feats]
    if todo:
        with Pool(jobs) as pool:
            for i, r in enumerate(pool.imap_unordered(hash_file, todo, chunksize=4)):
                if r:
                    feats[r[0]] = r[1:]
                if i % 500 == 0:
                    print(f"  hashed {i}/{len(todo)}", flush=True)
        ks = list(feats)
        np.savez(cache_file, paths=np.array(ks), size=np.array([feats[k][:2] for k in ks]),
                 hashes=np.array([feats[k][2] for k in ks]), thumbs=np.array([feats[k][3] for k in ks]))
    return {str(p): feats[str(p)] for p in paths if str(p) in feats}


def is_card_shaped(w, h):
    r = min(w, h) / max(w, h)
    return 0.65 < r < 0.78


def candidates():
    root = HD / "MarvelHD"
    out = sorted((root / "cropped").glob("*.*"))
    for p in sorted(root.glob("*URL.*")):
        if "FaceURL" in p.name or "BackURL" in p.name:
            with Image.open(p) as im:
                if is_card_shaped(*im.size):
                    out.append(p)
    return [p for p in out if p.suffix.lower() in (".png", ".jpg", ".jpeg")]


def identical_groups(ref_paths):
    by = {}
    for u, p in ref_paths.items():
        by.setdefault(hashlib.md5(p.read_bytes()).hexdigest(), []).append(u)
    return [g for g in by.values() if len(g) > 1]


def znorm(t):
    t = t.astype(np.float32)
    return (t - t.mean(-1, keepdims=True)) / (t.std(-1, keepdims=True) + 1e-6)


def match(cand, reff, threshold, groups=(), strict=60, shortlist=12):
    """{ref url: (candidate path, rotation, phash distance, pixel score)}.

    phash shortlists candidates; a normalized pixel diff of small grayscale thumbs picks among
    them (phash alone can't tell Rhino I from Rhino III). Each ref takes its best candidate, so a
    candidate may serve several refs (reprints share art).
    groups: lists of urls whose marvelcdb images are byte-identical (e.g. 01043a-d, variants that
    differ only in resource icon) -- see split_variants.
    Distances in (strict, threshold] are kept only if the crop isn't a closer match for another ref.
    """
    urls = list(reff)
    R = np.array([reff[u][2][0] for u in urls], dtype=np.float32)  # refs at rotation 0
    RT = znorm(np.array([reff[u][3][0] for u in urls]))
    r_land = np.array([reff[u][0] > reff[u][1] for u in urls])
    keys = [(p, i) for p in cand for i in range(4)]
    C = np.array([cand[p][2][i] for p, i in keys], dtype=np.float32)
    CT = np.array([cand[p][3][i] for p, i in keys])
    c_land = np.array([(cand[p][0] > cand[p][1]) != (ROTS[i] in (90, 270)) for p, i in keys])
    bits = R.shape[1]
    ranked = {}  # url -> [(pixel score, phash dist, path, rot)] best first
    for s in range(0, len(urls), 256):
        r = R[s:s + 256]
        d = bits - (r @ C.T + (1 - r) @ (1 - C).T)  # hamming distance
        d[r_land[s:s + 256, None] != c_land[None, :]] = 10**6  # orientation must agree
        for j, row in enumerate(d):
            top = np.argpartition(row, shortlist)[:shortlist]
            top = top[row[top] <= threshold]
            px = np.abs(znorm(CT[top]) - RT[s + j]).mean(1)
            ranked[urls[s + j]] = sorted((round(float(x), 3), int(row[k]), *keys[k]) for x, k in zip(px, top))
    best = {u: (p, ROTS[i], d, x) for u, rs in ranked.items() if rs for x, d, p, i in rs[:1]}
    for g in groups:
        split_variants(sorted(g), ranked, best)
    # loose matches (watermarked previews etc.) only count if no closer ref claims the same crop
    claim = {}
    for u, (p, _, d, _) in best.items():
        claim[p] = min(claim.get(p, 999), d)
    return {u: b for u, b in best.items() if b[2] <= strict or b[2] <= claim[b[0]]}


def split_variants(urls, ranked, best, margin=0.1):
    """Identical refs for N variants (01043a-d differ only in resource icon): take the N closest
    crops from the best crop's sheet and assign them in sheet order. A bare base code (01144 next
    to 01144a-c) gets the 'a' crop.

    ponytail: relies on TTS sheets listing variants in collector order (43A, 43B, ...); true for core.
    """
    pool = {}
    for u in urls:
        for x, d, p, i in ranked.get(u, []):
            pool.setdefault(p, (x, d, p, i))
    if not pool:
        return
    sheet = lambda p: re.match(r"(.*)_(\d+)\.\w+$", Path(p).name)
    top = min(pool.values())
    if not sheet(top[2]):
        return
    same = sorted(v for v in pool.values() if v[0] <= top[0] + margin and sheet(v[2]) and sheet(v[2])[1] == sheet(top[2])[1])
    named = [u for u in urls if re.search(r"[a-z]\.\w+$", u)]
    if len(same) == len(urls):
        targets = [[u] for u in urls]
    elif len(same) == len(named) == len(urls) - 1:
        base = next(u for u in urls if u not in named)
        targets = [[u] + ([base] if k == 0 else []) for k, u in enumerate(named)]
    else:
        return
    same.sort(key=lambda v: int(sheet(v[2])[2]))
    for us, (x, d, p, i) in zip(targets, same):
        for u in us:
            best[u] = (p, ROTS[i], d, x)


def encode(job):
    src, rot, dest = job
    img = Image.open(src)
    img.draft("RGB", (MAX_SIDE * 2, MAX_SIDE * 2))
    img = img.convert("RGB")
    if rot:
        img = img.rotate(rot, expand=True)
    img.thumbnail((MAX_SIDE, MAX_SIDE), Image.LANCZOS)
    img.save(dest, "AVIF", quality=QUALITY, speed=6)
    return dest


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("zip")
    ap.add_argument("cards")
    ap.add_argument("--ref-dir", default=str(ROOT / "images" / "mcdb-ref"))
    ap.add_argument("--threshold", type=int, default=110, help="max phash distance (of 256 bits)")
    ap.add_argument("--strict", type=int, default=60, help="distances above this need the crop to be uncontested")
    ap.add_argument("--jobs", type=int, default=os.cpu_count())
    ap.add_argument("--force", action="store_true", help="re-encode existing avifs")
    a = ap.parse_args()
    ref_dir = Path(a.ref_dir)

    cards, refs, aliases = load_cards(a.cards)
    print(f"{len(cards)} codes, {len(refs)} marvelcdb images, {len(aliases)} aliases")
    print("extracting"); extract(a.zip)
    print("downloading refs"); download_refs(refs, ref_dir)
    ref_paths = {u: ref_dir / Path(u).name for u in refs if (ref_dir / Path(u).name).exists()}
    print("hashing refs")
    rf = features(list(ref_paths.values()), ROOT / "images" / "ref-features.npz", a.jobs)
    reff = {u: rf[str(p)] for u, p in ref_paths.items() if str(p) in rf}
    print("hashing candidates")
    cand = features(candidates(), ROOT / "images" / "hd-features.npz", a.jobs)
    best = match(cand, reff, a.threshold, identical_groups(ref_paths), a.strict)

    manifest = {}
    for url, (path, rot, d, px) in best.items():
        for code in refs[url]:
            manifest[code] = {"source": os.path.relpath(path, HD), "rotate": rot, "distance": d, "pixel": px}
    for code, src in aliases.items():
        if src in manifest:
            manifest[code] = dict(manifest[src], alias_of=src)
    # Legacy main schemes: X.png is B and backimagesrc is A. Explicit A/B
    # filenames were already assigned their printed faces by load_cards.
    for code, c in cards.items():
        if c.get("backimagesrc") and c["type_code"] == "main_scheme" and not explicit_main_scheme_faces(c) and code + "a" in cards and code in manifest:
            manifest[code + "b"] = dict(manifest[code], alias_of=code)
    for code, (src, rot) in OVERRIDES.items():
        manifest[code] = {"source": src, "rotate": rot, "override": True}
    # reprints whose own marvelcdb scan didn't match take the original's image
    for code, c in cards.items():
        src = c.get("duplicate_of_code")
        if code not in manifest and src in manifest:
            manifest[code] = dict(manifest[src], alias_of=src)
    for code, rot in ROTATIONS.items():
        if code in manifest:
            manifest[code]["rotate"] = rot
    manifest = dict(sorted(manifest.items()))
    MANIFEST.write_text(json.dumps(manifest, indent=1))

    unmapped = sorted(c for c in cards if c not in manifest)
    core = [c for c in cards if cards[c]["pack_code"] == "core"]
    print(f"mapped {sum(c in manifest for c in cards)}/{len(cards)} codes; core {sum(c in manifest for c in core)}/{len(core)}")
    print("unmapped core:", [c for c in core if c not in manifest])
    (ROOT / "images" / "unmapped.txt").write_text("\n".join(
        f"{c}\t{cards[c]['pack_code']}\t{cards[c]['name']}\t{'no mcdb image' if not cards[c].get('imagesrc') else 'no match'}"
        for c in unmapped))

    OUT.mkdir(parents=True, exist_ok=True)
    jobs = [(HD / m["source"], m["rotate"], OUT / f"{c}.avif") for c, m in manifest.items()]
    jobs += [(HD / src, 0, OUT / f"{name}.avif") for name, src in BACKS.items() if src]
    jobs = [j for j in jobs if a.force or not j[2].exists()]
    print(f"encoding {len(jobs)}")
    with Pool(a.jobs) as pool:
        for i, _ in enumerate(pool.imap_unordered(encode, jobs)):
            if i % 500 == 0:
                print(f"  encoded {i}/{len(jobs)}", flush=True)


if __name__ == "__main__":
    main()
