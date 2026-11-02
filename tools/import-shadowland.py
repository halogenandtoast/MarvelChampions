#!/usr/bin/env python3
"""Import the Shadowland Drive scans. Requires curl, ImageMagick and Tesseract.

python3 tools/import-shadowland.py --download

Source scans and OCR caches live in ignored images/shadowland/. Web scans use
the existing 840px / quality 55 AVIF convention. The checked-in JSON is the
reference for future engine work; OCR is explicitly unverified.
"""
import argparse
from concurrent.futures import ThreadPoolExecutor
import json
from pathlib import Path
import re
import subprocess
import tempfile
import unicodedata

ROOT = Path(__file__).resolve().parent.parent
SOURCES = ROOT / 'tools/shadowland-sources.json'
CACHE = ROOT / 'images/shadowland'
IMAGES = ROOT / 'frontend/public/images/cards'
REFERENCE = ROOT / 'data/cards/shadowland.json'
INDEX = ROOT / 'frontend/public/data/cards.json'


def cached_source(item):
    original = CACHE / (item['code'] + '.tif')
    if original.exists() and original.stat().st_size == item['originalBytes']:
        return original
    preview = CACHE / (item['code'] + '.png')
    return preview if preview.exists() else None


def run(*args):
    return subprocess.run(args, check=True, capture_output=True, text=True).stdout


def metadata(group, item):
    parts = Path(item['name']).stem.split('_')
    position = int(parts[0])
    name = parts[1]
    # Correct filename typos against the printed scans, retaining the originals.
    name = {'The Street Below': 'The Streets Below', 'Sealed Entrance': 'Sealed Entrances', 'fear Projection': 'Fear Projection',
            'Poisin Darts': 'Poison Darts'}.get(name, name)
    kind = next(p for p in parts[2:] if p)
    kind = {'Attachement': 'Attachment', 'Treacehry': 'Treachery'}.get(kind, kind)
    card_type = kind.lower().replace(' ', '_')
    set_name = re.sub(r'^\d+\. ', '', group['name'])
    set_name = set_name.replace('Dark Protege', 'Dark Protégé')
    ascii_name = unicodedata.normalize('NFKD', set_name).encode('ascii', 'ignore').decode()
    set_code = 'shadowland-' + re.sub(r'[^a-z0-9]+', '-', ascii_name.lower()).strip('-')
    return {
        'code': item['code'], 'name': name,
        'packCode': 'shadowland', 'packName': 'Shadowland', 'packOrder': 1000,
        'position': int(group['name'].split('.')[0]) * 100 + position,
        'setCode': set_code, 'setName': set_name,
        'setType': 'scenario' if int(group['name'].split('.')[0]) <= 3 else 'modular',
        'setPosition': position, 'type': card_type, 'typeName': kind.title(),
        'faction': 'encounter', 'factionName': 'Encounter', 'traits': [],
        'image': item['code'] + '.avif',
        'bundledImage': True,
        'sourceFile': item['name'],
        'sourceUrl': 'https://drive.google.com/file/d/' + item['id'] + '/view',
    }


def process(pair):
    group, item = pair
    code = item['code']
    original = cached_source(item)
    if original is None:
        raise FileNotFoundError(f'Missing source for {code}; run with --download')
    card = metadata(group, item)
    ocr_file = CACHE / (code + '.ocr.json')
    if not ocr_file.exists():
        with tempfile.TemporaryDirectory(prefix='shadowland-') as tmp:
            # Resolve /tmp to /private/tmp: Leptonica cannot open symlinked /tmp
            # paths on some macOS installations.
            png = Path(tmp).resolve() / 'card.png'
            run('magick', str(original), '-auto-orient', '-resize', '1680x1680>', '-depth', '8', str(png))
            rotation = 90 if card['type'] in {'main_scheme', 'side_scheme'} else 0
            if rotation:
                run('magick', str(png), '-rotate', str(rotation), str(png))
            raw = run('tesseract', str(png), 'stdout', '-l', 'eng', '--psm', '3')
            width, height = map(int, run('magick', 'identify', '-format', '%w %h', str(png)).split())
            ocr_file.write_text(json.dumps({'rotation': rotation, 'width': width, 'height': height,
                                            'rawText': raw.strip()}, ensure_ascii=False, indent=2) + '\n')
    ocr = json.loads(ocr_file.read_text())
    output = IMAGES / (code + '.avif')
    if not output.exists():
        args = ['magick', str(original), '-auto-orient']
        if ocr['rotation']:
            args += ['-rotate', str(ocr['rotation'])]
        run(*args, '-resize', '840x840>', '-strip', '-quality', '55', str(output))
    card['orientation'] = 'landscape' if ocr['width'] > ocr['height'] else 'portrait'
    card['scanSource'] = 'original TIFF' if original.suffix == '.tif' else 'Google Drive preview'
    card['scanWidth'] = ocr['width']
    card['scanHeight'] = ocr['height']
    card['ocr'] = {'engine': 'Tesseract 5 / eng / psm 3', 'verified': False, 'rawText': ocr['rawText']}
    # Retain the raw OCR alongside searchable viewer text. Never infer numeric
    # stats or icon meanings from OCR: the scan remains authoritative.
    card['text'] = '\n\n'.join(' '.join(paragraph.splitlines()) for paragraph in ocr['rawText'].split('\n\n'))
    print(code, card['name'], flush=True)
    return card


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--download', action='store_true', help='download missing scans as Drive previews (at least 1024px)')
    parser.add_argument('--originals', action='store_true', help='download original TIFFs instead (about 2 GB)')
    parser.add_argument('--jobs', type=int, default=3)
    args = parser.parse_args()
    source = json.loads(SOURCES.read_text())
    pairs = [(group, item) for group in source['sets'] for item in group['files']]
    CACHE.mkdir(parents=True, exist_ok=True)
    IMAGES.mkdir(parents=True, exist_ok=True)
    if args.download:
        def download(pair):
            item = pair[1]
            dest = CACHE / (item['code'] + ('.tif' if args.originals else '.png'))
            complete_original = dest.exists() and dest.stat().st_size == item['originalBytes']
            if (args.originals and not complete_original) or (not args.originals and cached_source(item) is None):
                partial = dest.with_suffix('.part')
                url = ('https://drive.google.com/uc?export=download&id=' + item['id'] if args.originals
                       else 'https://drive.google.com/thumbnail?id=' + item['id'] + '&sz=w2000')
                run('curl', '-fLsS', '--retry', '3', '--max-time', '180',
                    url, '-o', str(partial))
                run('magick', 'identify', str(partial))
                partial.replace(dest)
        with ThreadPoolExecutor(max_workers=args.jobs) as pool:
            list(pool.map(download, pairs))
    with ThreadPoolExecutor(max_workers=args.jobs) as pool:
        cards = list(pool.map(process, pairs))
    by_code = {card['code']: card for card in cards}
    for card in cards:
        code = card['code']
        other = by_code.get(code[:-1] + ('b' if code.endswith('a') else 'a')) if code[-1] in 'ab' else None
        if other:
            card.update(doubleSided=True, linkedTo=other['code'])
            if code.endswith('b'):
                card['hidden'] = True
    cards.sort(key=lambda card: (card['position'], card['code']))
    pack = {'code': 'shadowland', 'name': 'Shadowland', 'order': 1000,
            'cardCount': len(cards)}
    reference = {'version': 1, 'source': source['source'],
                 'notes': 'OCR is unverified; icons and stylized lettering may be missing or misread. Consult each scan for authoritative text and stats. All source files, including printed copies, are retained.',
                 'packs': [pack], 'cards': cards}
    REFERENCE.parent.mkdir(parents=True, exist_ok=True)
    REFERENCE.write_text(json.dumps(reference, ensure_ascii=False, indent=2) + '\n')
    index = json.loads(INDEX.read_text())
    index['packs'] = [p for p in index['packs'] if p['code'] != 'shadowland'] + [pack]
    index['cards'] = [c for c in index['cards'] if c['packCode'] != 'shadowland'] + cards
    INDEX.write_text(json.dumps(index, ensure_ascii=False, separators=(',', ':')))
    print(f'Imported {len(cards)} faces; {sum(not c.get("hidden") for c in cards)} browser entries.')


if __name__ == '__main__':
    main()
