#!/usr/bin/env python3
"""Build reviewed encounter-card supplements using existing local scans only.

Run after editing data/card-text/*.json. --images encodes the Fear No Evil
scans already in images/hd; this script never downloads images or card data.
"""
import argparse
import json
import re
import subprocess
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
TEXT = ROOT / 'data' / 'card-text'
RULEBOOKS = {
    'fne': 'https://cdn.svc.asmodee.net/production-fantasyflightgames/uploads/2026/09/mc60_rulebook-web.pdf',
    'shadowland': 'https://cdn.svc.asmodee.net/production-fantasyflightgames/uploads/2026/10/MC65-Rulebook.pdf',
}
SHADOW_NUMBERS = {
    1: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 10, 11, 11],
    2: [37, 38, 39, 40, 41, 42, 43, 44, 44, 45, 45],
    3: [57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 70, 71, 71],
    4: list(range(28, 37)),
    5: list(range(46, 57)),
    6: list(range(12, 28)),
    7: [72, 72, 72, 72, 73, 74, 75, 76, 76, 77, 77, 78, 78],
}


def read(path):
    return json.loads(path.read_text())


def write(path, value):
    path.write_text(json.dumps(value, ensure_ascii=False, indent=2) + '\n')


def enrich_shadowland(cards):
    texts = read(TEXT / 'shadowland.json')
    metadata = read(TEXT / 'encounter-metadata.json')
    reviewed = read(TEXT / "shadowland-details.json")
    grouped = {}
    aliases = {}
    for original in cards:
        old = original['code']
        match = re.fullmatch(r'shadowland-(\d)-(\d{3})([ab]?)', old)
        if not match:
            # Already enriched: rebuilding the index is idempotent.
            for card in cards:
                card["text"] = texts[card["code"]]
                card["textVerified"] = True
                card["ocr"]["verified"] = False
                card.update(reviewed.get(card["code"], {}))
            return cards
        group, index, side = match.groups()
        number = SHADOW_NUMBERS[int(group)][int(index) - 1]
        code = f'65{number:03d}' + ('' if number == 37 else side)
        aliases[old] = code
        if code in grouped:
            grouped[code]['aliases'].append(old)
            grouped[code]['quantity'] += 1
            continue
        card = dict(original)
        card.update(code=code, aliases=[old], position=number, packOrder=65,
                    quantity=1, text=texts[code], sourceUrl=RULEBOOKS['shadowland'])
        card['textVerified'] = True
        card['traits'] = printed_traits(card['ocr']['rawText'])
        meta = metadata.get(code, {})
        add_stats(card, meta)
        card['name'] = re.sub(r' \d[AB]$', '', meta.get('name', card['name']))
        if number in [3, 4] and side == 'b':
            card['name'] = 'Atop the Tenshu'
        if number == 25:
            card['name'] = 'Shadowy Plot'
        if number == 72:
            card.update(name='Creeping Corruption', type='upgrade', typeName='Upgrade', permanent=True)
            card['traits'] = ['Overpowering'] if side == 'b' else []
        if card['type'] == 'villain':
            card['unique'] = number != 37
            if 12 <= number <= 17:
                card.update(stage=side.upper(), health=15 if side == 'a' else 18, healthPerHero=True)
            elif number in [57, 58, 59]:
                card.update(stage=['I', 'II', 'III'][number - 57], health=[18, 21, 25][number - 57], healthPerHero=True)
            else:
                card.update(health=0, healthPerHero=False)
        if card['type'] == 'main_scheme':
            card['stage'] = ('1' if number in [1, 38, 60] else '2') + side.upper()
        card.update(reviewed.get(code, {}))
        grouped[code] = card
    for card in grouped.values():
        if card.get('linkedTo'):
            card['linkedTo'] = aliases[card['linkedTo']]
        # An original single-sided villain was given an A suffix in the filenames.
        if card['code'] == '65037':
            card.pop('linkedTo', None)
            card.pop('doubleSided', None)
    return sorted(grouped.values(), key=lambda c: c['code'])


def printed_traits(raw):
    candidates = [line.strip() for line in raw.splitlines()
                  if re.fullmatch(r'[A-Z][A-Z. !]+\.', line.strip())]
    ignored = {'COMPLETED.', 'FAILED.', 'SETUP.', 'MARVEL.', 'VILLAIN.', 'ATTACHMENT.'}
    return [part.strip().title() for line in candidates if line not in ignored
            for part in line.split('.') if part.strip()]


def add_stats(card, meta):
    for field in ['health', 'attack', 'scheme']:
        if meta.get(field) is not None:
            card[field] = meta[field]
    if 'boostIcons' in meta:
        card['boost'] = meta['boostIcons']
        card['boostStar'] = meta.get('boostStar', False)
    if card['type'] in ['villain', 'main_scheme', 'support']:
        card.pop('boost', None)
        card.pop('boostStar', None)
    for field in ['acceleration', 'crisis', 'hazard', 'amplify']:
        if field in meta:
            card[field] = meta[field]


def fne_cards(encode_images=False):
    texts = read(TEXT / 'fear-no-evil.json')
    sources = read(ROOT / 'tools' / 'fne-image-sources.json')
    metadata = read(TEXT / 'encounter-metadata.json')
    reviewed = read(TEXT / 'fear-no-evil-details.json')
    cards = []
    for code, text in texts.items():
        source = ROOT / 'images' / 'hd' / sources[code]
        info = reviewed[code]
        card = dict(info)
        card.update(code=code, packCode='fne', packName='Fear No Evil', packOrder=60,
                    position=int(code[2:5]), faction='encounter', factionName='Encounter',
                    text=text, image=code+'.avif', bundledImage=True,
                    sourceFile=sources[code], sourceUrl=RULEBOOKS['fne'],
                    orientation='landscape' if info['type'] in ['main_scheme', 'side_scheme'] else 'portrait',
                    textVerified=True)
        add_stats(card, metadata.get(code, {}))
        # Reviewed printed values take precedence over incomplete secondary metadata.
        card.update(info)
        if code[-1] in 'ab':
            other = code[:-1] + ('b' if code[-1] == 'a' else 'a')
            if other in texts:
                card.update(doubleSided=True, linkedTo=other, hidden=code[-1] == 'b')
        if encode_images:
            target = ROOT / 'frontend' / 'public' / 'images' / 'cards' / card['image']
            if not target.exists():
                args = ['magick', str(source)]
                if card['orientation'] == 'landscape':
                    args += ['-rotate', '-90']
                subprocess.run(args + ['-resize', '840x840>', '-strip', '-quality', '55', str(target)], check=True)
        cards.append(card)
    return cards


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--images', action='store_true', help='encode existing local Fear No Evil scans')
    args = parser.parse_args()
    path = ROOT / 'data' / 'cards' / 'shadowland.json'
    shadow = read(path)
    shadow['cards'] = enrich_shadowland(shadow['cards'])
    shadow['packs'][0].update(order=65, cardCount=len(shadow['cards']))
    shadow.update(source=RULEBOOKS['shadowland'], notes='Reviewed rules text, official collector numbers, and existing local card scans. Legacy source codes remain aliases.')
    write(path, shadow)
    cards = fne_cards(args.images)
    write(ROOT / 'data' / 'cards' / 'fear-no-evil.json', {
        'version': 1, 'source': RULEBOOKS['fne'],
        'notes': 'Encounter and campaign cards supplement MarvelCDB’s existing player cards. Text reviewed against existing local scans.',
        'packs': [], 'cards': cards,
    })
    index_path = ROOT / 'frontend' / 'public' / 'data' / 'cards.json'
    index = read(index_path)
    replaced = {c['code'] for c in cards}
    index['cards'] = [c for c in index['cards'] if c['packCode'] != 'shadowland' and c['code'] not in replaced] + shadow['cards'] + cards
    for pack in index['packs']:
        if pack['code'] in ['fne', 'shadowland']:
            pack['cardCount'] = sum(c['packCode'] == pack['code'] for c in index['cards'])
            pack['order'] = 60 if pack['code'] == 'fne' else 65
    index['cards'].sort(key=lambda c: (c.get('packOrder', 999), c.get('position', 0), c['code']))
    index_path.write_text(json.dumps(index, ensure_ascii=False, separators=(',', ':')))
    print(f'Updated {len(shadow["cards"])} Shadowland and {len(cards)} Fear No Evil faces.')


if __name__ == '__main__':
    main()
