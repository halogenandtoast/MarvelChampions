#!/usr/bin/env python3
"""Cache official PDFs for local serving. --refresh replaces copies; --check verifies them."""
import argparse
import concurrent.futures
import hashlib
import json
import re
import subprocess
from pathlib import Path
from urllib.parse import unquote, urlparse

ROOT = Path(__file__).resolve().parents[1]
DEST = ROOT / 'frontend/public/rulebooks'
MANIFEST = DEST / 'sources.json'
OFFICIAL_HOSTS = {'cdn.svc.asmodee.net', 'images-cdn.fantasyflightgames.com', 'www.fantasyflightgames.com'}


def inventory():
    found = {}
    if MANIFEST.exists():
        for filename, entry in json.loads(MANIFEST.read_text()).items():
            found[filename] = {entry['source'], *entry.get('aliases', [])}
    texts = [(ROOT / 'docs/rules/README.md').read_text()]
    for path in (ROOT / 'frontend/src').rglob('*'):
        if path.suffix not in {'.ts', '.vue'}:
            continue
        text = path.read_text()
        root = re.search(r"const RULEBOOK_ROOT = ['\"]([^'\"]+)['\"]", text)
        if root and root[1].startswith('https://'):
            text = text.replace('${RULEBOOK_ROOT}', root[1])
        texts.append(text)
    cards = json.loads((ROOT / 'frontend/public/data/cards.json').read_text())['cards']
    texts.extend(card.get('sourceUrl', '') for card in cards)
    for text in texts:
        for url in re.findall(r'https?://[^\s|)<>`\"\']+\.pdf', text):
            parsed = urlparse(url)
            if parsed.hostname not in OFFICIAL_HOSTS:
                continue
            filename = unquote(parsed.path.rsplit('/', 1)[-1])
            found.setdefault(filename, set()).add(url)
    return found


def digest(path):
    data = path.read_bytes()
    if not data.startswith(b'%PDF-'):
        raise ValueError(f'{path.name} is not a PDF')
    return hashlib.sha256(data).hexdigest(), len(data)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--refresh', action='store_true')
    parser.add_argument('--check', action='store_true')
    args = parser.parse_args()
    old = json.loads(MANIFEST.read_text()) if MANIFEST.exists() else {}
    sources = inventory()
    DEST.mkdir(parents=True, exist_ok=True)
    if args.check:
        for filename in sources:
            sha, size = digest(DEST / filename)
            if filename not in old or sha != old[filename]['sha256'] or size != old[filename]['bytes']:
                raise ValueError(f'{filename} does not match its recorded checksum')
        print(f'Verified {len(sources)} local official PDFs.')
        return

    def fetch(item):
        filename, urls = item
        path = DEST / filename
        candidates = sorted(urls, key=lambda u: (not u.startswith('https://cdn.svc.asmodee.net/'), u))
        source = old.get(filename, {}).get('source', candidates[0])
        if args.refresh or not path.exists():
            fallback = 'https://cdn.svc.asmodee.net/production-fantasyflightgames/uploads/2026/09/' + filename
            candidates = list(dict.fromkeys([source, *candidates, fallback]))
            temporary = path.with_suffix('.pdf.download')
            for url in candidates:
                result = subprocess.run(['curl', '--fail', '--location', '--silent', '--show-error', '--retry', '1', '--max-time', '90', '--output', str(temporary), url], capture_output=True, text=True)
                if result.returncode == 0:
                    digest(temporary)
                    temporary.replace(path)
                    source = url
                    urls.add(url)
                    break
            else:
                temporary.unlink(missing_ok=True)
                raise RuntimeError(f'Could not download {filename}: {result.stderr.strip()}')
        sha, size = digest(path)
        print(f'{filename}: {size:,} bytes', flush=True)
        return filename, {'source': source, 'aliases': sorted(urls - {source}), 'sha256': sha, 'bytes': size}

    with concurrent.futures.ThreadPoolExecutor(max_workers=6) as executor:
        entries = dict(executor.map(fetch, sorted(sources.items())))
    MANIFEST.write_text(json.dumps(entries, indent=2) + '\n')
    print(f'Cached {len(entries)} official PDFs ({sum(e["bytes"] for e in entries.values()) / 1024 / 1024:.1f} MiB).')


if __name__ == '__main__':
    main()
