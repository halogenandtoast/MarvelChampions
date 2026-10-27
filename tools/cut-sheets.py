#!/usr/bin/env python3
"""Cut a print-and-play sheet PDF (3x3 cards a page, 300 dpi, cards butted together inside a
white margin) into one PNG a card, for tools/image-overrides.json to name.

Usage:
  /tmp/mc-venv/bin/python -I tools/cut-sheets.py ~/Downloads/61_Jessica_Jones.pdf

Writes images/hd/sheets/<pdf name>/<page>-<cell>.png, cells numbered 0-8 left to right, top to
bottom. Needs pdfimages (poppler).
"""
import subprocess, sys, tempfile
from pathlib import Path

import numpy as np
from PIL import Image

ROOT = Path(__file__).resolve().parent.parent


def bounds(a):
    """The first and last row (or column) that isn't the white page margin."""
    ink = np.flatnonzero((a <= 235).mean(1) > 0.1)
    return ink[0], ink[-1] + 1


def main():
    pdf = Path(sys.argv[1])
    out = ROOT / "images" / "hd" / "sheets" / pdf.stem
    out.mkdir(parents=True, exist_ok=True)
    with tempfile.TemporaryDirectory() as tmp:
        subprocess.run(["pdfimages", "-png", pdf, f"{tmp}/p"], check=True)
        # pdfimages writes each page's image then its soft mask
        pages = [p for p in sorted(Path(tmp).glob("p-*.png")) if Image.open(p).mode != "L"]
        for n, page in enumerate(pages):
            im = Image.open(page).convert("RGB")
            a = np.asarray(im.convert("L"))
            y0, y1 = bounds(a)
            x0, x1 = bounds(a.T)
            for cell in range(9):
                i, j = cell % 3, cell // 3
                box = (round(x0 + i * (x1 - x0) / 3), round(y0 + j * (y1 - y0) / 3),
                       round(x0 + (i + 1) * (x1 - x0) / 3), round(y0 + (j + 1) * (y1 - y0) / 3))
                im.crop(box).save(out / f"{n}-{cell}.png")
    print(f"{len(pages)} pages to {out.relative_to(ROOT)}")


if __name__ == "__main__":
    main()
