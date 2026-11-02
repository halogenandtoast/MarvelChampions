Local card packs use the card viewer's `CardIndex` format. `tools/generate-cards.py`
merges each JSON file here into the MarvelCDB index, so rebuilding the database
preserves these imports.

Shadowland comes from the [shared Google Drive folder](https://drive.google.com/drive/folders/1-p53LqODOLy6RS89H718cvz0kKCwnoPG).
`shadowland.json` contains 93 distinct faces across seven sets, representing all
108 source faces through quantities and aliases. Each record preserves its source
filename.
The source inventory is `tools/shadowland-sources.json`.

To rebuild from the existing local scans and update the viewer index:

```sh
python3 tools/import-shadowland.py
```

Requires `curl`, ImageMagick with AVIF support, and Tesseract with English data.
Downloads use Drive previews (at least 1024px) or cached full-resolution TIFFs.
Use `--download --originals` to fetch all original TIFFs (about 2 GB). To reprocess
after upgrading sources or changing OCR settings, remove the affected cached
`.ocr.json` and generated `.avif` files before running the importer again.

Source images and OCR caches are stored under ignored `images/shadowland/`;
viewer images are stored under ignored `frontend/public/images/cards/`, following
the existing 840px, quality 55 AVIF convention. Imported records set `bundledImage`
so these scans load from the app even when `VITE_ASSET_HOST` points to a bucket.
Include the generated scans when building the client for deployment.

The reviewed Shadowland supplement has 93 distinct faces covering all 78 collector
numbers. Repeated physical copies are represented by `quantity`. Its earlier local
codes remain in `aliases`, so saved card links continue to work. Rules text is
reviewed separately from the preserved raw OCR: `textVerified` applies to `text`,
while `ocr.rawText` remains the original, uncorrected transcription.

Fear No Evil adds 171 encounter and campaign faces to the 68 player faces already
provided by MarvelCDB. This includes all five interchangeable scenarios, their
villains, Kingpin, six modular sets, and the campaign cards. Printed rules text is
in `data/card-text/fear-no-evil.json`; reviewed names, types and statistics are in
`fear-no-evil-details.json` beside it.

Rebuild these supplements and the viewer index without network access:

```sh
python3 tools/encounter-card-data.py
```

Add `--images` to encode the existing Fear No Evil HD crops as local AVIFs. The
source mapping is `tools/fne-image-sources.json`; this operation never downloads
images. Shadowland continues to use its existing local AVIFs. Generated scans are
ignored by Git and must accompany the client when it is deployed.

The official rulebooks are the [Fear No Evil rulebook](https://cdn.svc.asmodee.net/production-fantasyflightgames/uploads/2026/09/mc60_rulebook-web.pdf)
and [Shadowland rulebook](https://cdn.svc.asmodee.net/production-fantasyflightgames/uploads/2026/10/MC65-Rulebook.pdf).
These imports make the cards browsable; engine behavior is implemented separately.
