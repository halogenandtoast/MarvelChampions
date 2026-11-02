Local card packs use the card viewer's `CardIndex` format. `tools/generate-cards.py`
merges each JSON file here into the MarvelCDB index, so rebuilding the database
preserves these imports.

Shadowland comes from the [shared Google Drive folder](https://drive.google.com/drive/folders/1-p53LqODOLy6RS89H718cvz0kKCwnoPG).
`shadowland.json` contains all 108 source faces across seven sets, including physical
copies and linked fronts/backs. Each record preserves its source filename and URL.
The source inventory is `tools/shadowland-sources.json`.

To download, OCR, convert the scans and update the viewer index:

```sh
python3 tools/import-shadowland.py --download
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

OCR is **unverified**. Each card has searchable `text`, the original `ocr.rawText`,
and `ocr.verified: false`. Stylized titles and symbols may be misread or omitted;
numeric stats, icons and traits are deliberately not inferred. Consult the scan
before implementing a card's behavior. Names and types are taken from source
filenames, with obvious filename typos corrected. This import makes the cards
browsable; it does not implement their engine behavior.
