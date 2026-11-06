# Marvel Champions

An online Marvel Champions: The Card Game. The engine and API are Haskell (`src/`,
`app/`), the client is Vue (`frontend/`).

## Developing

With postgres running locally and GHC 9.14 / stack on the path:

```
./dev.up
```

builds and watches the API, restarting it on each rebuild, and starts the client once
the API answers. Tests: `stack test`.

## Card data and images

- `tools/generate-cards.py` rebuilds `frontend/public/data/cards.json` from MarvelCDB.
- `tools/import-images.py` turns the HD scans (`MarvelHD.zip`, not in the repo) into
  `frontend/public/images/cards/<code>.avif`. The images are not committed. To serve
  them from elsewhere (an S3 bucket), build the client with `VITE_ASSET_HOST` set to its
  origin; it reads `<host>/images/cards/<code>.avif`. The same setting serves token
  images, product art and rulebook PDFs. Without it, local development uses `public/`.

## Rules

`docs/rules/` holds the Rules Reference and Learn to Play as markdown.

## Docker

```
docker compose up
```

then open <http://localhost:3000>. The client image loads images and rulebooks from
`https://assets.marvelchampions.app`; they are excluded from the Docker build.
See [asset hosting](docs/asset-hosting.md) for uploads and CDN configuration.

## Deploying

`terraform/` sets up DigitalOcean (Kubernetes, managed Postgres, DNS and TLS for
marvelchampions.app); `scripts/deploy.sh` builds and pushes both images and applies
`k8s/app.yaml`. See `terraform/README.md`.

The deploy script uses `marvel-builder-v2`, configured by
`scripts/buildkitd.toml`, to retain Stack packages and compiled application modules
between builds. The first build populates these caches. Later source edits compile
incrementally; documentation, tests, infrastructure, and runtime card data do not
invalidate the Haskell compilation layer. `MARVEL_BUILDER` selects another builder.

The backend build script repairs objects touched by an interrupted build before
retrying, while retaining older compiled modules. Caches are separated by target
architecture and compiler, and locked against concurrent writers. If the toolchain
changes incompatibly, increment `BUILD_CACHE_VERSION` in the Dockerfile (or pass
`--build-arg BUILD_CACHE_VERSION=2`). Existing builders read their GC configuration
when they start; editing the TOML file alone does not reconfigure a running builder.

To validate a backend image without publishing or deploying:

```sh
docker buildx build --builder marvel-builder-v2 --platform linux/amd64 \
  --load -t marvel-backend:build-check .
```

Add `--build-arg BUILD_TIMINGS=1 --progress plain` to collect GHC phase timings
and compiler allocation/GC statistics for modules that need recompiling.

The engine and tests compile modules in parallel. The small API executable uses
`-j1` because parallel GHC linking hangs under Rosetta after its C subprocess
exits. This controls compilation, not the server's runtime thread count. Large
engine JSON codecs use Template Haskell to avoid expensive Generic specialization;
compatibility tests check the existing saved-game format. `Marvel.Window.Types`
retains `-O0`; other engine modules retain the normal optimization level.
