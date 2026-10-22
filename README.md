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
  origin; it reads `<host>/images/cards/<code>.avif`.

## Rules

`docs/rules/` holds the Rules Reference and Learn to Play as markdown.

## Docker

```
docker compose up
```

then open <http://localhost:3000>. The client image carries the card images it was
built with.

## Deploying

`terraform/` sets up DigitalOcean (Kubernetes, managed Postgres, DNS and TLS for
marvelchampions.app); `scripts/deploy.sh` builds and pushes both images and applies
`k8s/app.yaml`. See `terraform/README.md`.
