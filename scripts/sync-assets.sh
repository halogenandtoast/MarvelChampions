#!/usr/bin/env bash
# Upload changed assets separately from application deployments. Never delete remote
# files: an older app build may still reference them.
set -euo pipefail
ROOT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)"
BUCKET="${MARVEL_ASSET_BUCKET:-marvel-champions-assets}"

aws s3 sync "$ROOT_DIR/frontend/public/images/" "s3://$BUCKET/images/" \
  --exclude '*' --include '*.avif' --include '*.webp' --include '*.png' --include '*.jpg' --include '*.jpeg' --include '*.svg' \
  --cache-control 'public,max-age=86400' --no-progress "$@"
aws s3 sync "$ROOT_DIR/frontend/public/rulebooks/" "s3://$BUCKET/rulebooks/" \
  --exclude '*' --include '*.pdf' \
  --content-type application/pdf --cache-control 'public,max-age=86400' --no-progress "$@"
