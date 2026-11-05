#!/usr/bin/env bash
# deploy.sh — build both images locally for linux/amd64, push them to Docker
# Hub tagged with the git SHA, then apply k8s/app.yaml to the DOKS cluster.
#
# Requires:
#   - `docker buildx` (default in modern Docker Desktop)
#   - `docker login` already done against Docker Hub
#   - `doctl auth init` done, so the cluster's kubeconfig can be fetched
#   - kubectl
#
# Env overrides:
#   MARVEL_DOCKERHUB_USER   Docker Hub namespace (default: halogenandtoast)
#   MARVEL_BUILD_PLATFORM   buildx platform (default: linux/amd64)
#   MARVEL_CLUSTER          DOKS cluster name (default: marvel-champions)
#   MARVEL_BUILDER          buildx builder (default: marvel-builder-v2)

set -euo pipefail

ROOT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)"
DOCKER_USER="${MARVEL_DOCKERHUB_USER:-halogenandtoast}"
PLATFORM="${MARVEL_BUILD_PLATFORM:-linux/amd64}"
CLUSTER="${MARVEL_CLUSTER:-marvel-champions}"
BUILDER="${MARVEL_BUILDER:-marvel-builder-v2}"

SHA="$(git -C "$ROOT_DIR" rev-parse --short HEAD)"
echo "deploy.sh: shipping $SHA"

if ! git -C "$ROOT_DIR" diff --quiet || ! git -C "$ROOT_DIR" diff --cached --quiet; then
  echo "deploy.sh: warning — the image includes uncommitted working-tree changes under tag $SHA" >&2
fi

# A separate builder applies the cache policy without interrupting deployments
# still running on the original marvel-builder. Its first build populates caches.
if ! docker buildx inspect "$BUILDER" >/dev/null 2>&1; then
  docker buildx create --name "$BUILDER" --driver docker-container \
    --buildkitd-config "$ROOT_DIR/scripts/buildkitd.toml" --bootstrap >/dev/null
fi

build_and_push() {
  local image="$1" ctx="$2"
  echo "deploy.sh: building $image:{$SHA,latest} for $PLATFORM"
  docker buildx build \
    --builder "$BUILDER" \
    --platform "$PLATFORM" \
    --tag "$image:$SHA" \
    --tag "$image:latest" \
    --push \
    "$ctx"
}

build_and_push "$DOCKER_USER/marvel-backend"  "$ROOT_DIR"
build_and_push "$DOCKER_USER/marvel-frontend" "$ROOT_DIR/frontend"

echo "deploy.sh: applying k8s/app.yaml to $CLUSTER"
KUBECONFIG="$(mktemp)"
export KUBECONFIG
trap 'rm -f "$KUBECONFIG"' EXIT
doctl kubernetes cluster kubeconfig show "$CLUSTER" > "$KUBECONFIG"

sed -e "s/@TAG@/$SHA/g" -e "s|halogenandtoast/|$DOCKER_USER/|g" "$ROOT_DIR/k8s/app.yaml" \
  | kubectl apply -f -
kubectl rollout status deployment/marvel-backend --timeout=5m
kubectl rollout status deployment/marvel-frontend --timeout=5m

echo "deploy.sh: done"
