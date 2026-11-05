# syntax=docker/dockerfile:1.7

# Stage 1: build the Haskell binary with stack. The haskell image's ghc matches
# stack.yaml's compiler, which stack.yaml tells stack to use (system-ghc).
FROM haskell:9.14.1 AS dependencies

ARG TARGETARCH
ARG BUILD_CACHE_VERSION=1

WORKDIR /build

RUN apt-get update \
  && apt-get install -y --no-install-recommends \
       ca-certificates curl gnupg lsb-release \
       libgmp-dev zlib1g-dev pkg-config \
  && install -d /usr/share/postgresql-common/pgdg \
  && curl -fsSLo /usr/share/postgresql-common/pgdg/apt.postgresql.org.asc \
       https://www.postgresql.org/media/keys/ACCC4CF8.asc \
  && echo "deb [signed-by=/usr/share/postgresql-common/pgdg/apt.postgresql.org.asc] https://apt.postgresql.org/pub/repos/apt $(lsb_release -cs)-pgdg main" \
       > /etc/apt/sources.list.d/pgdg.list \
  && apt-get update \
  && apt-get install -y --no-install-recommends libpq-dev \
  && rm -rf /var/lib/apt/lists/*

# Pull in just the dependency descriptors first so dependency compilation
# can be cached independently from source changes.
COPY stack.yaml stack.yaml.lock package.yaml MarvelChampions.cabal ./
RUN --mount=type=cache,id=marvel-stack-${TARGETARCH}-ghc9.14.1-v${BUILD_CACHE_VERSION},target=/root/.stack,sharing=locked \
    --mount=type=cache,id=marvel-work-${TARGETARCH}-ghc9.14.1-v${BUILD_CACHE_VERSION},target=/build/.stack-work,sharing=locked \
    stack +RTS -V0 -RTS build --only-dependencies --no-terminal

FROM dependencies AS builder

COPY src/ src/
COPY app/ app/
COPY scripts/docker-build-backend.sh scripts/docker-build-backend.sh
ARG BUILD_TIMINGS=0
RUN --mount=type=cache,id=marvel-stack-${TARGETARCH}-ghc9.14.1-v${BUILD_CACHE_VERSION},target=/root/.stack,sharing=locked \
    --mount=type=cache,id=marvel-work-${TARGETARCH}-ghc9.14.1-v${BUILD_CACHE_VERSION},target=/build/.stack-work,sharing=locked \
    MARVEL_BUILD_TIMINGS="$BUILD_TIMINGS" sh scripts/docker-build-backend.sh

# Stage 2: minimal runtime.
FROM debian:bookworm-slim AS runtime

RUN apt-get update \
  && apt-get install -y --no-install-recommends \
       libgmp10 \
       libtinfo6 \
       libpq5 \
       zlib1g \
       ca-certificates \
       netbase \
  && rm -rf /var/lib/apt/lists/* \
  && useradd --create-home --shell /usr/sbin/nologin marvel

USER marvel
WORKDIR /home/marvel

COPY --from=builder /out/marvel-champions /usr/local/bin/marvel-champions
# Decks resolve against the card browser's database.
COPY frontend/public/data/cards.json /home/marvel/cards.json

ENV PORT=3000
ENV CARDS_JSON=/home/marvel/cards.json
EXPOSE 3000

CMD ["marvel-champions"]
