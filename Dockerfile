FROM node:lts as frontend

# Frontend

ENV LC_ALL=en_US.UTF-8

RUN npm install -g @vue/cli

RUN mkdir -p /opt/marvel/src/frontend

# ENV VUE_APP_API_HOST "https://marvel-horror-api.herokuapp.com"

WORKDIR /opt/marvel/src/frontend
COPY ./frontend/package.json /opt/marvel/src/frontend/package.json
COPY ./frontend/babel.config.js /opt/marvel/src/frontend/babel.config.js
COPY ./frontend/tsconfig.json /opt/marvel/src/frontend/tsconfig.json
COPY ./frontend/package-lock.json /opt/marvel/src/frontend/package-lock.json
RUN npm ci
WORKDIR /opt/marvel/src/frontend
COPY ./frontend /opt/marvel/src/frontend
ENV VUE_APP_ASSET_HOST ${ASSET_HOST:-""}
RUN npm run build

FROM fpco/stack-build:latest as dependencies

ENV LC_ALL=en_US.UTF-8

RUN mkdir -p \
  /opt/marvel/bin \
  /opt/marvel/src/backend/marvel-api \
  /opt/marvel/src/backend/marvel-core \
  /opt/marvel/src/backend/cards-discover

WORKDIR /opt/marvel/src/backend
COPY ./backend/stack.yaml /opt/marvel/src/backend/stack.yaml
COPY ./backend/marvel-api/package.yaml /opt/marvel/src/backend/marvel-api/package.yaml
COPY ./backend/marvel-core/package.yaml /opt/marvel/src/backend/marvel-core/package.yaml
COPY ./backend/cards-discover/package.yaml /opt/marvel/src/backend/cards-discover/package.yaml
RUN stack build --system-ghc --dependencies-only --no-terminal --ghc-options '-j4 +RTS -A128m -n2m -RTS'

FROM fpco/stack-build:lts-18.18 as api

ENV LC_ALL=en_US.UTF-8

# API

ENV PATH "$PATH:/opt/stack/bin:/opt/marvel/bin"

RUN mkdir -p \
  /opt/marvel/src/backend \
  /opt/marvel/bin

COPY ./backend /opt/marvel/src/backend
COPY --from=dependencies /root/.stack /root/.stack

WORKDIR /opt/marvel/src/backend/cards-discover
RUN stack build --system-ghc --no-terminal --ghc-options '-j4 +RTS -A128m -n2m -RTS' cards-discover

WORKDIR /opt/marvel/src/backend/marvel-api
RUN stack build --no-terminal --system-ghc --ghc-options '-j4 +RTS -A128m -n2m -RTS'
RUN stack --no-terminal --local-bin-path /opt/marvel/bin install

FROM ubuntu:18.04 as app

# App

ENV LC_ALL=en_US.UTF-8

RUN apt-get update && \
  apt-get upgrade -y --assume-yes && \
  apt-get install -y --assume-yes libpq-dev ca-certificates nginx && \
  rm -rf /var/lib/apt/lists/*

RUN mkdir -p \
  /opt/marvel/bin \
  /opt/marvel/src/backend/marvel-api \
  /opt/marvel/src/frontend \
  /var/log/nginx \
  /var/lib/nginx \
  /run

COPY --from=frontend /opt/marvel/src/frontend/dist /opt/marvel/src/frontend/dist
COPY --from=api /opt/marvel/bin/marvel-api /opt/marvel/bin/marvel-api
COPY ./backend/marvel-api/config /opt/marvel/src/backend/marvel-api/config
COPY ./prod.nginxconf /opt/marvel/src/backend/prod.nginxconf
COPY ./start.sh /opt/marvel/src/backend/marvel-api/start.sh

RUN useradd -ms /bin/bash yesod && \
  chown -R yesod:yesod /opt/marvel /var/log/nginx /var/lib/nginx /run && \
  chmod a+x /opt/marvel/src/backend/marvel-api/start.sh
USER yesod
ENV PATH "$PATH:/opt/stack/bin:/opt/marvel/bin"

EXPOSE 3000

WORKDIR /opt/marvel/src/backend/marvel-api
ENV PORT ${PORT:-"3000"}
CMD ["./start.sh"]
