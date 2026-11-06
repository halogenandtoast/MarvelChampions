# syntax=docker/dockerfile:1.7

FROM node:20-bookworm-slim AS builder
WORKDIR /build

COPY package.json package-lock.json ./
RUN npm ci

COPY . .
ARG VITE_ASSET_HOST=https://assets.marvelchampions.app
ENV VITE_ASSET_HOST=$VITE_ASSET_HOST
RUN test -n "$VITE_ASSET_HOST"
RUN npm run build

FROM caddy:2-alpine AS runtime

COPY --from=builder /build/dist /usr/share/caddy
COPY Caddyfile /etc/caddy/Caddyfile

EXPOSE 80
