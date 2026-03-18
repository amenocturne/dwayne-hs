# Stage 1: Build Haskell executable
FROM haskell:9.6 AS haskell-builder

WORKDIR /build

COPY core/cabal.project core/cabal.project
COPY core/dwayne-hs.cabal core/dwayne-hs.cabal

WORKDIR /build/core
RUN cabal update && cabal build --only-dependencies exe:dwayne

COPY core/ /build/core/
RUN cabal build exe:dwayne \
    && cp "$(cabal list-bin exe:dwayne)" /build/dwayne

# Stage 2: Build web frontend
FROM node:20-slim AS web-builder

WORKDIR /build/web

COPY web/package.json web/package-lock.json ./
RUN npm ci

COPY web/ ./
RUN npm run build

# Stage 3: Runtime
FROM debian:bookworm-slim

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
       libgmp10 zlib1g libffi8 libsqlite3-0 ca-certificates \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

COPY --from=haskell-builder /build/dwayne /app/dwayne
COPY --from=web-builder /build/web/dist/ /app/web/dist/

RUN mkdir -p /app/data

ENV DWAYNE_CONFIG=/app/data/config.yml

EXPOSE 8080

CMD ["/app/dwayne", "--serve"]
