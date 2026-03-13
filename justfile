default:
  @just --list

install: haskell-install raycast-install

# Haskell dev

haskell-install:
  cd core; cabal build
  cp "$(cd core && cabal list-bin dwayne)" ~/.local/bin/dwayne

haskell-run:
  cd core; DWAYNE_CONFIG=./resources/config.yml cabal run

haskell-serve:
  cd core; DWAYNE_CONFIG=./resources/config.yml cabal run dwayne -- --serve

haskell-build:
  cd core; cabal build

haskell-test:
  cd core; cabal test

haskell-profile:
  cd core; cabal configure --enable-profiling
  cd core; cabal run dwayne -- +RTS -p -RTS

haskell-format:
  cd core; ormolu --mode inplace $(find src app test -name "*.hs" -type f)

# Web dev

web-dev:
  cd web; npm run dev

web-build:
  cd web; npm run build

web-install:
  cd web; npm install

# Mobile dev

mobile-build:
  cd mobile; ANDROID_HOME=/opt/homebrew/share/android-commandlinetools ./gradlew assembleDebug

mobile-clean:
  cd mobile; ./gradlew clean

# Raycast extension

raycast-install:
  cd raycast; bun install
  cd raycast; bun run build

raycast-dev:
  cd raycast; bun run dev
