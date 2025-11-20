run:
  cd core; DWAYNE_CONFIG=./resources/config.yml cabal run

build:
  cd core; cabal build

test:
  cd core; cabal test

install:
  cd core; cabal install --overwrite-policy=always

profile:
  cd core; cabal configure --enable-profiling
  cd core; cabal run dwayne -- +RTS -p -RTS

format:
  cd core; ormolu --mode inplace $(find src app test -name "*.hs" -type f)
