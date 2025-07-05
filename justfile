run:
  DWAYNE_CONFIG=./resources/config.yml cabal run

build:
  cabal build

test:
  cabal test

install:
  cabal install --overwrite-policy=always

profile:
  cabal configure --enable-profiling
  cabal run dwayne -- +RTS -p -RTS
