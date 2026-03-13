# Packaging

install:
  cd core; cabal install --overwrite-policy=always

# Haskell dev

hrun:
  cd core; DWAYNE_CONFIG=./resources/config.yml cabal run

hserve:
  cd core; DWAYNE_CONFIG=./resources/config.yml cabal run dwayne -- --serve

hbuild:
  cd core; cabal build

htest:
  cd core; cabal test

hprofile:
  cd core; cabal configure --enable-profiling
  cd core; cabal run dwayne -- +RTS -p -RTS

hformat:
  cd core; ormolu --mode inplace $(find src app test -name "*.hs" -type f)

# Web dev

wdev:
  cd web; npm run dev

wbuild:
  cd web; npm run build

winstall:
  cd web; npm install

# Mobile dev

mbuild:
  cd mobile; ANDROID_HOME=/opt/homebrew/share/android-commandlinetools ./gradlew assembleDebug

mclean:
  cd mobile; ./gradlew clean

# Raycast extension

raycast-install:
  cd raycast; bun install
  cd raycast; bun run dev

raycast-dev:
  cd raycast; bun run dev
