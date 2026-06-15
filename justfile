default:
  @just --list

install: haskell-install raycast-install

# Haskell dev
#
# zlib lib/include dirs are exported from pkg-config (zlib comes from
# nix-managed home-manager packages — see dotfiles/modules/packages.nix).
# Cabal's `zlib` package needs LIBRARY_PATH/C_INCLUDE_PATH at link time;
# PKG_CONFIG_PATH alone is not enough for it.
#
# We source hm-session-vars.sh because non-login shells (CI, agents,
# subprocesses) don't get PKG_CONFIG_PATH from .zprofile.
export LIBRARY_PATH := `set +u; f="/etc/profiles/per-user/$USER/etc/profile.d/hm-session-vars.sh"; [ -f "$f" ] && . "$f"; pkg-config --variable=libdir zlib 2>/dev/null || true`
export C_INCLUDE_PATH := `set +u; f="/etc/profiles/per-user/$USER/etc/profile.d/hm-session-vars.sh"; [ -f "$f" ] && . "$f"; pkg-config --variable=includedir zlib 2>/dev/null || true`

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
  cd mobile; ANDROID_HOME="${ANDROID_HOME:-/opt/homebrew/share/android-commandlinetools}" ./gradlew assembleDebug

mobile-install:
  bash mobile/scripts/agent-harness build-install

mobile-launch:
  bash mobile/scripts/agent-harness launch

mobile-share text:
  bash mobile/scripts/agent-harness share "{{text}}"

mobile-screenshot path="mobile/tmp/screenshot.png":
  bash mobile/scripts/agent-harness screenshot "{{path}}"

mobile-view-hierarchy:
  bash mobile/scripts/agent-harness hierarchy

mobile-release-key:
  #!/usr/bin/env bash
  set -euo pipefail
  if [[ -e mobile/keystores/dwayne-release.jks || -e mobile/keystore.properties ]]; then
    echo "Release key/config already exists. Keep it, or remove mobile/keystores/dwayne-release.jks and mobile/keystore.properties before regenerating."
    exit 1
  fi
  password="$(uuidgen | tr -d '-')"
  mkdir -p mobile/keystores
  keytool -genkeypair -v -keystore mobile/keystores/dwayne-release.jks -storepass "$password" -keypass "$password" -alias dwayne-release -keyalg RSA -keysize 4096 -validity 10000 -dname "CN=Dwayne, OU=Personal, O=Personal, L=Nowhere, ST=None, C=XX" -noprompt
  printf 'storeFile=keystores/dwayne-release.jks\nstorePassword=%s\nkeyAlias=dwayne-release\nkeyPassword=%s\n' "$password" "$password" > mobile/keystore.properties
  echo "Created mobile/keystores/dwayne-release.jks and mobile/keystore.properties. Back them up; they are required for app updates."

mobile-release:
  #!/usr/bin/env bash
  set -euo pipefail
  android_home="${ANDROID_HOME:-/opt/homebrew/share/android-commandlinetools}"
  ANDROID_HOME="$android_home" mobile/gradlew --project-dir mobile :app:assembleRelease

  version="$(awk -F'"' '/versionName =/ { print $2; exit }' mobile/app/build.gradle.kts)"
  apk="mobile/dist/dwayne-$version.apk"
  signer="$(find "$android_home/build-tools" -type f -name apksigner | sort | tail -n 1)"
  test -n "$signer"

  mkdir -p mobile/dist
  cp mobile/app/build/outputs/apk/release/app-release.apk "$apk"
  "$signer" verify --verbose "$apk"
  printf 'Release APK: %s\n' "$apk"

mobile-clean:
  cd mobile; ./gradlew clean

# Raycast extension

raycast-install:
  cd raycast; bun install
  cd raycast; bun run build

raycast-dev:
  cd raycast; bun run dev
