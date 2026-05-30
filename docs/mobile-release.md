# Mobile Release

The APK you can copy to a phone and install by tapping must be a release build
signed with Dwayne's private release key. Debug APKs are for `adb` development
only.

## First Setup

```sh
just mobile-release-key
```

`mobile-release-key` creates the keystore and writes matching generated
passwords to `mobile/keystore.properties`. It fills certificate identity fields
with dummy local metadata; only the key passwords matter for APK signing. Keep
both `mobile/keystore.properties` and `mobile/keystores/` out of git and back
them up; losing this key means installed release builds cannot be updated.

The same values can be supplied through environment variables instead:

```sh
DWAYNE_RELEASE_STORE_FILE=keystores/dwayne-release.jks
DWAYNE_RELEASE_STORE_PASSWORD=...
DWAYNE_RELEASE_KEY_ALIAS=dwayne-release
DWAYNE_RELEASE_KEY_PASSWORD=...
```

## Build

```sh
just mobile-release
```

The signed APK is copied to `mobile/dist/dwayne-<version>.apk` and verified with
`apksigner`.

If a device already has a debug build installed, uninstall it once before
installing the release APK. Android treats debug and release signing keys as
different app owners.

```sh
adb uninstall com.skril.dwayne
```

Debug builds use `com.skril.dwayne.debug`, so future `adb` installs can coexist
with the tap-installed release build.
