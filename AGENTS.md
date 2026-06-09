# Dwayne Agent Notes

Use `just` for project commands; run `just` to list available recipes.

## Android

The mobile recipes intentionally set the Android SDK fallback:

```bash
ANDROID_HOME="${ANDROID_HOME:-/opt/homebrew/share/android-commandlinetools}"
```

When running Gradle directly, preserve that convention. For example:

```bash
cd mobile
ANDROID_HOME="${ANDROID_HOME:-/opt/homebrew/share/android-commandlinetools}" ./gradlew :app:testDebugUnitTest
```

Do not assume missing `ANDROID_HOME` means the Android SDK is unavailable; check the justfile first.
