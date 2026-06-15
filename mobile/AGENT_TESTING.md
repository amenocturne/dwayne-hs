# Mobile Agent Testing

Use the root `just` recipes for repeatable agent checks:

```bash
just mobile-build
just mobile-install
just mobile-launch
just mobile-share "test capture"
just mobile-screenshot mobile/tmp/screenshot.png
just mobile-view-hierarchy
```

Android SDK tools are expected to come from the user's nix/dotfiles setup, not ad-hoc installs. If `adb`, `emulator`, or SDK build tools are missing, add the Android SDK command-line/platform tools to `/Users/skril/Vault/Projects/personal/dotfiles` and rebuild the environment.

The debug app component is:

```text
com.skril.dwayne.debug/com.skril.dwayne.MainActivity
```

The helper script is `mobile/scripts/agent-harness`. It supports `build`, `install`, `build-install`, `launch`, `share`, `screenshot`, `hierarchy`, and `test-share-flow`.
