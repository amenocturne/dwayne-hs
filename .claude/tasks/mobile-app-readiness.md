# Mobile App Readiness

## Progress
- [x] Phase 1: Mutation API endpoints
- [ ] Phase 2: Web frontend mutation wiring
- [ ] Phase 3: Phase 2 keywords (TODAY/SOON)
- [ ] Phase 4: Vault reader API
- [x] Phase 5: Kotlin/Compose project setup
- [ ] Phase 6: Wire screens to live API    <-- next

## Current State
**Android app scaffold is complete, builds, and runs on physical device.**

### What's done
- 5 mutation endpoints added to Servant API (prior session)
- Android project scaffolded in `mobile/` with Kotlin + Jetpack Compose
- Android SDK CLI tools installed via brew (`/opt/homebrew/share/android-commandlinetools`)
- `ANDROID_HOME` configured in `~/.config/zsh/usr/android.zsh`
- `USE_MOCK_DATA` build flag: debug=true (offline mock), release=false (live API)
- Successfully installed and tested on physical Android device
- Clean `./gradlew assembleDebug` — zero warnings

### Mobile app structure
- **Data layer:** Task.kt (all API models with kotlinx.serialization), TodoKeyword enum, MockTaskRepository
- **Mock data:** Realistic inbox/today/soon/todo tasks matching Haskell API JSON
- **3 screens:**
  - TaskFeedScreen — scrollable tab row (work-queue, inbox, today, soon, todo, done), LazyColumn of TaskCards
  - CaptureScreen — text input + send, recently-captured list
  - SwipeProcessingScreen — tinder-style card drag (left=SOMEDAY, right=TODO, up=TODAY)
- **Navigation:** Bottom nav bar with 3 tabs (Feed, Capture, Process)
- **Theme:** Material 3 dark theme with keyword/priority-specific colors
- **Build:** `just mbuild` / `just mclean`
- **Deploy:** `adb install mobile/app/build/outputs/apk/debug/app-debug.apk`

### What's next (pick any)
1. **Wireless debugging** — set up ADB over WiFi/Bluetooth so deploys don't need USB cable (`adb pair`/`adb connect`)
2. **Wire to live API** — replace MockTaskRepository with Ktor HTTP client hitting real Dwayne server
3. **Web frontend mutation wiring** — independent of mobile
4. **Vault reader API** — needed before mobile vault reader screen
5. **UX polish** — animations, swipe-back gestures, search bar, pull-to-refresh

### API gap analysis for mobile
- Task feed: done (14 views + pagination + search)
- Capture: done (`POST /api/tasks/capture`)
- State transitions: done (mutation endpoints)
- Vault reader: not started
- Batch operations: not started (low priority)
- Offline queue: mobile-side concern, no API changes needed

## Notes
- Mutation handlers follow captureHandler pattern: `modifyMVar` → Core.Operations → `storeSave`
- `withMutation` in `Api/Handlers.hs` is the reusable glue — new mutation endpoints are one-liners
- Commits: 70fc921 (scaffold), 908548e (build flag)
- Android Studio not installed — using CLI SDK only. Can install later for Compose previews.
- Brew SDK at `/opt/homebrew/share/android-commandlinetools` (not default `~/Library/Android/sdk`)
- Gradle wrapper 8.11.1, system gradle 9.3.1
- Phone device ID: 2B081FDH20099A
