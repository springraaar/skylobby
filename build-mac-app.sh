#!/usr/bin/env bash
#
# build-mac-app.sh — build a standalone macOS skylobby.app locally.
#
# The CI build-mac job (.github/workflows/release.yml) is disabled and its
# jpackage/mac argfile points at resources/icon.ico, a Windows icon jpackage
# can't use on macOS. This script does the mac packaging properly:
#   1. builds the host GUI uberjar (target/skylobby.jar),
#   2. generates an .icns from the per-size PNG icons (resources/icon16..512.png),
#   3. runs `jpackage --type app-image` to produce dist/skylobby.app.
#
# The result bundles a full Java runtime, so it runs on a Mac with no JDK.
# It is arm64-only (built from the host uberjar) and only ad-hoc signed by
# jpackage — not notarized, so Gatekeeper needs a right-click > Open (or
# `xattr -dr com.apple.quarantine dist/skylobby.app`) on other Macs.
#
# Usage:
#   ./build-mac-app.sh                 # build -> dist/skylobby.app
#   ./build-mac-app.sh --skip-frontend # skip the cljs frontend recompile (faster)
#   ./build-mac-app.sh --run           # also smoke-test launch the built app
#
set -euo pipefail

cd "$(dirname "$0")"

# ── Config ───────────────────────────────────────────────────────────────
# A reverse-DNS bundle id is required: jpackage rejects the id it would
# otherwise derive from the main class ("spring_lobby" — underscores invalid).
BUNDLE_ID="com.skynet.skylobby"
# Mirrors the JVM flags in the jpackage/mac argfile.
JAVA_OPTIONS="-Xmx2g -XX:+ExitOnOutOfMemoryError -XX:+DoEscapeAnalysis -XX:+UseCompressedOops --add-opens=java.base/java.nio=ALL-UNNAMED --add-opens=java.base/sun.nio.ch=ALL-UNNAMED"

OUT_DIR="dist"
APP="$OUT_DIR/skylobby.app"

# ── Args ─────────────────────────────────────────────────────────────────
SKIP_FRONTEND=0
DO_RUN=0
for arg in "$@"; do
  case "$arg" in
    --skip-frontend) SKIP_FRONTEND=1 ;;
    --run)           DO_RUN=1 ;;
    -h|--help)       sed -n '2,25p' "$0"; exit 0 ;;
    *) echo "Unknown argument: $arg" >&2; exit 2 ;;
  esac
done

log() { printf '\n=== %s ===\n' "$1"; }

command -v jpackage >/dev/null 2>&1 || { echo "jpackage not found on PATH (need a JDK 17+)" >&2; exit 1; }

# ── 1. Frontend (optional) ───────────────────────────────────────────────
if [ "$SKIP_FRONTEND" -eq 0 ]; then
  log "Compiling cljs frontend"
  npm install
  clojure -M:cljs compile frontend
else
  echo "Skipping frontend recompile (--skip-frontend)"
fi

# ── 2. Build the host GUI uberjar ────────────────────────────────────────
log "Building uberjar (host platform)"
clojure -M:uberjar:headless
[ -f target/skylobby.jar ] || { echo "uberjar build did not produce target/skylobby.jar" >&2; exit 1; }

# ── 3. Generate the macOS .icns from the PNG icon ────────────────────────
log "Generating .icns"
ICNS_DIR="$(mktemp -d)"
trap 'rm -rf "$ICNS_DIR"' EXIT
ICONSET="$ICNS_DIR/skylobby.iconset"
ICNS="$ICNS_DIR/skylobby.icns"
mkdir -p "$ICONSET"
# Copy raar's per-size icons (icon16..512) into the iconset at native
# resolution. icon1024.png / icon.svg still carry the OLD logo upstream, so
# they're intentionally not used; the largest rep is therefore 512x512.
cp resources/icon16.png  "$ICONSET/icon_16x16.png"
cp resources/icon32.png  "$ICONSET/icon_16x16@2x.png"
cp resources/icon32.png  "$ICONSET/icon_32x32.png"
cp resources/icon64.png  "$ICONSET/icon_32x32@2x.png"
cp resources/icon128.png "$ICONSET/icon_128x128.png"
cp resources/icon256.png "$ICONSET/icon_128x128@2x.png"
cp resources/icon256.png "$ICONSET/icon_256x256.png"
cp resources/icon512.png "$ICONSET/icon_256x256@2x.png"
cp resources/icon512.png "$ICONSET/icon_512x512.png"
iconutil -c icns "$ICONSET" -o "$ICNS"

# ── 4. Package the .app ──────────────────────────────────────────────────
log "Packaging skylobby.app"
rm -rf "$APP"
mkdir -p "$OUT_DIR"
# --type app-image yields a standalone .app (no --type would build a .dmg
# installer instead). file-associations / add-launcher from jpackage/lobby
# are unsupported for app-image, so they're intentionally omitted.
jpackage \
  --type app-image \
  --input target \
  --main-jar skylobby.jar \
  --name skylobby \
  --main-class spring_lobby.main \
  --vendor skynet \
  --mac-package-identifier "$BUNDLE_ID" \
  --icon "$ICNS" \
  --java-options "$JAVA_OPTIONS" \
  --dest "$OUT_DIR"
[ -d "$APP" ] || { echo "jpackage did not produce $APP" >&2; exit 1; }

# ── 5. Verify ────────────────────────────────────────────────────────────
log "Verifying"
APP_ID="$(/usr/libexec/PlistBuddy -c 'Print :CFBundleIdentifier' "$APP/Contents/Info.plist")"
[ "$APP_ID" = "$BUNDLE_ID" ] || { echo "Unexpected bundle id: $APP_ID" >&2; exit 1; }
[ -x "$APP/Contents/MacOS/skylobby" ] || { echo "Missing launcher binary" >&2; exit 1; }
[ -f "$APP/Contents/Resources/skylobby.icns" ] || { echo "Missing app icon" >&2; exit 1; }
echo "  bundle id:  $APP_ID"
echo "  launcher:   present"
echo "  icon:       present"
echo "  size:       $(du -sh "$APP" | awk '{print $1}')"

# ── 6. Smoke-test launch (optional) ──────────────────────────────────────
if [ "$DO_RUN" -eq 1 ]; then
  log "Smoke-test launch (12s)"
  set +e
  timeout 12 "$APP/Contents/MacOS/skylobby" >/tmp/skylobby-launch.log 2>&1
  code=$?
  set -e
  if [ "$code" -eq 124 ]; then
    echo "  still running after 12s (good — GUI launched)"
  else
    echo "  app exited early (code $code) — check /tmp/skylobby-launch.log" >&2
    tail -20 /tmp/skylobby-launch.log >&2
    exit 1
  fi
fi

echo
echo "Built: $APP"
echo "Done."
