#!/bin/bash
set -e

STACK="stack --no-terminal --jobs=1"

# Setup & install dependencies or abort
ret=0
$TIMEOUT 40m $STACK --install-ghc build \
  --only-dependencies --test --haddock \
  || ret=$?
case "$ret" in
  0) # continue
    ;;
  124)
    echo "Timed out while installing dependencies."
    echo "Try pushing a new commit to build again."
    exit 1
    ;;
  *)
    echo "Failed to install dependencies."
    exit 1
    ;;
esac

# Set up configuration
STACK_EXTRA_FLAGS=""
if [ -z "$TRAVIS_TAG" ]
then
  # On non-release builds, disable optimizations.
  STACK_EXTRA_FLAGS="--fast"
fi

if [ "$STACKAGE_NIGHTLY" = "true" ]
then
  STACK_EXTRA_FLAGS="$STACK_EXTRA_FLAGS --resolver=nightly"
fi

echo "STACK_EXTRA_FLAGS=\"$STACK_EXTRA_FLAGS\""
BUILD_COMMAND="$STACK build --pedantic --test $STACK_EXTRA_FLAGS"

if [ "$BUILD_TYPE" = "normal" ]
then
  echo ">>> Building & testing..."
  echo "> $BUILD_COMMAND"
  $BUILD_COMMAND
fi
