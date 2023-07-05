#!/bin/bash

panic() {
  printf "%s\n" "$1"
  exit 1
}

version() { echo "$@" | awk -F. '{ printf("%d%03d%03d%03d\n", $1,$2,$3,$4); }'; }

: ${LLVM_CONFIG=}

if [ ! "$LLVM_CONFIG" ]; then
  if [ -x "$(command -v llvm-config-15)" ]; then
     LLVM_CONFIG=llvm-config-15
  elif [ -x "$(command -v llvm-config)" ]; then
     LLVM_CONFIG=llvm-config
  else
     panic "Failed to find 'llvm-config'"
  fi
fi

MIN_LLVM_VERSION=("15.0.0")
if [ $(version $($LLVM_CONFIG --version)) -lt $(version $MIN_LLVM_VERSION) ]; then
  echo "Tried to use " $(which $LLVM_CONFIG) "version" $($LLVM_CONFIG --version)
  panic "Requirement: 'llvm-config' must be version 15"
fi

MAX_LLVM_VERSION=("15.999.999")
if [ $(version $($LLVM_CONFIG --version)) -gt $(version $MAX_LLVM_VERSION) ]; then
  echo "Tried to use " $(which $LLVM_CONFIG) "version" $($LLVM_CONFIG --version)
  panic "Requirement: 'llvm-config' must be version 15"
fi

SCRIPT=`realpath $0`
SCRIPTPATH=`dirname $SCRIPT`
LINKER_FLAGS=`$LLVM_CONFIG --libs core native --system-libs`

pushd "${SCRIPTPATH}/../" >/dev/null 2>&1
./noxb nox-self -out:noxs -verbose -verbose-more-timings -extra-linker-flags:$LINKER_FLAGS -o3 -disable-assert
popd >/dev/null 2>&1
