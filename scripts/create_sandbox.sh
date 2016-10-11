#!/usr/bin/env bash -e

error() {
  local parent_lineno="$1"
  local message="$2"
  local code="${3:-1}"
  if [[ -n "$message" ]] ; then
    echo "Error on or near line ${parent_lineno}: ${message}; exiting with status ${code}"
  else
    echo "Error on or near line ${parent_lineno}; exiting with status ${code}"
  fi
  exit "${code}"
}
trap 'error ${LINENO}' ERR

TOP_DIR=$(git rev-parse --show-toplevel)
BASE_REPO="${TOP_DIR}/fixtures/base_repo.git"
SANDBOX="${TOP_DIR}/sandbox"

cleanup() {
  set +x
  rm -rf $SANDBOX
}
if [ -d $SANDBOX ]; then
  echo "Error: Directory exists: $SANDBOX"
  exit 1
fi

mkdir $SANDBOX
cp -r $BASE_REPO "$SANDBOX/.git"
cd $SANDBOX
#trap 'set +x; cleanup' 0

git checkout .

set -x
