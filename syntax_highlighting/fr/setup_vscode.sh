#! /usr/bin/env sh

set -eu

FILE=~/.vscode/extensions/catala-fr
SCRIPT=`realpath $0`
SCRIPTPATH=`dirname $SCRIPT`

if [ ! -L "$FILE" ]; then
  echo "Creating link"
  ln -s -f $SCRIPTPATH/vscode "$FILE"
fi
