#! /usr/bin/env bash

FILE=~/.vscode/extensions/catala-nv
SCRIPT=`realpath $0`
SCRIPTPATH=`dirname $SCRIPT`

if [ ! -L "$FILE" ]; then
  echo "Creating link"
  ln -s -f $SCRIPTPATH/vscode "$FILE"
fi
