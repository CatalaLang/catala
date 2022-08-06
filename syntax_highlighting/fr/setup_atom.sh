#! /usr/bin/env sh

set -eu

FILE=~/.atom/packages/catala_fr
SCRIPT=`realpath $0`
SCRIPTPATH=`dirname $SCRIPT`

if [ ! -L "$FILE" ]; then
  echo "Creating link"
  ln -s -f $SCRIPTPATH/atom "$FILE"
fi
