#! /usr/bin/env bash

FILE=~/.atom/packages/catala_en
SCRIPT=`realpath $0`
SCRIPTPATH=`dirname $SCRIPT`

if [ ! -L "$FILE" ]; then
  echo "Creating link"
  ln -s -f $SCRIPTPATH/atom "$FILE"
fi
