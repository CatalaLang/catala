#! /usr/bin/env bash

FILE=~/.atom/packages/catala_fr

if [ ! -L "$FILE" ]; then
  echo "Creating link"
  ln -s -f $(dirname "$0")/atom "$FILE"
fi
