#! /bin/bash

temp=$(mktemp)
(Rscript -e "options(styler.colored_print.vertical=FALSE); con <- file('stdin'); out <- styler::style_text(readLines(con)); close(con); out" < $1) > $temp 
cat $temp > $1
