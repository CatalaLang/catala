" In order to enable the syntax highlighting:
"
"     1. Copy or link the current file into $VIMCONFIG/syntax
"
"     2. Enable file type detection by adding to $VIMCONFIG/filetype.vim:
"
"           augroup filetypedetect
"               au! BufRead,BufNewFile *.catala_ro setfiletype catala_ro
"           augroup END
"
" More informations could be found at:
"
"     https://elias.rhi.hi.is/vim/syntax.html#:syn-files
"

if exists("b:current_syntax")
  finish
endif

syn match PreProc "^\s*#.*$"
syn match Include "^\s*>\s*Include:.*$"

syn match sc_id_def contained "\<\([a-zăâîșțĂÂÎȘȚ][a-zăâîșțĂÂÎȘȚA-Z0-9_\']*\)\>"
syn match cc_id contained "\<\([A-ZĂÂÎȘȚ][a-zăâîșțĂÂÎȘȚA-Z0-9_\']*\)\>"
syn match Keyword contained "\<\(scop\|depinde\s\+de\|declarație\|include\|listă\s\+de\|conținut\|tip\|opțional\|structură\|enumerare\|context\|regulă\|în\s\+condiția\|condiție\|date\|consecință\|îndeplinit\|egal\|aserțiune\|definiție\|stare\|etichetă\|excepție\|orice\|listă\s+goală\)\>"
syn match Statement contained "\<\(potrivește\|cu\s\+model\|dar\s\+înlocuiește\|fixat\|de\|descrescător\|crescător\|variază\|cu\|avem\|fie\|în\|astfel\s\+că\|există\|pentru\|toți\|de\|dacă\|atunci\|altfel\|inițial\|printre\|este\s+maxim\|este\s+minim\|combină\|aplică\s+fiecare\|la\|inițial\)\>"
syn keyword Conditional contained dacă atunci altfel
syn match Comment contained "#.*$"
syn match Number contained "|[0-9]\+-[0-9]\+-[0-9]\+|"
syn match Float contained "\<\([0-9]\+\(\.[0-9]*\)*\(\,[0-9]*\)\{0,1}\)\>"
syn keyword Boolean contained adevărat fals
syn match Operator contained "\(->\|+\.\|+@\|+\^\|+\$\|+\|-\.\|-@\|-\^\|-\$\|-\|\*\.\|\*@\|\*\^\|\*\$\|\*\|/\.\|/@\|/\$\|/\|\!\|>\.\|>=\.\|<=\.\|<\.\|>@\|>=@\|<=@\|<@\|>\$\|>=\$\|<=\$\|<\$\|>\^\|>=\^\|<=\^\|<\^\|>\|>=\|<=\|<\|=\|nu\|sau\|sau\s\+exclusiv\|și\|\$\|RON\|%\|lună\|an\|zi\)"
syn match punctuation contained "\(--\|\;\|\.\|,\|\:\|(\|)\|\[\|\]\|{\|}\)"
syn keyword Type contained întreg boolean dată durată bani text zecimal număr sumă position_source

syn region ctxt contained
      \ matchgroup=Keyword start="\<\(context\|intrare\|ieșire\|intern\)\(\|\s\+ieșire\)"
      \ matchgroup=sc_id_def end="\s\+\([a-zăâîșțĂÂÎȘȚ][a-zăâîșțĂÂÎȘȚA-Z0-9_\']*\)\>"

syn region cc_id_dot_sc_id contained contains=punctuation
      \ matchgroup=cc_id start="\<\([A-ZĂÂÎȘȚ][a-zăâîșțĂÂÎȘȚA-Z0-9_\']*\)\."rs=e-1
      \ matchgroup=sc_id_def end="\([a-zăâîșțĂÂÎȘȚ][a-zăâîșțĂÂÎȘȚA-Z0-9_\']*\)\>"

syn region sc_id_def_dot_sc_id contained contains=punctuation
      \ matchgroup=sc_id_def start="\<\([a-zăâîșțĂÂÎȘȚ][a-zăâîșțĂÂÎȘȚA-Z0-9_\']*\)\."rs=e-1
      \ matchgroup=sc_id end="\([a-zăâîșțĂÂÎȘȚ][a-zăâîșțĂÂÎȘȚA-Z0-9_\']*\)\>"

syn region code transparent matchgroup=Ignore start="```catala" matchgroup=Ignore end="```"
      \ contains=ALLBUT, PreProc, Include

syn region metadata transparent matchgroup=Ignore start="```catala-metadata" matchgroup=Ignore end="```"
      \ contains=ALLBUT, PreProc, Include

" Synchronizes the position where redrawing start at the start of a code block.
syntax sync match codeSync grouphere code "```catala\w*"

hi link sc_id_def Identifier
hi link sc_id Function
hi link cc_id Type
hi link punctuation Ignore

let b:current_syntax = "catala_ro"
