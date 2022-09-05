" In order to enable the syntax highlighting:
"
"     1. Copy or link the current file into $VIMCONFIG/syntax
"
"     2. Enable file type detection by adding to $VIMCONFIG/filetype.vim:
"
"           augroup filetypedetect
"               au! BufRead,BufNewFile *.catala_en setfiletype catala_en
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


syn match sc_id_def contained "\<\([a-zéèàâùîôêœç][a-zéèàâùîôêœçA-ZÉÈÀÂÙÎÔÊŒÇ0-9_\']*\)\>"
syn match cc_id contained "\<\([A-ZÉÈÀÂÙÎÔÊŒÇ][a-zéèàâùîôêœçA-ZÉÈÀÂÙÎÔÊŒÇ0-9_\']*\)\>"
syn match Keyword contained "\<\(zakres\|zalezy\s\+od\|deklaracja\|kolekcja\|typu\|opcjonalny\|struktura\|enumeracja\|kontekst\|wejście\|wyjście\|wewnętrzny\|zasada\|pod\s\+warunkuem\|czas\|konsekwencja\|spelnione\|wynosi\|asercja\|definicja\|stan\|etykieta\|wyjątek\|cokolwiek\)\>"
syn match Statement contained "\<\(pasuje\|ze\s\+wzorem\|staloprzecinkowa\|przez\|malejacy\|rosnacy\|rozna\|wraz z\|mamy\|w\|takich ze\|istnieje\|dla\|wszystkie\|z\|jezeli\|wtedy\|inaczej\|poczatkowy\)\>"
syn keyword Conditional contained if then else
syn match Comment contained "#.*$"
syn match Number contained "|[0-9]\+-[0-9]\+-[0-9]\+|"
syn match Float contained "\<\([0-9]\+\(,[0-9]*\|\)*\(.[0-9]*\)\{0,1}\)\>"
syn keyword Boolean contained prawda falsz
syn match Operator contained "\(->\|+\.\|+@\|+\^\|+\$\|+\|-\.\|-@\|-\^\|-\$\|-\|\*\.\|\*@\|\*\^\|\*\$\|\*\|/\.\|/@\|/\$\|/\|\!\|>\.\|>=\.\|<=\.\|<\.\|>@\|>=@\|<=@\|<@\|>\$\|>=\$\|<=\$\|<\$\|>\^\|>=\^\|<=\^\|<\^\|>\|>=\|<=\|<\|=\|nie\|lub\|xor\|i\|\$\|%\|rok\|miesiac\|dzien\)"
syn match punctuation contained "\(--\|\;\|\.\|,\|\:\|(\|)\|\[\|\]\|{\|}\)"
syn keyword Type contained calkowita zerojedynkowy czas trwania pieniądze warunek tekst dziesiętny suma

syn region ctxt contained
      \ matchgroup=Keyword start="\<\(kontekst\|wejście\|wyjście\|wewnętrzny\)\(\|\s\+wyjście\)"
      \ matchgroup=sc_id_def end="\s\+\([a-zéèàâùîôêœç][a-zéèàâùîôêœçA-ZÉÈÀÂÙÎÔÊŒÇ0-9_\']*\)\>"

syn region cc_id_dot_sc_id contained contains=punctuation
      \ matchgroup=cc_id start="\<\([A-ZÉÈÀÂÙÎÔÊŒÇ][a-zéèàâùîôêœçA-ZÉÈÀÂÙÎÔÊŒÇ0-9_\']*\)\."rs=e-1
      \ matchgroup=sc_id_def end="\([a-zéèàâùîôêœç][a-zéèàâùîôêœçA-ZÉÈÀÂÙÎÔÊŒÇ0-9_\']*\)\>"

syn region sc_id_def_dot_sc_id contained contains=punctuation
      \ matchgroup=sc_id_def start="\<\([a-zéèàâùîôêœç][a-zéèàâùîôêœçA-ZÉÈÀÂÙÎÔÊŒÇ0-9_\']*\)\."rs=e-1
      \ matchgroup=sc_id end="\([a-zéèàâùîôêœç][a-zéèàâùîôêœçA-ZÉÈÀÂÙÎÔÊŒÇ0-9_\']*\)\>"

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

let b:current_syntax = "catala_pl"
