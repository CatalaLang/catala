" In order to enable the syntax highlighting:
"
"     1. Copy or link the current file into $VIMCONFIG/syntax
"
"     2. Enable file type detection by adding to $VIMCONFIG/filetype.vim:
"
"           augroup filetypedetect
"               au! BufRead,BufNewFile *.catala_fr setfiletype catala_fr
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
syn match Include "^\s*>\s*Inclusion:.*$"

syn match sc_id_def contained "\<\([a-zéèàâùîôêœç][a-zéèàâùîôêœçA-ZÉÈÀÂÙÎÔÊŒÇ0-9_\']*\)\>"
syn match cc_id contained "\<\([A-ZÉÈÀÂÙÎÔÊŒÇ][a-zéèàâùîôêœçA-ZÉÈÀÂÙÎÔÊŒÇ0-9_\']*\)\>"

syn match Keyword contained "\<\(contexte\|entrée\|résultat\|interne\|champ\s\+d'application\|collection\|structure\|donnée\|énumération\|définition\|déclaration\|si\s\+et\s\+seulement\s\+si\|dépend\s\+de\|inclus\|contenu\|optionnel\|règle\|sous\s\+condition\|condition\|conséquence\|rempli\|égal\s\+à\|assertion\|état\|étiquette\|exception\|n'importe\s\+quel\)\>"
syn match Statement contained "\<\(selon\|sous\s\+forme\|fixé\|par\|décroissante\|croissante\|varie\|avec\|on\s\+a\|soit\|dans\|tel\s\+que\|existe\|pour\|tout\|de\|initial\)\>"
syn keyword Conditional contained si alors sinon
syn match Comment contained "#.*$"
syn match Number contained "|[0-9]\+-[0-9]\+-[0-9]\+|"
syn match Float contained "\<\([0-9]\+\(,[0-9]*\)*\(.[0-9]*\)\{0,1}\)\>"
syn keyword Boolean contained vrai faux
" (EmileRolley) NOTE: maybe special characters such as '€' should be encoded differently.
syn match Operator contained "\(->\|+\.\|+@\|+\^\|+€\|+\|-\.\|-@\|-\^\|-€\|-\|\*\.\|\*@\|\*\^\|\*€\|\*\|/\.\|/@\|/€\|/\|\!\|>\.\|>=\.\|<=\.\|<\.\|>@\|>=@\|<=@\|<@\|>€\|>=€\|<=€\|<€\|>\^\|>=\^\|<=\^\|<\^\|>\|>=\|<=\|<\|=\|non\|ou\s\+bien\|ou\|et\|€\|%\|an\|mois\|jour\)"
syn match punctuation contained "\(--\|\;\|\.\|,\|\:\|(\|)\|\[\|\]\|{\|}\)"
syn keyword Type contained entier booléen date durée argent texte décimal décret loi nombre somme

syn region ctxt contained
      \ matchgroup=Keyword start="\<\(contexte\|entrée\|résultat\|interne\)\(|\s\+résultat\)"
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

let b:current_syntax = "catala_fr"
