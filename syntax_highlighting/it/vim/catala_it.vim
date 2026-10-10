" In order to enable the syntax highlighting:
"
"     1. Copy or link the current file into $VIMCONFIG/syntax
"
"     2. Enable file type detection by adding to $VIMCONFIG/filetype.vim:
"
"           augroup filetypedetect
"               au! BufRead,BufNewFile *.catala_it setfiletype catala_it
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
syn match Include "^\s*>\s*Inclusione:.*$"

syn match sc_id_def contained "\<\([a-zàèéìíîòóùú][a-zàèéìíîòóùúA-ZÀÈÉÌÍÎÒÓÙÚ0-9_\']*\)\>"
syn match cc_id contained "\<\([A-ZÀÈÉÌÍÎÒÓÙÚ][a-zàèéìíîòóùúA-ZÀÈÉÌÍÎÒÓÙÚ0-9_\']*\)\>"

syn match Keyword contained "\<\(contesto\|entrata\|risultato\|interno\|campo\s\+di\s\+applicazione\|lista\s\+di\|opzionale\s\+di\|struttura\|dato\|enumerazione\|definizione\|dichiarazione\|dipende\s\+da\|contenuto\|tipo\|regola\|sotto\s\+condizione\|condizione\|conseguenza\|soddisfatta\|vale\|asserzione\|stato\|etichetta\|eccezione\|qualsiasi\|o\s\+se\s\+lista\s\+vuota\)\>"
syn match Statement contained "\<\(secondo\|con\s\+forma\|ma\s\+sostituendo\|per\s\+difetto\|per\s\+eccesso\|con\|abbiamo\|sia\|in\|tale\s\+che\|esiste\|per\|ogni\|di\|contenuto\s\+in\|massimo\|minimo\|combina\|trasforma\s\+ogni\|diventa\|inizialmente\|ordina\|in\s\+ordine\s\+\(de\)\?crescente\|e\s\+poi\|impossibile\|arrotondato\|numero\|somma\|contiene\)\>"
syn keyword Conditional contained se allora altrimenti
syn match Comment contained "#.*$"
syn match Number contained "|[0-9]\+-[0-9]\+-[0-9]\+|"
syn match Float contained "\<\([0-9]\+\(,[0-9]*\)*\(.[0-9]*\)\{0,1}\)\>"
syn keyword Boolean contained vero falso
syn match Operator contained "\(->\|+\.\|+@\|+\^\|+€\|+\|-\.\|-@\|-\^\|-€\|-\|\*\.\|\*@\|\*\^\|\*€\|\*\|/\.\|/@\|/€\|/\|\!\|>\.\|>=\.\|<=\.\|<\.\|>@\|>=@\|<=@\|<@\|>€\|>=€\|<=€\|<€\|>\^\|>=\^\|<=\^\|<\^\|>\|>=\|<=\|<\|=\|€\|%\)"
" Word operators need word boundaries: "o" and "e" would otherwise match inside
" identifiers.
syn match Operator contained "\<\(non\|oppure\|o\|e\|è\|anno\|anni\|mese\|mesi\|giorno\|giorni\)\>"
syn match punctuation contained "\(--\|\;\|\.\|,\|\:\|(\|)\|\[\|\]\|{\|}\)"
syn keyword Type contained intero booleano data durata somma_di_denaro posizione_sorgente decimale

syn region ctxt contained
      \ matchgroup=Keyword start="\<\(contesto\|entrata\|risultato\|interno\)\(|\s\+risultato\)"
      \ matchgroup=sc_id_def end="\s\+\([a-zàèéìíîòóùú][a-zàèéìíîòóùúA-ZÀÈÉÌÍÎÒÓÙÚ0-9_\']*\)\>"

syn region cc_id_dot_sc_id contained contains=punctuation
      \ matchgroup=cc_id start="\<\([A-ZÀÈÉÌÍÎÒÓÙÚ][a-zàèéìíîòóùúA-ZÀÈÉÌÍÎÒÓÙÚ0-9_\']*\)\."rs=e-1
      \ matchgroup=sc_id_def end="\([a-zàèéìíîòóùú][a-zàèéìíîòóùúA-ZÀÈÉÌÍÎÒÓÙÚ0-9_\']*\)\>"

syn region sc_id_def_dot_sc_id contained contains=punctuation
      \ matchgroup=sc_id_def start="\<\([a-zàèéìíîòóùú][a-zàèéìíîòóùúA-ZÀÈÉÌÍÎÒÓÙÚ0-9_\']*\)\."rs=e-1
      \ matchgroup=sc_id end="\([a-zàèéìíîòóùú][a-zàèéìíîòóùúA-ZÀÈÉÌÍÎÒÓÙÚ0-9_\']*\)\>"

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

let b:current_syntax = "catala_it"
