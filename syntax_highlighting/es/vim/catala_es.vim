" Catala Spain-Spanish syntax highlighting.
if exists("b:current_syntax")
  finish
endif

syn match PreProc "^\s*#.*$"
syn match Include "^\s*>\s*Uso.*$"
syn match Identifier "\<[a-záéíóúüñ][a-záéíóúüñA-ZÁÉÍÓÚÜÑ0-9_']*\>"
syn match Type "\<[A-ZÁÉÍÓÚÜÑ][a-záéíóúüñA-ZÁÉÍÓÚÜÑ0-9_']*\>"
syn keyword Keyword ámbito depende de declaración contiene lista de opcional de contenido tipo estructura enumeración contexto regla bajo condición condición consecuencia cumplida igual a definición estado etiqueta excepción cualquiera
syn keyword Statement coincide con patrón pero reemplazando tenemos sea en tal que existe para todos si entonces si no inicialmente entre es máximo mínimo combinar transformar cada a ordenar en orden creciente en orden decreciente y después imposible
syn keyword Boolean verdadero falso
syn keyword Type entero booleano fecha duración dinero decimal posicion_fuente
syn match Comment "#.*$"
syn match Number "|[0-9]\+-[0-9]\+-[0-9]\+|"
syn match Float "\<[0-9]\+,[0-9]*\>"
syn match Operator "\(->\|+\|+-\|-\|\*\|/\|!\|>=\|<=\|>\|<\|=\|%\)"
syn match punctuation "\(--\|;\|\.\|,\|:\|(\|)\|\[\|\]\|{\|}\)"

syn region code transparent matchgroup=Ignore start="```catala" matchgroup=Ignore end="```" contains=ALLBUT,PreProc,Include
syn region metadata transparent matchgroup=Ignore start="```catala-metadata" matchgroup=Ignore end="```" contains=ALLBUT,PreProc,Include

hi link Identifier Function
hi link Type Type
hi link punctuation Ignore
let b:current_syntax = "catala_es"
