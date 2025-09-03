#import "cheat-sheet.typ"

#import "catala_syntax_hl.typ": setup
#show: setup

#let prog_lit = cheat-sheet.syntax-doc([Programare literată],
```catala-ro
# Titlu
### Sub-subtitlu
```, [Titlu],
raw("```catala      ```catala-metadata\n```            ```"),
[Bloc cod / metadata],
```catala-ro
> Modul Mdl
```, [Declarație modul],
```catala-ro
> Folosind Mdl ca M
```, [Import+alias modul],
```catala-ro
> Include: foo.catala_ro,
```, [Includere fișier],
{
    show raw: text.with(size: 0.9em)
    raw("```catala-test-cli\n$ catala interpret --scope Scop1\n```")
}, [Test interfață linie comandă],
)

#let lit_types = cheat-sheet.syntax-doc([Literali și tipuri],
```catala-ro-code
adevărat            fals
```,
```catala-ro-code
boolean
```,
```catala-ro-code
65.536
```,
```catala-ro-code
întreg
```,
```catala-ro-code
65536,262144        37%
```,
```catala-ro-code
zecimal
```,
```catala-ro-code
1.234.567,89 RON
```,
```catala-ro-code
bani
```,
```catala-ro-code
|2024-04-01|
```,
```catala-ro-code
dată
```,
```catala-ro-code
254 zi              3 lună
```,
```catala-ro-code
durată
```,
```catala-ro-code
"Bună ziua!"
```,
```catala-ro-code
text
```,
)

#let stmt_struct = cheat-sheet.syntax-doc([Instrucțiuni și structuri],
```catala-ro-code
fie x egal 36 + 5 în ...
```, [Legare locală],
```catala-ro-code
potrivește expr cu model
-- Cons1: ...
-- Cons2 de x: ...
-- orice: ...
```, [Potrivire model],
```catala-ro-code
expr cu Model1
dar înlocuiește { 
  -- fld: val; 
  -- fld1: val1 
}
```, [Actualizare struct],
```catala-ro-code
dacă expr 
atunci expr 
altfel expr
```, [Condiție],
```catala-ro-code
nume_structură {
  -- câmp1: val1
  -- câmp2: val2
}
```, [Constructor struct],
```catala-ro-code
Nume_enumerare conținut valoare
```, [Injecție enumerare],
)

#let collections = cheat-sheet.syntax-doc([Colecții],
```catala-ro-code
[ 12; 24; 36 ]
```, [Liste (literal)],
```catala-ro-code
x printre [ 1; 2; 3 ]
```, [Apartenență la listă],
```catala-ro-code
listă goală
```, [Test listă goală],
```catala-ro-code
aplică fiecare f la list
```, [Funcția `map`],
```catala-ro-code
combină f cu e inițial pentru list
```, [Fold: `f(f(f(e,x1),x2),x3)`],
```catala-ro-code
listă de întreg
```, [Tip listă],
```catala-ro-code
număr de elems
```, [Cardinal (dimensiune listă)],
```catala-ro-code
sumă întreg de elems
```, [Suma elementelor din listă],
```catala-ro-code
maxim de coll 
  sau dacă lista este goală -1
```, [Maxim, cu valoare implicită],
```catala-ro-code
minim de ...
```, [Minim],
```catala-ro-code
arg_maxim de f printre elems
```, [Cel mai mare argument],
```catala-ro-code
arg_minim de ...
```, [Cel mai mic argument],
)

// Continuarea ar fi similară pentru restul elementelor de sintaxă...

#cheat-sheet.cheat-sheet(none, prog_lit, lit_types, stmt_struct, collections)