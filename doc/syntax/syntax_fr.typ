#import "cheat-sheet.typ"

#import "catala_syntax_hl.typ": setup
#show: setup

#let prog_lit = cheat-sheet.syntax-doc([Programmation littéraire],
```catala-fr
# Titre
### Sous-sous-titre
```, [En-têtes],
```catala-fr
# Article 1 | JORFARTI000012345678
# Article 2 | LEGIARTI000012345678
# Décision 3 | CETATEXT000012345678
```, [Référence au journal officiel], {
show raw: text.with(size: 0.9em)
raw("```catala       ```catala-metadata\n```             ```",
    lang: "catala")
}, [Bloc de code / métadonnées],
```catala-fr
> Module Mdl
```, [Déclaration de module],
```catala-fr
> Usage de Mdl en tant que M
```, [Import de module],
```catala-fr
> Inclusion: foo.catala_en
```, [Inclusion textuelle],
{
    show raw: text.with(size: 0.9em)
    raw("```catala-test-cli\n$ catala interpret --scope Chp1\n```")
}, [Test intégré],
)

#let lit_types = cheat-sheet.syntax-doc([Littéraux et types],
```catala-fr-code
vrai                faux
```,
```catala-fr-code
booléen
```,
```catala-fr-code
65536
```,
```catala-fr-code
entier
```,
```catala-fr-code
65536,262144        37%
```,
```catala-fr-code
décimal
```,
```catala-fr-code
1 234 567,89€
```,
```catala-fr-code
argent
```,
raw(lang: "catala-fr-code", "|"+datetime.today().display()+"|"),
```catala-fr-code
date
```,
```catala-fr-code
254 jour     -4 mois     1 an
```,
```catala-fr-code
durée
```,
```catala-fr-code
Cas1 contenu 12        Cas2
```,
```catala-fr-code
Énum1
```,
```catala-fr-code
Struct1 { -- chp1: 9 -- chp2: 7% }
```,
```catala-fr-code
Struct1
```,
```catala-fr-code
Présent contenu 34€    Absent
```,
```catala-fr-code
optionnel de argent
```,
```catala-fr-code
[ 12; 24; 36 ]
```,
```catala-fr-code
liste de entier
```,
```catala-fr-code
[]
```,
```catala-fr-code
liste de
  n'importe quel
  de type t
```,
```catala-fr-code
(|2012-02-03|, 30€, 1%)
```,
```catala-fr-code
(date,argent,décimal)
```,
```catala-fr-code
f de x, y égal à
  y * x / 12,0€
```,
```catala-fr-code
décimal dépend de
  x contenu argent,
  y contenu décimal
```,
)

#let operators = cheat-sheet.syntax-doc([Opérations],
```catala-fr-code
non a         a et b
a ou b        # "ou à défaut"
a ou bien b   # ou exclusif
```, [Opérateurs logiques],
```catala-fr-code
- a          a + b        a - b
a * b        a / b
```, [Arithmétique],
```catala-fr-code
a = b        a < b        a <= b
a != b       a > b        a >= b
```, [Comparaisons],
```catala-fr-code
décimal de 44
argent de 23,15
```, [Conversions],
```catala-fr-code
arrondi de 9,99€
```, [Arrondis],
```catala-fr-code
accès_année de ...
premier_jour_du_mois de ...
```, [Éléments de dates],
)

#let metadata = cheat-sheet.syntax-doc([Déclaration des métadonnées],
```catala-fr-code
déclaration structure Struct1:
  donnée chp1 contenu entier
  donnée chp2 contenu décimal
```, [Déclaration de structure],
```catala-fr-code
déclaration énumération Énum1:
  -- Cas1 contenu entier
  -- Cas2
```, [Déclaration d'énumération],
```catala-fr-code
## Documetation de Chp1
```, [Texte de documentation],
```catala-fr-code
#[test]
```, [Annot. champ de test],
```catala-fr-code
déclaration champ d'application Chp1:
```, [Déclaration de champ],
```catala-fr-code
  interne var1 contenu entier
```, [Variable de champ],
```catala-fr-code
    état avant
    état après
```, [Transitions d'état],
```catala-fr-code
  interne var2 condition
```, [Condition],
```catala-fr-code
  sub1 champ d'application Chp0
```, [s/s champ],
```catala-fr-code
  résultat var3 contenu ...
  entrée var4 contenu ...
  entrée résultat var5 contenu ...
  contexte var6 contenu ...
  contexte résultat var7 contenu ...
  résultat sub2
    champ d'application Chp0
```, [Qualificateurs d'entrée-sortie],
```catala-fr-code
déclaration const contenu décimal
  égal à 17,1
```, [Définition globale],
```catala-fr-code
déclaration carré contenu décimal
  dépend de x contenu décimal
  égal à x * x
```, [Définition de fonction globale],
)

#let expressions = cheat-sheet.syntax-doc([Expressions],
```catala-fr-code
soit x égal à 36 - 5 dans ...
```, [Définition locale],
```catala-fr-code
selon expr sous forme
-- Cas1 contenu x : ...
-- n'importe quel : ...
```, [Filtrage par motif],
```catala-fr-code
impossible
```, [Calcul inaccessible],
```catala-fr-code
#[debug.print = "message"] expr
```, [Annot. de débug],
```catala-fr-code
expr sous forme Cas1
expr sous forme Cas1 contenu x
     et x >= 2
```, [Test de filtrage avec variable optionnelle],
```catala-fr-code
struc1 mais en remplaçant
  { -- chp2: 8% }
```, [Remplacement de champs],
```catala-fr-code
struc1.chp2         tuple1.2
sub1.var0
```, [Champ, élément de \ n-uplet, s/s-variable],
```catala-fr-code
f de 44,50€, 1/3
```, [Appel de fonction],
```catala-fr-code
résultat de Chp1
  avec { -- chp1: 9 -- chp2: 15% }
```, [Appel direct de champ d'application],
```catala-fr-code
si ... alors ... sinon ...
```, [Branchement],
```catala-fr-code
var1 état avant
```, [Accès à un état]
)

#let scope = cheat-sheet.syntax-doc([Définition de champ d'application],
```catala-fr-code
champ d'application Chp1: ...
```, [Définition des membres],
```catala-fr-code
champ d'application Chp1
  sous condition var1 >= 2: ...
```, [Avec condition générale],
```catala-fr-code
définition var1 égal à ...
```, [Déf. inconditionnelle],
```catala-fr-code
définition var1
  sous condition ...
  conséquence égal à ...
```, [Définition conditionnelle],
```catala-fr-code
règle var2
  sous condition var1 >= 2
  conséquence ·non· rempli
```, [Règle\ (définition de condition)],
```catala-fr-code
définition f de x, y égal à ...
```, [Déf./règle fonction],
```catala-fr-code
étiquette étq1 définition var1 ...
```, [Déf./règle étiquetée],
```catala-fr-code
exception étq1 définition var1 ...
```, [Exc. à déf. étiquetée],
```catala-fr-code
exception définition var1 ...
```, [Exception à implicite],
```catala-fr-code
définition var1 état avant
  égal à ...
```, [Définition d'états],
```catala-fr-code
assertion ...
```, [Assertion],
```catala-fr-code
date arrondi inf·supérieur
```, [Mode arrondi dates]
)

#let lists = cheat-sheet.syntax-doc([Opérations sur les listes],
```catala-fr-code
lst contient 3
```, [Test de présence],
```catala-fr-code
existe x parmi lst tel que x > 2
```, [Test d'existence],
```catala-fr-code
pour tout x parmi lst on a x > 2
```, [Test pour tout],
```catala-fr-code
transforme chaque x parmi lst
  en x - 2
```, [Application un-à-un],
```catala-fr-code
liste de x parmi lst tel que x > 2
```, [Filtrage],
```catala-fr-code
transforme chaque x parmi lst
  tel que x > 2 en x - 2
```, [Filtrage + application],
```catala-fr-code
transforme chaque (x, y)
  parmi (lst1, lst2) en x + y
```, [Application multiple],
```catala-fr-code
lst1 ++ lst2
```, [Réunion],
```catala-fr-code
somme entier de lst
```, [Agrégation],
```catala-fr-code
nombre de lst
```, [Comptage],
```catala-fr-code
maximum de lst
  ou si liste vide alors -1
```, [Extremum\ (optionnel: défaut)],
```catala-fr-code
contenu de x parmi lst
  tel que x * x est minimum
  ou si liste vide alors -1
```, [Élément selon extremum\ (optionnel: défaut)],
```catala-fr-code
combine tout x parmi lst
  dans acc initialement 0
  avec acc + x
```, [Accumulation]
)

#cheat-sheet.layout(
    [La syntaxe de Catala], [_français_],
    (prog_lit, lit_types, operators),
    (metadata, expressions),
    (scope, lists)
)
