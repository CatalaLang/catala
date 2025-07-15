#set page(paper: "a4", flipped: true, margin: 1cm)
#set text(font: "Inter 18pt", size: 7pt)
#show raw: text.with(font: "Annotation Mono", size: 7pt)

#import "catala_syntax_hl.typ": setup
#show: setup

#show heading: text.with(size: 9pt)

#place(top+left, image("logo.svg", width: 30pt))

#place(top+right)[v0.10.0 · Revision \#3 · ⓒ 2025]

#box(height:30pt, width:100%, align(horizon+center)[
#upper(text(size:15pt)[The Catala Syntax])
#h(30pt)
#text(size: 9pt)[english version]
])


#v(1em)

#let syntax-doc(title, ..args) = [
  #let lines = args.pos().chunks(2).map(x => (x.at(0), text(style:"oblique",x.at(1))))
  = #title
  #v(0.9em)
  #grid(columns: (65%, 35%),
      row-gutter: 0.8em,
      ..lines.flatten())
]

#let prog_lit = syntax-doc([Literate programming],
```catala-en
# Title
### Sub-subtitle
```, [Heading],
raw("```catala      ```catala-metadata\n```            ```"),
[Code / metadata block],
```catala-en
> Module Mdl
```, [Module declaration],
```catala-en
> Using Mdl as M
```, [Module import+alias],
```catala-en
> Include: foo.catala_en,
```, [File inclusion]
)

#let lit_types = syntax-doc([Literals and types],
```catala-en-code
true                false
```,
```catala-en-code
boolean
```,
```catala-en-code
65,536
```,
```catala-en-code
integer
```,
```catala-en-code
65536.262144        37%
```,
```catala-en-code
decimal
```,
```catala-en-code
$1,234,567.89
```,
```catala-en-code
money
```,
```catala-en-code
|2024-04-01|
```,
```catala-en-code
date
```,
```catala-en-code
254 day      -4 month      1 year
```,
```catala-en-code
duration
```,
```catala-en-code
[ 12; 24; 36 ]
```,
```catala-en-code
list of integer
```,
```catala-en-code
(|2024-04-01|, $30, 1%)
```,
```catala-en-code
(date,money,decimal)
```,
```catala-en-code
f of x, y equals
  y * x / $12.0
```,
```catala-en-code
decimal depends on
  x content money,
  y content decimal
```,
```catala-en-code
Struct1 { -- fld1: 9 -- fld2: 7% }
```,
```catala-en-code
Struct1
```,
```catala-en-code
Case1 content 12        Case2
```,
```catala-en-code
Enum1
```
)

#let operators = syntax-doc([Operators and built-ins],

```catala-en-code
not a        a and b
a or b       # "or otherwise"
a xor b      # exclusive or
```, [Logical operators],
```catala-en-code
- a          a + b        a - b
a * b        a / b
```, [Arithmetic],
```catala-en-code
a = b        a < b        a <= b
a != b       a > b        a >= b
```, [Comparison],
```catala-en-code
decimal of 44
money of 23.15
```, [Conversions],
```catala-en-code
round of $9.99
```, [Rounding],
```catala-en-code
get_month of ...
first_day_of_month of ...
```, [Date parts],
```catala-en-code
a +! b   a +. b   a +$ b   a +^ b
# int.   decimal  money    duration
```, [Explicitly typed operators]
)

#let metadata = syntax-doc([Metadata declaration],
```catala-en-code
declaration structure Struct1:
  data fld1 content integer
  data fld2 content decimal
```, [Structure declaration],
```catala-en-code
declaration enumeration Enum1:
  -- Case1 content integer
  -- Case2
```, [Enumeration declaration],
```catala-en-code
declaration scope Scope1:
  internal var1 content integer
  internal var2 condition
  sub1 scope Scope0
```, [Scope declaration],
```catala-en-code
  internal var1 content ...
  output var3 content ...
  input var4 content ...
  input output var5 content ...
  context var6 content ...
  context output var7 content ...
  output sub2 scope Scope0
```, [Input-output qualifiers],
```catala-en-code
  internal var1 content ...
    state before
    state after
```, [State transitions declaration],
```catala-en-code
declaration const content decimal
  equals 17.1
```, [Global definition],
```catala-en-code
declaration square content decimal
  depends on x content decimal
  equals x * x
```, [Global function definition],
)

#let expressions = syntax-doc([Expressions],
```catala-en-code
let x equals 36 - 5 in ...
```, [Local definition],
```catala-en-code
match expr with pattern
-- Case1 of x : ...
-- Case2 : ...
-- anything : ...
```, [Pattern matching],
```catala-en-code
expr with pattern Case1
expr with pattern Case1 of x
     and x >= 2
```, [Pattern test\  and optional binding],
```catala-en-code
struc1 but replace { -- fld2: 8% }
```, [Field replacement],
```catala-en-code
struc1.fld2         tuple1.2
sub1.var0
```, [Field, tuple element, subscope variable],
```catala-en-code
f of $44.50, 1/3
```, [Function call],
```catala-en-code
output of Scope1 with
  { -- fld1: 9 -- fld2: 15% }
```, [Direct scope call],
```catala-en-code
if ... then ... else ...
```, [Conditional],
```catala-en-code
var1 state before
```, [Variable state access]
)

#let scope = syntax-doc([Scope definition],
```catala-en-code
scope Scope1: ...
```, [Scope use],
```catala-en-code
scope Scope1
  under condition var1 >= 2: ...
```, [Use-wide condition],
```catala-en-code
definition var1 equals ...
```, [Unconditional def.],
```catala-en-code
definition var1
  under condition ...
  consequence equals ...
```, [Conditional definition],
```catala-en-code
rule var2
  under condition var1 >= 2
  consequence ·not· fulfilled
```, [Rule (definition for conditions)],
```catala-en-code
definition f of x, y equals ...
```, [Function def. or rule],
```catala-en-code
label lbl1 definition var1 ...
```, [Labeled def. or rule],
```catala-en-code
exception lbl1 definition var1 ...
```, [Exception to label],
```catala-en-code
exception definition var1 ...
```, [Exception to implicit],
```catala-en-code
definition var1 state before
  equals ...
```, [State definition],
```catala-en-code
assertion ...
```, [Assertion],
```catala-en-code
date round in·decreasing
```, [Date rounding mode]
)

#let lists = syntax-doc([List operations],
```catala-en-code
lst contains 3
```, [Presence test],
```catala-en-code
exists x among lst such that x > 2
```, [Existence test],
```catala-en-code
for all x among lst we have x > 2
```, [For all test],
```catala-en-code
map each x among lst to x + 2
```, [Mapping],
```catala-en-code
list of x among lst such that x > 2
```, [Filter],
```catala-en-code
map each x among lst such that x > 2
  to x - 2
```, [Filter + map],
```catala-en-code
map each (x, y) among (lst1, lst2)
  to x + y
```, [Multiple mapping],
```catala-en-code
lst1 ++ lst2
```, [Merge],
```catala-en-code
sum integer of lst
```, [Aggregation],
```catala-en-code
number of lst
```, [Count],
```catala-en-code
maximum of lst
  or if list empty then -1
```, [Extremum\ (optional default)],
```catala-en-code
content of x among lst
  such that x * x is minimum
  or if list empty then -1
```, [Arg-extremum\ \ (optional default)],
```catala-en-code
combine all x among lst
  in acc initially 0
  with acc + x
```, [Folding]
)

#grid(
    columns: (1fr, 1fr, 1fr),
    gutter: 0pt,
    stroke: (x, y) => if x > 0 { (left: 0.2pt + black) },
    inset: (x, y) => if x > 0 { (left: 6pt) } + if x < 2 { (right: 6pt) },
    [ #prog_lit #v(1fr) #lit_types #v(1fr) #operators ],
    grid.vline(),
    [ #metadata #v(1fr) #expressions ],
    grid.vline(),
    [ #scope #v(1fr) #lists ]
)
