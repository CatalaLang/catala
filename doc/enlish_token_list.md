# Catala english token proposals

## FIELD

Current proposal: "application field".

The logical context in which the text of the law will apply. Examples are
"computation of family benefits", "determination of child credit eligibility".

```
application field FamilyBenefitsComputation:
  ...
```

## DATA

Current proposal: "data".

When declaring a data structure, introduces a data field :

```
structure Foo:
  data bar ...
```

## CONTENT

Current proposal: "content".

Introduces type annotation for data.


```
data bar content boolean
```

## DEPENDS

Current proposal : "depends"

Introduces a dependency on a parameter for a piece of data when declaring
a structure.

```
  data bar depends on Foo
```

## DECLARATION

Current proposal : "declaration"

Declares a structure or a enumeration in the metadata section.

## INCLUDES

Current proposal "includes"

A FIELD can include another FIELD, building up code modularity which is also
present in statutes.

```
declaration application field FamilyBenefitsComputation:
   ...
includes application field HouseholdIncomeComputation
```

## CONTEXT

Current proposal : "context"

A FIELD has several parameters, which are the data in the logical context of the
FIELD. CONTEXT introduces these data.

```
declaration application field FamilyBenefitsComputation:
   context foo content FOO
```

Also introduces context matching when including another field:

```
  ...
includes application field HouseholdIncomeComputation:
   HouseholdIncomeComputation.household = FamilyBenefitsComputation.household
   ...
```

## VARIES_WITH DECREASING INCREASING

Current proposal : "varies with" "decreasing" "increasing"

Way in which a numeric function can vary.

```
foo varies with bar decreasing
```

## OF

Current proposal: "of"

Used as function application token with a single parameter (foo of bar)

```
income of household
```

## COLLECTION

Current proposal: "collection"

Annotation for data that are collection of other data.

```
data children content collection Child
```

## ENUM

Current proposal : "enumeration".

An enumeration is a sum type modelling an alternative.

```
declaration enumeration Foo:
  -- ChoiceA
  -- ChoiceB content Bar
```

## STRUCT

Current proposal: "structure".

Introduces a structure data type (product type).

```
declaration structure Foo:
  fieldA content integer
  fieldB content boolean
  ...
```

## INTEGER

Current proposal : "integer".

The base integer data type.

```
data foo content integer
```

## MONEY

Current proposal : "amount".

The base data type for representing amounts of money.

```
data foo content money
```

## TEXT

Current proposal : "text".

The base data type for representing strings.

```
data foo content text
```

## DECIMAL

Current proposal : "decimal".

The base data type for representing fixed- or floating-point numbers.

```
data foo content decimal
```

## DATE

Current proposal : "date".

The base data type for representing dates.

```
data foo content date
```

## BOOLEAN

Current proposal : "boolean".

The base data type for representing booleans. Apparently lawyers don't know what
it is but I haven't found a better word.

```
data foo content boolean
```

## SUM

Current proposal: "sum".

Function for doing the sum of a quantity over a collection of data.

```
sum for child in children of child.income
```

## CARDINAL

Current proposal: "number".

Function for counting the number of data in a collection that satisfy a predicate.

```
number for child in children of child.age > 15 year
```

## CONDITION

Current proposal: "condition"

A condition is a special kind of DATA used to represent logical and juridical
conditions. In default logic, it is a boolean whose default value is false.

```
structure ArticleFoo:
  condition eligible_for_credit
```

## FILLED

Current proposal: "fulfilled"

Action TOKEN to declare a CONDITION valid in a rule.

```
eligible_for_credit fulfilled
```

## RULE

Current proposal: "rule"

Inside a FIELD, introduces a rules that sets a CONDITION to FILLED or not FILLED

```
application field FamilyBenefitsComputation:
  rule under condition
    number of household.children > 2
  consequence eligible filled
```

## DEFINITION

Current proposal: "definition".

Same thing as RULE but for data that is not a condition.

```
application field FamilyBenefitsComputation:
  definition income defined as
    sum for child in household.children of child.income
```

## DEFINED_AS

Current proposal: "defined as".

Introduces the expression of a data in a DEFINITION.

See above for example.

## MATCH

Current proposal: "match".

Allows to case on an enumeration.

```
match matrimonial_status with pattern
-- Married
-- Single
```

## WITH

Current proposal: "with pattern".

Complement of MATCH. See above.

## UNDER_CONDITION

Current proposa: "under condition".

Soft if-then-else for default logic.

```
definition credit
  under condition eligible_for_credit
  consequence defined as $10
```

## CONSEQUENCE

Current proposal: "consequence"

Counterpart of UNDER_CONDITION. See above.

## IF THEN ELSE

Current proposal: "if" "then" "else"

The hard if/then/else of traditional programming.

```
if child.age > 15 then Eligible else NotEligible
```

## OPTIONAL

Current proposal: "optional".

Syntax shorthand for a Absent/Present enumeration.

```
data husband optional
```

## ASSERTION

Current proposal: "assertion".

Introduces inside a FIELD a thing that should always be true.

```
assertion for each child in children we have child.age >= 0
```

## FOR ALL WE_HAVE

Current proposal: "for" "all" "we have"

Introduces a forall predicate or aggregation over a collection of data.

See above.

### EXISTS IN SUCH THAT

Current proposal: "exists" "in" "such" "that"

Introduces an existential predicate over a collection of data.

```
exists child in children such that child.age > 20 year
```

## FIXED BY

Current proposal: "fixed" "by"

Asserts that a particular variable is defined in a particular category of
legislative text.

```
assertion foo.bar fixed by executive order
```

## NOW

Current proposal "now".

Builtin function returning the date of today when executing the code.

## AND OR NOT

Current proposal: "and" "or" "not".

Logical connectives.

```
(age > 15) and (eligible or special_thing)
```

### YEAR MONTH DAY

Current proposal: "year" "month" "day"

Units of time for duration specification.

```
15 months + 1 year
```
