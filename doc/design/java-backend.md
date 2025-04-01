# Java backend design

The objective is to introduce a new backend to Java. In this
document, we document the main design choices.

The goal is to generate pure Java files that are human-readable - this
property can be also be reached later on if it's too constraining. We
also want the possibility to output a jar but that shouldn't be the
main target of this work. As such, we want to be the least restrictive
as possible. Both in terms of Java version as external dependencies or
building tools. One is expected to be generate a bunch of files and
integrate the source files in their project with little friction.

We also want to generate a `maven` configuration to the generated
files.
TODO: determine is `maven` is the best target candidate

## Runtime

## Multithreading

For the sake of concurrency, the Catala java runtime will be an
instance of an object that contains its state (e.g., log stack) so
that it ensures reentrancy. Scope executions will
remain sequential.

### Dates

It is still unclear what dates_calc's dates will compile to in
Java. We need to explore the stdlib to determine if there is a good
candidate to fill that slot.

### Arbitrary precision numbers

It is probably easier to use Java's arbitrary precision classes (i.e.,
BigInteger, BigFraction).

Cf. https://docs.oracle.com/javase/8/docs/api/java/math/BigInteger.html
    https://commons.apache.org/proper/commons-numbers/commons-numbers-docs/apidocs/org/apache/commons/numbers/fraction/BigFraction.html
    (^^ not in stdlib !! ^^)

- Catala integers should be BigInteger
- Money could be BigInteger like in the other backends, but BigDecimal would
  actually be a better fit
- There doesn't appear to be built-in support for rationals, we may may need
  Apache Commons' `BigFraction` class, that can probably be vendored.

## Compilation schemas

### Modules

Module are to be compiled as a package containing its necessary classes.

https://www.w3schools.com/java/java_packages.asp

It means that a module declaration will yield a package by creating a
new directory (named after this package). And, each generated class
will have to begin with the `package module_name;` declaration.

Note: should we allow users to configure a root package? (e.g. `fr.cnaf.` ?

Module usages will be translated as: `import path.package_name.*;`

https://www.w3schools.com/java/java_packages.asp

Note: if needs be, we can be a little "dirty" to begin with and add
`import *` declaration in every inner generated module file. We can
clean un-usued import later on so that Java linters are happy.

### Non-modules

Non-module Catala files can still be generated as a module.

### Scopes

Scopes are to be compiled as a single java file named after this
scope. The computation would be triggered during object construction
which could throw an exception if needs be.

E.g.,
```catala-metadata
declaration scope S:
  input x content integer
  internal y content decimal
  output z content money
```
would yield a `S.java` file such as
```java
class S {

  CatalaMoney z;

  S(CatalaInteger x) // throws
  {
    // body
  }
}
```

```java
// Then used as:
S scope_s_result = new S(new CatalaInteger(3));
... scope_s_result.z ...
```

For visibility, we can use `private` for internal variables and
constructors/methods (when the scope is not exposed). `protected` for
non-exposed global functions/structures and public otherwise.

Scopes that are meant to be executed (no input scopes) could also have
a `main` attached.


### Global declarations

Global declarations can be bundled together in a `Common.java` file as
static methods. Their visibility depends on Catala's one (`public` if
its declaration is part of a `catala-metadata` or `protected` otherwise)

### Structures

Immutable records were introduced in Java 14 (March 2020)
(https://www.baeldung.com/java-record-keyword). This seems to be a
good candidate to compile structures to. Otherwise, we can generate
classes with `final` parameters.

### Enumerations

Java enumerations cannot contain arbitrary data. Hence, we need to
define a specific (static) class for it.

It may be something like:
```catala
declaration enumeration E:
  -- A
  -- B content money
```
```java
class E {
    enum Kind { A, B };

    final Kind kind;
    final CatalaVal contents;

    static E makeA() { E e = new E(); e.kind = A; return e; }

    static E makeB(CatalaMoney i){
        E e = new E();
        e.kind = B;
        e.contents = i; // <= subtype of CatalaVal
        return e;
    }
}
```

Note: we could investigate sealed abstract classes and pattern matching to get type safety and compile-time exhaustiveness checking. But this might be overkill as we get those guarantees from the compiler?

e.g in this case we might have something like this (LLM-generated, sorry)

```java
sealed abstract class Reimbursement permits NoReimbursement, MoneyReimbursement {
    // Common methods could go here if needed
}

final class NoReimbursement extends Reimbursement {
    // Singleton pattern
    private static final NoReimbursement INSTANCE = new NoReimbursement();

    private NoReimbursement() {}

    public static NoReimbursement getInstance() {
        return INSTANCE;
    }

    @Override
    public String toString() {
        return "NoReimbursement";
    }
}

final class MoneyReimbursement extends Reimbursement {
    private final int amount;

    public MoneyReimbursement(int amount) {
        this.amount = amount;
    }

    public int getAmount() {
        return amount;
    }

    @Override
    public String toString() {
        return "MoneyReimbursement(" + amount + ")";
    }
}

// Creating instances
Reimbursement none = NoReimbursement.getInstance();
Reimbursement money = new MoneyReimbursement(500);

// Using switch expressions with pattern matching
String description = switch (reimbursement) {
    case NoReimbursement n -> "No reimbursement provided";
    case MoneyReimbursement m -> "Reimbursement amount: $" + m.getAmount();
};

// Processing with switch
void processReimbursement(Reimbursement r) {
    switch (r) {
        case NoReimbursement n -> System.out.println("No action needed");
        case MoneyReimbursement m -> System.out.println("Processing payment of $" + m.getAmount());
    };
}
```

The main benefit to this approach might be that it could guide users to write cleaner code at the borders/interfaces of the system (when supplying data for catala computation, or when implementing external modules in java?)

##### New iteration

Depending on Java 21 might be too constraining for people depending on
their IT dpt to install new jdks, thus we currently on 17 with this
compilation pattern:

```catala
declaration enumeration E:
  -- Case1 content integer
  -- Case2
  -- Case3 content integer, integer
```

```java
public class E {

    public enum Kind {
        Case1, Case2, Case3
    };

    private final CatalaValue contents;
    public final Kind kind;

    private E(Kind k, CatalaValue contents) {
        this.kind = k;
        this.contents = contents;
    }

    public static E makeCase1(CatalaInteger i) {
        E o = new E(Kind.Case1, i);
        return o;
    }

    public static E makeCase2() {
        E o = new E(Kind.Case2, CatalaUnit.INSTANCE);
        return o;
    }

    public static E makeCase3(CatalaValue tup) {
        E o = new E(Kind.Case3, tup);
        return o;
    }

    public <T> T getContentsAs(E.Kind k, Class<T> clazz) {
        if (this.kind != k) {
            throw new CatalaException("Invalid enum contents access: expected " + k + ", got " + this.kind);
        }
        return (T) this.contents;
    }

    public CatalaInteger getCase1Contents() {
        return this.getContentsAs(Kind.Case1, CatalaInteger.class);
    }

    public CatalaTuple getCase3Contents() {
        return this.getContentsAs(Kind.Case3, CatalaTuple.class);
    }
}
```

### Tuples

Two options are possible:
we can introduce a bunch of tuples-like classes in the runtime:
```java
class Pair<A, B> {
    private final A e1;
    private final B e1;

    public Tuple(A e1, B e1) {
        this.e1 = e1;
        this.e2 = e2;
    }

    public A getE1() { return e1; }
    public B getE2() { return e2; }
```
*or*
we can compile to static arrays:
```java
CatalaVal[] tupl = { new CatalaMoney(3), a_variable, new CatalaList(1,2,3) };
```
with dynamic accesses.

The first approach adds some type-safety and is cleaner. The second
approach is more concise and more "hackable" if we need to locally
optimize (e.g., in list operations).

### Lambdas

Lambdas are part of Java since 2014, it should be straightforward to
compile Catala lambdas to Java's ones but we need proper digging to
determine if there are limitations.

## Experimenting

To view scalc: `catala scalc ~/catala/tests/enum/good/<catala_file> -O`
To compile to Java: `catala java ~/catala/tests/enum/good/<catala_file> -O -o <output_dir> --debug`
To compile & view the files: `catala java <path_to_catala_file>/sub_scope.catala_en --debug -O && tail -n +1 <output_dir|OR|path_to_catala_file>/src/main/java/sub_scope/*`
