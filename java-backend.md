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
that it ensures reentrancy. Initially (?), scope executions will
remain sequentials.

### Dates

It is still unclear what dates_calc's dates will compile to in
Java. We need to explore the stdlib to determine if there is a good
candidate to fill that slot.

### Arbitrary precision numbers

It is probably easier to use Java's arbitrary precision classes (i.e.,
BigInteger, BigDecimal).

Cf. https://docs.oracle.com/javase/8/docs/api/java/math/BigInteger.html

## Compilation schemas

### Modules

Module are to be compiled as a package containing its necessary classes.

https://www.w3schools.com/java/java_packages.asp

It means that a module declaration will yield a package by creating a
new directory (named after this package). And, each generated class
will have to begin with the `package module_name;` declaration.

Module usages will be translated as: `import path.package_name.*;`

https://www.w3schools.com/java/java_packages.asp

Note: if needs be, we can be a little "dirty" to begin with and add
`import *` declaration in every inner generated module file. We can
clean un-usued import later on so that Java linters are happy.

### Non-modules

Non-module Catala files can still be generated as a module.

### Scopes

Scopes are to be compiled as a single java file named after this
scope. Additionally, we can make them implement the `Function` Java
interface with their input and ouput as internal public records (structures).

E.g.,
```catala-metadata
declaration scope S:
  input x content integer
  internal y content float
  output z content money
```
would yield a `S.java` file such as
```java
class S implements Function<S.S_in, S.S_out>{

  private BigDecimal y;

  public record S_in(CatalaInteger x);
  public record S_out(CatalaMoney m);

  @Override
  public F_out apply(F_in t) {
    ...
  }
}
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
