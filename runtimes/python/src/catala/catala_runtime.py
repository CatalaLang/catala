"""
.. module:: catala_runtime
   :platform: Unix, Windows
   :synopsis: The Python bindings for the functions used in the generated Catala code
   :noindex:

.. moduleauthor:: Denis Merigoux <denis.merigoux@inria.fr>
"""
from __future__ import annotations # 'ClsType' ~> ClsType annotations

# This file should be in sync with compiler/runtime.{ml, mli} !

import math
from fractions import Fraction
import dates
from typing import NewType, List, Generic, Callable, Tuple, TypeVar, Iterable, Union, Any, overload, override, ClassVar
from functools import reduce
from enum import Enum, IntEnum, nonmember, auto
import copy

Alpha = TypeVar('Alpha', bound='Value')
Beta = TypeVar('Beta', bound='Value')
Gamma = TypeVar('Gamma', bound='Value')

# ================================
# Base class for all Catala values
# ================================

class Value:
    __slots__ = ()

    # Catala values are immutable
    def __setattr__(self, *ignored):
        raise NotImplementedError

    def __new__(cls, *elts, **fields):
        if elts != ():
            return super().__new__(cls, *elts, **fields)
        obj = super().__new__(cls)
        for k, v in fields.items():
            object.__setattr__(obj, k, v)
        return obj

    # Defaults expected to be overriden
    def __truediv__(self, other: Value, pos: SourcePosition | None = None):
        if other == 0: raise DivisionByZero(pos)
        else: return self.__class__(super().__truediv__(other)) #type:ignore[misc]

    def __eq__(self, other: Value, pos: SourcePosition | None = None) -> Bool: #type:ignore[override]
        return Bool(super().__eq__(other))

    def compare(self: Value, other: Value, pos: SourcePosition | None = None) -> int:
        if type(self) is not type(other):
            raise UncomparableValues(pos)
        return super().__gt__(other) - super().__lt__(other) #type:ignore[misc]

    def __str__(self, indent: int = 0) -> str:
        return super().__str__()

    def __hash__(self) -> int:
        return super().__hash__()

    # ensure typing of inherited common operators
    def __add__(self, other):
        return self.__class__(super().__add__(other))
    def __sub__(self, other):
        return self.__class__(super().__sub__(other))
    def __mul__(self, other):
        return self.__class__(super().__mul__(other))
    def __neg__(self):
        return self.__class__(super().__neg__())

    # deduced methods
    def __ne__(self, other: Value, pos: SourcePosition | None = None) -> Bool: #type:ignore[override]
        return Bool(not(self.__eq__(other, pos)))

    def __lt__(self, other: Value, pos: SourcePosition | None = None) -> Bool: #type:ignore[override]
        return Bool(self.compare(other, pos) < 0)

    def __le__(self, other: Value, pos: SourcePosition | None = None) -> Bool: #type:ignore[override]
        return Bool(self.compare(other, pos) <= 0)

    def __gt__(self, other: Value, pos: SourcePosition | None = None) -> Bool: #type:ignore[override]
        return Bool(self.compare(other, pos) > 0)

    def __ge__(self, other: Value, pos: SourcePosition | None = None) -> Bool: #type:ignore[override]
        return Bool(self.compare(other, pos) >= 0)

# ==========
# Exceptions
# ==========

class SourcePosition(Value):
    __slots__ = ( 'filename', 'start_line', 'start_column', 'end_line', 'end_column', 'law_headings' )
    filename: str
    start_line: int
    end_line: int
    start_column: int
    end_column: int

    def __new__(cls,
                filename: str,
                start_line: int,
                start_column: int,
                end_line: int,
                end_column: int,
                law_headings: List[str]):
        return super().__new__(cls,
                               filename=filename,
                               start_line=start_line,
                               start_column=start_column,
                               end_line=end_line,
                               end_column=end_column,
                               law_headings=law_headings)

    def __str__(self, indent: int = 0) -> str:
        return "{}:{}.{}-{}.{}".format(
            self.filename,
            self.start_line, self.start_column,
            self.end_line, self.end_column)

    def __eq__(self, other, pos = None):
        return Bool(isinstance(other, SourcePosition) and
                    self.filename == other.filename and
                    self.start_line == other.start_line and
                    self.start_column == other.start_column and
                    self.end_line == other.end_line and
                    self.end_column == other.end_column)

class CatalaError(Exception):
    __slots__ = ( 'message', 'source_positions', 'note' )

    message: str
    source_positions: List[SourcePosition]
    note: str | None

    def __init__(self, source_positions: None | SourcePosition | List[SourcePosition], note: str | None = None) -> None:
        if source_positions is None:
            self.source_positions = []
        elif isinstance(source_positions, List):
            self.source_positions = source_positions
        else:
            self.source_positions = [source_positions]
        self.note = note
    # Prints in the same format as the OCaml runtime
    def __str__(self) -> str:
        return "\n\x1b[1;31m[ERROR]\x1b[m At {}: {}{}".format(
            ', '.join([str(e) for e in self.source_positions]),
            self.message,
            "" if self.note is None else (". " + self.note))

class AssertionFailed(CatalaError):
    message = "this assertion doesn't hold"

class NoValue(CatalaError):
    message = "no computation with valid conditions found"

class Conflict(CatalaError):
    message = "two or more concurring valid computations"

class DivisionByZero(CatalaError):
    message = "division by zero"

class ListEmpty(CatalaError):
    message = "the list was empty"

class NotSameLength(CatalaError):
    message = "traversing multiple lists of different lengths"

class UncomparableValues(CatalaError):
    message = "comparison of values of these types is not supported"

class DateError(CatalaError):
    message = "Date error"

class Impossible(CatalaError):
    message = "\"impossible\" computation reached"

# =============================
# Types and value introspection
# =============================

def compare(a, b) -> int:
    return (a > b) - (a < b)

class Array[T: Value](Value, tuple):  #type:ignore[misc]
    def __new__(cls, value: object):
        return super().__new__(cls, value)

    def __len__(self):
        return super().__len__()

    def __str__(self, indent: int = 0) -> str:
        if len(self) == 0:
            return '[]'
        else:
            return "[%s%s]" % (
                ''.join([f"\n{'':>{indent + 2}}" + x.__str__(indent + 2) + ";"
                         for x in self]),
                f"\n{'':>{indent}}"
            )

    def __eq__(self, other, pos = None):
        try:
            return Bool(all([x.__eq__(y, pos) for x, y in zip(self, other, strict=True)]))
        except ValueError: return false

    def compare(self, other: Value, pos: SourcePosition | None = None):
        if not isinstance(other, Array):
            raise UncomparableValues(pos)
        for (x, y) in zip(self, other, strict=False):
            cmp = x.compare (y, pos)
            if cmp != 0: return cmp
        return compare (len(self), len(other))

    def fold_left(self: Array[Beta], f: Callable[[Alpha, Beta], Alpha], init: Alpha) -> Alpha:
        return reduce(f, self, init)

    def filter(self: Array[Alpha], f: Callable[[Alpha], bool]) -> Array[Alpha]:
        return Array([i for i in self if f(i)])

    def map(self: Array[Alpha], f: Callable[[Alpha], Beta]) -> Array[Beta]:
        return Array([f(i) for i in self])

    def map2(self: Array[Alpha], pos: SourcePosition, f: Callable[[Alpha, Beta], Gamma], l2: Array[Beta]) -> Array[Gamma]:
        try:
          zipped = zip(self, l2, strict=True)
        except ValueError: raise NotSameLength(pos)
        return Array([f(i, j) for i, j in zipped])

    def reduce(self: Array[Alpha], f: Callable[[Alpha, Alpha], Alpha], dft: Callable[[Unit], Alpha]) -> Alpha:
        if len(self) == 0:
            return dft(Unit())
        else:
            return reduce(f, self)

    def length(self: Array[Alpha]) -> Integer:
        return Integer(len(self))


class CatalaTuple[*T](Value):
    __slots__ = ( 'value' )
    value: tuple[Value]

    def __new__(cls, *members) -> CatalaTuple:
        return super().__new__(cls, value = members)

    def __getitem__(self, index):
        return self.value[index]

    def __eq__(self, other: Value, pos: SourcePosition | None = None) -> Bool: #type:ignore[override]
        if not isinstance(other, CatalaTuple): return false
        try:
          zipped = zip(self.value, other.value, strict=True)
        except ValueError: return false
        return Bool(all([x.__eq__(y, pos) for x, y in zipped]))

    def compare(self, other: Value, pos: SourcePosition | None = None) -> int:
        if not isinstance(other, CatalaTuple): raise UncomparableValues(pos)
        for (x, y) in zip(self.value, other.value, strict=False):
            cmp = x.compare (y, pos)
            if cmp != 0: return cmp
        return compare (len(self.value), len(other.value))

    def __str__(self, indent: int = 0) -> str:
        return "({})".format(', '.join([x.__str__(indent + 2) for x in self.value]))

class CatalaStruct(Value):
    name: ClassVar[str]
    fields: ClassVar[dict[str, str]]

    def __new__(cls, **init_fields) -> CatalaStruct:
        if not set(init_fields.keys()).issubset(cls.fields.keys()):
            raise Exception(f'Extra field at initialisation of {cls}')
        return super().__new__(cls, **init_fields)

    def __eq__(self, other: Value, pos: SourcePosition | None = None) -> Bool: #type:ignore[override]
        return Bool(isinstance(other, CatalaStruct) and self.name == other.name and
                    all([getattr(self, fld).__eq__(getattr(other, fld), pos) for (fld, label) in self.fields.items()]))

    def compare(self, other: Value, pos: SourcePosition | None = None):
        if not isinstance(other, CatalaStruct):
            raise UncomparableValues(pos)
        if self.name < other.name: return -1
        elif self.name > other.name: return 1
        for fld, label in self.fields.items():
            cmp = getattr(self, fld).compare(getattr(other, fld), pos)
            if cmp != 0: return cmp
        return 0

    def __str__(self, indent: int = 0):
        if len(self.fields) == 0: return f'{self.name} {{}}'
        return "%s {%s\n%s}" % (
            self.name,
            ''.join([ f"\n{'':>{indent+2}}-- {label}: {getattr(self, fld).__str__(indent + 2)}"
                      for fld, label in self.fields.items() ]),
            f"{'':>{indent}}"
        )

class CatalaEnum(Value):
    __slots__ = ('code', 'payload')
    name: ClassVar[str]
    code: Code
    payload: Value

    class Code(Enum):
        def __new__(cls, name):
            obj = object.__new__(cls)
            obj._value_ = len(cls.__members__)
            obj._name_ = name
            return obj
        def __str__(self, indent: int = 0):
            return self.name

    def __new__(cls, code: Code, payload: Value | None = None):
        return super().__new__(cls, code=code, payload=payload)

    def __eq__(self, other: Value, pos: SourcePosition | None = None) -> Bool: #type:ignore[override]
        return Bool(isinstance(other, CatalaEnum) and
                    self.name == other.name and
                    self.code == other.code and
                    self.payload.__eq__(other.payload, pos))

    def compare(self, other: Value, pos: SourcePosition | None = None):
        if not isinstance(other, CatalaEnum): raise UncomparableValues(pos)
        if self.name < other.name: return -1
        elif self.name > other.name: return 1
        if self.code.value < other.code.value: return -1
        elif self.code.value > other.code.value: return 1
        else:
            return self.payload.compare(other.payload, pos)

    def __str__(self, indent: int = 0):
        if isinstance(self.payload, Unit):
            return str(self.code)
        else:
            return f'{self.code} content {self.payload}'

class Function[Targs, TRet](Value):
    __slots__ = ('value')
    value: function

    def __new__(cls, value: Callable) -> Function:
        return super().__new__(cls, value=value)

    def __call__(self, *args: Targs) -> TRet:
        return self.value(*args)

    def __eq__(self, other: object, pos: SourcePosition | None = None) -> Bool: #type:ignore[override]
        raise UncomparableValues(pos)

    def compare(self, other: object, pos: SourcePosition | None = None) -> int:
        raise UncomparableValues(pos)

    def __str__(self, indent: int = 0):
        return "<function>"

# ============
# Type classes
# ============

class Bool(Value):
    __slots__ = ( 'value' )
    value: bool

    def __new__(cls, value) -> Bool:
        return super().__new__(cls, value=bool(value))

    def __eq__(self, other: Value, pos: SourcePosition | None = None) -> Bool: #type:ignore[override]
        return Bool(isinstance(other, Bool) and self.value == other.value)

    def compare(self, other: Value, pos: SourcePosition | None = None) -> int:
        if not isinstance(other, Bool): raise UncomparableValues(pos)
        return self.value - other.value

    def __bool__(self) -> bool:
        return self.value

    def not_(self) -> Bool:
        return Bool(not self.value)

    def __sub__(self, other) -> int:
        return self.value - other.value

    def __str__(self, indent: int = 0):
        if self.value: return "true"
        else: return "false"

true = Bool(True)
false = Bool(False)

class Integer(Value, int): #type:ignore[misc]
    def __new__(cls, value: Union[str, int, Decimal, Money]) -> Integer:
        if isinstance(value, Money):
            return super().__new__(cls, round(Decimal(value)))
        else:
            return super().__new__(cls, value)

    def __truediv__(self, other: Integer, pos: SourcePosition | None = None) -> Decimal: #type:ignore[override]
        return Decimal(self).__truediv__(Decimal(other), pos)

    def __repr__(self) -> str:
        return f"Integer({int(self)})"


class Decimal(Value, Fraction): #type:ignore[misc]
    def __new__(cls, value: object, *args) -> Decimal:
        if isinstance(value, Decimal):
            return value
        if isinstance(value, Fraction):
            frac = value
        elif isinstance(value, Money):
            frac = Fraction(numerator=value, denominator=100)
        else:
            frac = Fraction(*[value, *args])
        obj = object.__new__(cls)
        object.__setattr__(obj, '_numerator', frac.numerator)
        object.__setattr__(obj, '_denominator', frac.denominator)
        return obj

    def __str__(self, indent: int = 0) -> str:
        return "%.10f" % self

    def __repr__(self) -> str:
        return f"Decimal({self.numerator}, {self.denominator})"

    def round(self) -> Decimal:
        return Decimal(round(self))

class Money(Value, int): #type:ignore[misc]
    def __new__(cls, value: object) -> Money:
        if type(value) is int:
            # Not a catala value, raw init from number of cents
            return super().__new__(cls, value)
        elif isinstance(value, Money):
            return value
        else:
            # Otherwise, assume a number of units (= 100 cents)
            ret = super().__new__(cls, round(Decimal(value) * 100))
            return ret

    def __mul__(self, other: Union [Integer, Decimal]) -> Money: #type:ignore[override]
        rat_result : Decimal = Decimal(self) * Decimal(other)
        return Money(rat_result)

    @overload #type:ignore[override]
    def __truediv__(self, other: Money) -> Decimal: ...

    @overload
    def __truediv__(self, other: Union [Integer, Decimal]) -> Money: ...

    def __truediv__(self,
                    other: Union [Integer, Decimal, Money],
                    pos: SourcePosition | None = None
                    ) -> Union [Decimal, Money]:
        if other == 0: raise DivisionByZero(pos)
        if isinstance(other, Money):
            return Decimal(int(self), int(other))
        else:
            return self * (Decimal(1).__truediv__(Decimal(other)))

    def __str__(self, indent: int = 0) -> str:
        return "%01.2f€" % Decimal(self)

    def __repr__(self) -> str:
        return f"Money({int(self)})"

    def decimal(self) -> Decimal:
        return Decimal(self, 100)

    def __float__(self) -> float:
        return float(self.decimal())

    def round(self) -> Money:
        return Money(Integer(self))

class Date(Value):
    __slots__ = ( 'value' )

    value: dates.Date

    def __new__(cls, value: Union [Tuple[int, int, int], dates.Date, int], *args, pos: SourcePosition | None = None) -> Date:
        if isinstance(value, Date):
            return value
        if isinstance(value, dates.Date):
            return super().__new__(cls, value=value)
        if isinstance(value, int):
            (y, m, d) = (value, args[0], args[1])
        else:
            (y, m, d) = (value[0], value[1], value[2])
        try:
            return super().__new__(cls, value=dates.Date(year=int(y), month=int(m), day=int(d)))
        except dates.InvalidDate:
            raise DateError(pos, "|%04d-%02d-%02d| is not a valid date" %
                            (y, m, d))

    def __eq__(self, other: object, pos: SourcePosition | None = None) -> Bool: #type:ignore[override]
        return Bool(isinstance(other, Date) and self.value == other.value)

    def __add__(self, other: Duration, round: dates.DateRounding = dates.DateRounding.AbortOnRound,  pos: SourcePosition | None = None) -> Date:
        try: return Date(self.value.__add__(other.value, round))
        except dates.AmbiguousComputation: raise DateError(pos, note = "ambiguous date computation with no rounding mode specified")

    def compare(self, other: object, pos: SourcePosition | None = None) -> int:
        if not isinstance(other, Date):
            raise UncomparableValues(pos)
        elif self.value < other.value: return -1
        elif self.value > other.value: return 1
        else: return 0

    @overload
    def __sub__(self, other: Date) -> Duration: ...

    @overload
    def __sub__(self, other: Duration, round: dates.DateRounding = dates.DateRounding.AbortOnRound, pos: SourcePosition | None = None) -> Date: ...

    def __sub__(self, \
                other: Union[Date, Duration], \
                round: dates.DateRounding = dates.DateRounding.AbortOnRound, \
                pos: SourcePosition | None = None) -> Union[Duration, Date]:
        if isinstance(other, Date):
            return Duration(self.value - other.value)
        elif isinstance(other, Duration):
            try:
                return Date(self.value.__add__(-other.value, round))
            except dates.AmbiguousComputation:
                raise DateError(pos, note = "ambiguous date computation with no rounding mode specified")
        else:
            raise Exception("Substracting date and invalid obj")

    def __str__(self, indent: int = 0) -> str:
        return f"|{self.value.year:04d}-{self.value.month:02d}-{self.value.day:02d}|"

    def __repr__(self) -> str:
        return f"Date({self.value.__repr__()})"

class Duration(Value):
    __slots__ = ( 'value' )

    value: dates.Period

    def __new__(cls, value: Union [Duration, Tuple[int, int, int], dates.Period, int], *args) -> Duration:
        if isinstance(value, Duration):
            return value
        if isinstance(value, dates.Period):
            p = value
        elif isinstance(value, int):
            p = dates.Period(years=value, months=args[0], days=args[1])
        else:
            p = dates.Period(years=value[0], months=value[1], days=value[2])
        return super().__new__(cls, value=p)

    def __add__(self: Duration, rhs: Duration) -> Duration:
        return Duration(self.value + rhs.value)

    def __sub__(self: Duration, rhs: Duration) -> Duration:
        return Duration(self.value - rhs.value)

    def __neg__(self: Duration) -> Duration:
        return Duration(Duration(0, 0, 0) - self)

    def __truediv__(self, other: Duration, pos: SourcePosition | None = None) -> Decimal: #type:ignore[override]
        if (self.value.years != 0 or other.value.years != 0 or
            self.value.months != 0 or other.value.months != 0):
            raise DateError(pos, note="dividing durations that are not in days")
        else:
            return Decimal(self.value.days).__truediv__(Decimal(other.value.days), pos)

    def __mul__(self: Duration, rhs: Integer) -> Duration:
        return Duration(self.value * rhs)

    def __eq__(self, other: object, pos: SourcePosition | None = None) -> Bool: #type:ignore[override]
        if not isinstance(other, Duration): return false
        if (self.value.years == other.value.years and
            self.value.months == other.value.months and
            self.value.days == other.value.days):
            return true
        elif self.value.days == 0 and other.value.days == 0:
            return Bool(self.value.years * 12 + self.value.months == other.value.years * 12 + other.value.months)
        elif self.value.years == 0 and other.value.years == 0 and self.value.months == 0 and other.value.months == 0:
           return Bool(self.value.days == other.value.days)
        else:
            raise DateError(pos, "ambiguous comparison between durations in different units (e.g. months vs. days)")

    def compare(self, other: Value, pos: SourcePosition | None = None) -> int:
        if not isinstance(other, Duration):
            raise UncomparableValues(pos)
        if self.value.days == 0 and other.value.days == 0:
            return compare (self.value.years * 12 + self.value.months, other.value.years * 12 + other.value.months)
        elif self.value.years == 0 and other.value.years == 0 and self.value.months == 0 and other.value.months == 0:
            return compare (self.value.days, other.value.days)
        else:
            raise DateError(pos, "ambiguous comparison between durations in different units (e.g. months vs. days)")

    def __str__(self, indent: int = 0) -> str:
        return f'[{self.value.years} years, {self.value.months} months, {self.value.days} days]'

    def __repr__(self) -> str:
        return f"Duration({self.value.__repr__()})"


class Unit(Value):
    def __new__(cls) -> Unit:
        return super().__new__(cls)

    def __eq__(self, other: object, pos: SourcePosition | None = None) -> Bool: #type:ignore[override]
        if isinstance(other, Unit):
            return true
        else:
            return false

    def __str__(self, indent: int = 0) -> str:
        return "()"

    def __repr__(self) -> str:
        return "Unit()"

class Option[T: Value](Value):
    __slots__ = ( 'value' )
    value: T | None

    def __new__(cls, value: T | None) -> Option[T]:
        return super().__new__(cls, value = value)

    def __eq__(self, other: Value, pos: SourcePosition | None = None) -> Bool: #type:ignore[override]
        if not isinstance(other, Option): return false
        if self.value is None:
            return Bool(other.value is None)
        elif other.value is None:
            return false
        else:
            return self.value.__eq__(other.value, pos)

    def compare(self, other, pos: SourcePosition | None = None) -> int:
        if not isinstance(other, Option):
            raise UncomparableValues(pos)
        if self.value is None:
            if other.value is None: return 0
            else: return -1
        else:
            if other.value is None: return 1
            else: return self.value.compare(other.value, pos)

    def __str__(self, indent: int = 0) -> str:
        if self.value is None:
            return "Absent"
        else:
            return f'Present content {self.value.__str__(indent + 2)}'

    def __repr__(self) -> str:
        return f"Option({self.value.__repr__()})"

# ============================
# Constructors and conversions
# ============================

def round(q : Decimal) -> Decimal:
    qabs = abs(q)
    n = qabs.numerator
    d = qabs.denominator
    abs_round = (2 * n + d) // (2 * d)
    if q.numerator >= 0: return Decimal(abs_round)
    else: return Decimal(- abs_round)

# ========
# Defaults
# ========


def handle_exceptions(
    exceptions: List[Option[CatalaTuple]]) -> Option[CatalaTuple]:
    active_exns: List[CatalaTuple] = [e.value for e in exceptions if e.value is not None]
    count = len(active_exns)
    if count == 0:
        return Option(None)
    elif count == 1:
        return Option(active_exns[0])
    else:
        raise Conflict([e[1] for e in active_exns])

class Empty(Exception): pass

def no_input() -> Callable[[Unit], Alpha]:
    def closure(_: Unit):
        raise Empty
    return closure


# This value is used for the Python code generation to trump mypy and forcing
# it to accept dead code. Indeed, when raising an exception during a variable
# definition, mypy complains that the later dead code will not know what
# this variable was. So we give this variable a dead value.
dead_value: Any = 0

# =======
# Logging
# =======


class LogEventCode(Enum):
    VariableDefinition = auto()
    BeginCall = auto()
    EndCall = auto()
    DecisionTaken = auto()


class InputIO(Enum):
    NoInput = auto()
    OnlyInput = auto()
    Reentrant = auto()


class LogIO(Value):
    __slots__ = ( 'input_io', 'output_io' )
    input_io: InputIO
    output_io: bool

    def __new__(cls, input_io: InputIO, output_io: bool):
        return super().__new__(cls, input_io=input_io, output_io=output_io)

class LogEvent:
    def __init__(self, code: LogEventCode, io: Option[LogIO], payload: Union[List[str], SourcePosition, Tuple[List[str], Alpha]]) -> None:
        self.code = code
        self.io = io
        self.payload = payload


log: List[LogEvent] = []


def reset_log():
    global log; log = []


def retrieve_log() -> List[LogEvent]:
    return log


def log_variable_definition(headings: List[str], io: LogIO, value: Alpha) -> Alpha:
    log.append(LogEvent(LogEventCode.VariableDefinition, Option(io),
               (headings, copy.deepcopy(value))))
    return value


def log_begin_call(headings: List[str], f: Callable[[Alpha], Beta], value: Alpha) -> Beta:
    log.append(LogEvent(LogEventCode.BeginCall, Option(None), headings))
    return f(value)


def log_end_call(headings: List[str], value: Alpha) -> Alpha:
    log.append(LogEvent(LogEventCode.EndCall, Option(None), headings))
    return value


def log_decision_taken(pos: SourcePosition, value: bool) -> bool:
    log.append(LogEvent(LogEventCode.DecisionTaken, Option(None), pos))
    return value
