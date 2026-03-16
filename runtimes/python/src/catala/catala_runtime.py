"""
.. module:: catala_runtime
   :platform: Unix, Windows
   :synopsis: The Python bindings for the functions used in the generated Catala code
   :noindex:

.. moduleauthor:: Denis Merigoux <denis.merigoux@inria.fr>
"""
from __future__ import annotations # 'ClsType' ~> ClsType annotations

# This file should be in sync with compiler/runtime.{ml, mli} !

# from gmpy2 import log2, mpz, mpq, mpfr, t_divmod, qdiv, f_div, t_div, sign  # type: ignore
import math
from fractions import Fraction
import dates
from typing import NewType, List, Generic, Callable, Tuple, TypeVar, Iterable, Union, Any, overload
from functools import reduce
from enum import Enum, IntEnum, nonmember, auto
import copy

Alpha = TypeVar('Alpha')
Beta = TypeVar('Beta')
Gamma = TypeVar('Gamma')

# ==========
# Exceptions
# ==========

class SourcePosition:
    __slots__ = ( 'filename', 'start_line', 'start_column', 'end_line', 'end_column', 'law_headings' )

    def __init__(self,
                 filename: str,
                 start_line: int,
                 start_column: int,
                 end_line: int,
                 end_column: int,
                 law_headings: List[str]) -> None:
        self.filename = filename
        self.start_line = start_line
        self.start_column = start_column
        self.end_line = end_line
        self.end_column = end_column
        self.law_headings = law_headings

    def __str__(self) -> str:
        return "{}:{}.{}-{}.{}".format(
            self.filename,
            self.start_line, self.start_column,
            self.end_line, self.end_column)

    def __eq__(self, other: object) -> bool:
        if isinstance(other, SourcePosition):
            return (self.filename == other.filename and
                    self.start_line == other.start_line and
                    self.start_column == other.start_column and
                    self.end_line == other.end_line and
                    self.end_column == other.end_column)
        else:
            return False

class CatalaError(Exception):
    __slots__ = ( 'message', 'source_positions', 'note' )

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

class InvalidDate(CatalaError):
    message = "the provided numbers do not correspond to a valid date"

class UndefinedComparison(CatalaError):
    message = "comparison of values of these types is not supported"

class UncomparableDurations(CatalaError):
    message = "comparing durations in different units (e.g. months vs. days)"

class AmbiguousDateRounding(CatalaError):
    message = "ambiguous date computation, and rounding mode was not specified"

class IndivisibleDurations(CatalaError):
    message = "dividing durations that are not in days"

class Impossible(CatalaError):
    message = "\"impossible\" computation reached"

# =============================
# Types and value introspection
# =============================

def compare(a: object, b: object):
    return (a > b) - (a < b)

# Generic base class
class Value:
    # Defaults expected to be overriden
    def __truediv__(self, other: object, pos: SourcePosition | None = None):
        if other == 0: raise DivisionByZero(pos)
        else: return super().__truediv__(other)

    def __eq__(self, other: object, pos: SourcePosition | None = None):
        return super().__eq__(other)

    def compare(self, other: object, pos: SourcePosition | None = None) -> int:
        if type(self) is not type(other):
            raise UndefinedComparison(pos)
        return super().__gt__(other) - super().__lt(other)

    def __str__(self, indent: int = 0) -> str:
        return super().__str__()

    # deduced methods
    def __ne__(self, other: object, pos: SourcePosition | None = None) -> bool:
        return not(self.__eq__(other, pos))

    def __lt__(self, other: object, pos: SourcePosition | None = None) -> bool:
        return self.compare(other, pos) < 0

    def __le__(self, other: object, pos: SourcePosition | None = None) -> bool:
        return self.compare(other, pos) <= 0

    def __gt__(self, other: object, pos: SourcePosition | None = None) -> bool:
        return self.compare(other, pos) > 0

    def __ge__(self, other: object, pos: SourcePosition | None = None) -> bool:
        return self.compare(other, pos) >= 0

class Array(Value):
    def __getitem__(self, index):
        return self.value[index]

    def __iter__(self):
        return iter(self.value)

    def __all__(self):
        return all(self.value)

    def __str__(self, indent: int = 0) -> str:
        if len(self.value) == 0:
            return '[]'
        else:
            return "[%s%s]" % (
                ''.join([f"\n{'':>{indent + 2}}" + x.__str__(indent + 2) + ";"
                         for x in self.value]),
                f"\n{'':>{indent}}"
            )

    def __eq__(self, other: object, pos: SourcePosition | None = None):
        try:
          zipped = zip(self.value, other.value, strict=True)
        except ValueError: return False
        return all([x.__eq__(y, pos) for x, y in zipped])

    def compare(self, other: object, pos: SourcePosition | None = None):
        if not isinstance(other, Array):
            raise UndefinedComparison(pos)
        for (x, y) in zip(self.value, other.value, strict=False):
            cmp = x.compare (y, pos)
            if cmp != 0: return cmp
        return compare (len(self.value), len(other.value))

    def fold_left(self: List[Beta], f: Callable[[Alpha, Beta], Alpha], init: Alpha) -> Alpha:
        return reduce(f, self.value, init)

    def filter(self: List[Alpha], f: Callable[[Alpha], bool]) -> List[Alpha]:
        return [i for i in self.value if f(i)]

    def map(self: List[Alpha], f: Callable[[Alpha], Beta]) -> List[Beta]:
        return Array([f(i) for i in self.value])

    def map2(self: List[Alpha], pos: SourcePosition, f: Callable[[Alpha, Beta], Gamma], l2: List[Beta]) -> List[Gamma]:
        try:
          zipped = zip(self.value, l2.value, strict=True)
        except ValueError: raise NotSameLength(pos)
        return Array([f(i, j) for i, j in zipped])

    def reduce(self: List[Alpha], f: Callable[[Alpha, Alpha], Alpha], dft: Callable[[Unit], Alpha]) -> Alpha:
        if len(self.value) == 0:
            return dft(Unit())
        else:
            return reduce(f, self.value)

    def length(self: List[Alpha]) -> Integer:
        return Integer(len(self.value))


class CatalaTuple(Value):
    def __init__(self, *members):
        self.value = members

    def __getitem__(self, index):
        return self.value[index]

    def __eq__(self, other: object, pos: SourcePosition | None = None):
        try:
          zipped = zip(self.value, other.value, strict=True)
        except ValueError: return False
        return all([x.__eq__(y, pos) for x, y in zipped])

    def compare(self, other: object, pos: SourcePosition | None = None) -> bool:
        if type(self) is not type(other):
            raise UndefinedComparison(pos)
        for (x, y) in zip(self.value, other.value, strict=False):
            cmp = x.compare (y, pos)
            if cmp != 0: return cmp
        return compare (len(self.value), len(other.value))

    def __str__(self, indent: int = 0) -> str:
        return "({})".format(', '.join([x.__str__(indent + 2) for x in self.value]))

class CatalaStruct(Value):
    def __init__(self, **init_fields):
        for (fld, label) in self.fields:
            setattr(self, fld, init_fields[fld])
            del init_fields[fld]
        if init_fields != {}:
            raise Exception(f'Extra field at initialisation of {type(self)}')

    def __eq__(self, other: object, pos: SourcePosition | None = None):
        if not isinstance(other, CatalaStruct) or not self.name == other.name:
            return False
        return all([getattr(self, fld).__eq__(getattr(other, fld), pos) for (fld, label) in self.fields])

    def compare(self, other: object, pos: SourcePosition | None = None):
        if not isinstance(other, CatalaStruct):
            raise UndefinedComparison(pos)
        if self.name < other.name: return -1
        elif self.name > other.name: return 1
        for (fld, label) in fields:
            cmp = getattr(self, fld).compare(getattr(other, fld), pos)
            if cmp != 0: return cmp
        return 0

    def __str__(self, indent: int = 0):
        if len(self.fields) == 0: return f'{self.name} {{}}'
        return "%s {%s\n%s}" % (
            self.name,
            ''.join([f"\n{'':>{indent+2}}-- {label}: {getattr(self, fld).__str__()}" for (fld, label) in self.fields]), # Should be (indent + 2)
            f"{'':>{indent}}"
        )

class CatalaEnum(Value):
    __slots__ = ('name', 'code', 'payload')

    class Code(Enum):
        def __new__(cls, label):
            obj = object.__new__(cls)
            obj._value_ = len(cls.__members__)
            obj.label = label
            return obj

        def __str__(self, indent: int = 0):
            return self.label

    def __init__(self, code: Code, payload: Any = None) -> None:
        self.code = code
        self.payload = payload

    def __eq__(self, other: object, pos: SourcePosition | None = None):
        return isinstance(other, CatalaEnum) and self.name == other.name and self.code == other.code and self.payload.__eq__(other.payload, pos)

    def compare(self, other: object, pos: SourcePosition | None = None):
        if not isinstance(other, CatalaEnum):
            raise UndefinedComparison(pos)
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

class Function(Value):
    __slots__ = ('value')

    def __call__(self, *args):
        return self.value(*args)

    def __eq__(self, other: object, pos: SourcePosition | None = None):
        raise UndefinedComparison(pos)

    def compare(self, other: object, pos: SourcePosition | None = None):
        raise UndefinedComparison(pos)

    def __str__(self, indent: int = 0):
        return "<function>"

# ============
# Type classes
# ============

class Bool(Value):
    __slots__ = ( 'value' )

    value: bool

    def __init__(self, value) -> None:
        self.value = value

    def __eq__(self, other: object, pos: SourcePosition | None = None) -> Bool:
        return self.value == other.value

    def __bool__(self):
        return self.value

    def __str__(self, indent: int = 0):
        if self.value: return "true"
        else: return "false"


class Integer(Value, int):
    def __new__(cls, value: Union[str, int, Decimal]) -> Integer:
        return super().__new__(cls, int(value))

    def __repr__(self) -> str:
        return f"Integer({int(self)})"


class Decimal(Value, Fraction):
    def __new__(cls, value: object, *args) -> Decimal:
        if isinstance(value, Money):
            return super().__new__(cls, numerator=value, denominator=100)
        else:
            return super().__new__(cls, value, *args)

    def __str__(self, indent: int = 0) -> str:
        return "%.10f" % self

    def __repr__(self) -> str:
        return f"Decimal({self.value.__repr__()})"


class Money(Integer):
    def __mul__(self, other: Union [Integer, Decimal]) -> Money:
        rat_result : Decimal = Decimal(self) * Decimal(other)
        return Money(round(rat_result))

    @overload
    def __truediv__(self, other: Money) -> Decimal: ...

    @overload
    def __truediv__(self, other: Union [Integer, Decimal]) -> Money: ...

    def __truediv__(self,
                    other: Union [Integer, Decimal, Money],
                    pos: SourcePosition = None
                    ) -> Union [Decimal, Money]:
        if other == 0: raise DivisionByZero(pos)
        if isinstance(other, Money):
            return Decimal(self, other)
        else:
            return self * (Decimal(1).__truediv__(Decimal(other)))

    def __str__(self, indent: int = 0) -> str:
        return "%01.2f€" % (self / 100)

    def __repr__(self) -> str:
        return f"Money({int(self)})"

    def decimal(self) -> Decimal:
        return Decimal(self, 100)

    def __float__(self) -> float:
        return float(self.decimal())


class Date(Value):
    __slots__ = ( 'value' )

    value: dates.Date

    def __init__(self, value: Union [Tuple[int, int, int], dates.Date], pos: SourcePosition | None = None):
        try:
            if isinstance(value, dates.Date):
                self.value = value
            else:
                self.value = dates.Date(year=value[0], month=value[1], day=value[2])
        except dates.InvalidDate: raise InvalidDate(pos)

    def __add__(self, other: Duration, round: dates.DateRounding = dates.DateRounding.AbortOnRound,  pos: SourcePosition | None = None) -> Date:
        try: return Date(self.value.__add__(other.value, round))
        except dates.AmbiguousComputation: raise AmbiguousDateRounding(pos)

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
                raise AmbiguousDateRounding(pos)
        else:
            raise Exception("Substracting date and invalid obj")

    def __str__(self, indent: int = 0) -> str:
        return f"|{self.value.year:04d}-{self.value.month:02d}-{self.value.day:02d}|"

    def __repr__(self) -> str:
        return f"Date({self.value.__repr__()})"

class Duration(Value):
    __slots__ = ( 'value' )

    value: dates.Period

    def __init__(self, value: Union [Tuple[int, int, int], dates.Period]):
        if isinstance(value, dates.Period):
            self.value = value
        else:
            self.value = dates.Period(years=value[0], months=value[1], days=value[2])

    def __add__(self: Duration, rhs: Duration) -> Duration:
        return Duration(self.value + rhs.value)

    def __sub__(self: Duration, rhs: Duration) -> Duration:
        return Duration(self.value - rhs.value)

    def __truediv__(self, other: Duration, pos: SourcePosition | None = None) -> Decimal:
        if (self.value.years != 0 or other.value.years != 0 or
            self.value.months != 0 or other.value.months != 0):
            raise IndivisibleDurations(pos)
        else:
            return Decimal(self.value.days) / Decimal(other.value.days)

    def __mul__(self: Duration, rhs: Integer) -> Duration:
        return Duration(self.value * rhs.value)

    def __eq__(self, other: object, pos: SourcePosition | None = None) -> bool:
        if not isinstance(other, Duration): return False
        if self.value == other.value:
            return True
        elif self.value.days == 0 and other.value.days == 0:
            return self.value.years * 12 + self.value.months == other.value.years * 12 + other.value.months
        elif self.value.years == 0 and other.value.years == 0 and self.value.months == 0 and other.value.months == 0:
           return self.value.days == other.value.days
        else:
            raise UncomparableDurations(pos)

    def compare(self, other: Duration, pos: SourcePosition | None = None) -> bool:
        if not isinstance(other, Duration):
            raise UndefinedComparison(pos)
        if self.value.days == 0 and other.value.days == 0:
            return compare (self.value.years * 12 + self.value.months, other.value.years * 12 + other.value.months)
        elif self.value.years == 0 and other.value.years == 0 and self.value.months == 0 and other.value.months == 0:
            return compare (self.value.days, other.value.days)
        else:
            raise UncomparableDurations(pos)

    def __str__(self, indent: int = 0) -> str:
        return f'[{self.value.years} years, {self.value.months} months, {self.value.days} days]'

    def __repr__(self) -> str:
        return f"Duration({self.value.__repr__()})"


class Unit(Value):
    __slots__ = ()

    def __init__(self) -> None:
        pass

    def __eq__(self, other: object, pos: SourcePosition | None = None) -> bool:
        if isinstance(other, Unit):
            return True
        else:
            return False

    def __ne__(self, other: object) -> bool:
        if isinstance(other, Unit):
            return False
        else:
            return True

    def __str__(self, indent: int = 0) -> str:
        return "()"

    def __repr__(self) -> str:
        return "Unit()"

class Option(Generic[Alpha], Value):
    __slots__ = ( 'value' )

    value: Alpha | None

    def __init__(self, value: Alpha | None) -> None:
        self.value = value

    def __eq__(self, other: object, pos: SourcePosition | None = None) -> bool:
        if isinstance(other, Option):
            return self.value.__eq__(other.value, pos)
        else:
            return False

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


def div(pos: SourcePosition, x, y):
    try: return x / y
    except ZeroDivisionError: raise DivisionByZero(pos)

def eq_duration(pos: SourcePosition, d1: Duration, d2: Duration) -> bool:
    return d1.__eq__(d2, pos)

def le_duration(pos: SourcePosition, d1: Duration, d2: Duration) -> bool:
    return d1.__le__(d2, pos)
def lt_duration(pos: SourcePosition, d1: Duration, d2: Duration) -> bool:
    return d1.__lt__(d2, pos)
def ge_duration(pos: SourcePosition, d1: Duration, d2: Duration) -> bool:
    return d1.__ge__(d2, pos)
def gt_duration(pos: SourcePosition, d1: Duration, d2: Duration) -> bool:
    return d1.__gt__(d2, pos)

def round(q : Decimal) -> Integer:
    qabs = abs(q)
    n = qabs.numerator
    d = qabs.denominator
    abs_round = (2 * n + d) // (2 * d)
    print (f'ROUND : {q} => {math.copysign(abs_round, q)} => {Integer(math.copysign(abs_round, q))}')
    return Integer(math.copysign(abs_round, q))

# -----
# Money
# -----


def money_of_cents_string(v: str) -> Money:
    return Money(Integer(v))


def money_of_units_int(v: int) -> Money:
    return Money(Integer(v) * Integer(100))


def money_of_cents_integer(v: Integer) -> Money:
    return Money(v)


def money_to_string(m: Money) -> str:
    return str(m)


def money_to_cents(m: Money) -> Integer:
    return Integer(m)

def money_round(m: Money) -> Money:
    units : Decimal = Decimal(m) / Decimal(100)
    return Money(round(units) * Integer(100))

def money_of_decimal(d: Decimal) -> Money:
    """
    Warning: rounds to the nearest cent
    """
    return Money(round(d * Decimal(100)))

def money_of_integer(i: Integer) -> Money:
    return Money(i * Integer(100))


# --------
# Decimals
# --------


def decimal_of_string(d: str) -> Decimal:
    return Decimal(d)


def decimal_to_float(d: Decimal) -> float:
    return float(d)


def decimal_of_float(d: float) -> Decimal:
    return Decimal(d)


def integer_of_decimal(d: Decimal) -> Integer:
    return Integer(d)

def decimal_of_integer(d: Integer) -> Decimal:
    return Decimal(d)

def decimal_to_string(precision: int, i: Decimal) -> str:
    return "{1:.{0}}".format(precision, mpfr(i, precision * 10 // 2))


def decimal_round(q: Decimal) -> Decimal:
    return Decimal(round(q))

def decimal_of_money(m: Money) -> Decimal:
    return Decimal(m)

def integer_of_money(m: Money) -> Integer:
    return round(Decimal(m))


# --------
# Integers
# --------


def integer_of_string(s: str) -> Integer:
    return Integer(s)


def integer_to_string(d: Integer) -> str:
    return str(d)


def integer_of_int(d: int) -> Integer:
    return Integer(d)


def integer_to_int(d: Integer) -> int:
    return int(d)


def integer_exponentiation(i: Integer, e: int) -> Integer:
    return i ** e  # type: ignore


def integer_log2(i: Integer) -> int:
    return int(math.log2(i))

# -----
# Dates
# -----


def day_of_month_of_date(d: Date) -> Integer:
    return integer_of_int(d.value.day)


def month_number_of_date(d: Date) -> Integer:
    return integer_of_int(d.value.month)


def year_of_date(d: Date) -> Integer:
    return integer_of_int(d.value.year)

def date_to_string(d: Date) -> str:
    return "{}".format(d.value)

def add_date_duration(rounding: dates.DateRounding):
    def add(pos: SourcePosition, dat: Date, dur: Duration):
        return dat.__add__(dur, rounding, pos)
    return add

def sub_date_duration(rounding: dates.DateRounding):
    def sub(pos: SourcePosition, dat: Date, dur: Duration):
        return dat.__sub__(dur, rounding, pos)
    return sub

# ---------
# Durations
# ---------

def duration_to_years_months_days(d: Duration) -> Tuple[int, int, int]:
    return (d.value.years, d.value.value.months, d.value.value.days)  # type: ignore


def duration_to_string(s: Duration) -> str:
    return "{}".format(s.value)

# ========
# Defaults
# ========


def handle_exceptions(
    exceptions: List[Option[Tuple[Alpha,SourcePosition]]]) -> Option[Tuple[Alpha,SourcePosition]]:
    active_exns: List[Tuple[Alpha,SourcePosition]] = [e.value for e in exceptions if e.value is not None]
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


class LogIO:
    def __init__(self, input_io: InputIO, output_io: bool):
        self.input_io = input_io
        self.output_io = output_io


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
