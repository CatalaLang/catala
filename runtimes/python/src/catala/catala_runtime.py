"""
.. module:: catala_runtime
   :platform: Unix, Windows
   :synopsis: The Python bindings for the functions used in the generated Catala code
   :noindex:

.. moduleauthor:: Denis Merigoux <denis.merigoux@inria.fr>
"""
from __future__ import annotations # 'ClsType' ~> ClsType annotations

# This file should be in sync with compiler/runtime.{ml, mli} !

from gmpy2 import log2, mpz, mpq, mpfr, t_divmod, qdiv, f_div, t_div, sign  # type: ignore
import dates
from typing import NewType, List, Generic, Callable, Tuple, TypeVar, Iterable, Union, Any, overload
from functools import reduce
from enum import Enum
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
    __slots__ = ( 'message', 'source_positions' )

    def __init__(self, message: str, source_positions: List[SourcePosition]) -> None:
        self.message = message
        self.source_positions = source_positions
    # Prints in the same format as the OCaml runtime
    def __str__(self) -> str:
        return "[ERROR] At {}: {}".format(
            ', '.join([str(e) for e in self.source_positions]),
            self.message)

class AssertionFailed(CatalaError):
    def __init__(self, source_position: SourcePosition) -> None:
        super().__init__(
            "this assertion doesn't hold", [source_position])

class NoValue(CatalaError):
    def __init__(self, source_position: SourcePosition) -> None:
        super().__init__("no computation with valid conditions found", [source_position])

class Conflict(CatalaError):
    def __init__(self, pos: List[SourcePosition]) -> None:
        super().__init__("two or more concurring valid computations", pos)

class DivisionByZero(CatalaError):
    def __init__(self, source_position: SourcePosition) -> None:
        super().__init__("division by zero", [source_position])

class ListEmpty(CatalaError):
    def __init__(self, source_position: SourcePosition) -> None:
        super().__init__("the list was empty", [source_position])

class NotSameLength(CatalaError):
    def __init__(self, source_position: SourcePosition) -> None:
        super().__init__("traversing multiple lists of different lengths",
                         [source_position])

class InvalidDate(CatalaError):
    def __init__(self, source_position: SourcePosition | None) -> None:
        super().__init__(
            "the provided numbers do not correspond to a valid date",
            [source_position] if source_position is not None else [])

class UncomparableDurations(CatalaError):
    def __init__(self, source_position: SourcePosition | None) -> None:
        super().__init__(
            "comparing durations in different units (e.g. months vs. days)",
            [source_position] if source_position is not None else [])

class AmbiguousDateRounding(CatalaError):
    def __init__(self, source_position: SourcePosition | None) -> None:
        super().__init__("ambiguous date computation, and rounding mode was not specified",
                         [source_position] if source_position is not None else [])

class IndivisibleDurations(CatalaError):
    def __init__(self, source_position: SourcePosition | None) -> None:
        super().__init__("dividing durations that are not in days",
                         [source_position] if source_position is not None else [])

class Impossible(CatalaError):
    def __init__(self, source_position: SourcePosition) -> None:
        super().__init__("\"impossible\" computation reached",
                         [source_position])

# ============
# Type classes
# ============

class Integer:
    __slots__ = ( 'value' )

    value: int

    def __init__(self, value: Union[str, int, Decimal]) -> None:
        if isinstance(value, Decimal):
            self.value = t_div(
                  value.value.numerator,
                  value.value.denominator)
        else:
            self.value = mpz(value)

    def __add__(self, other: Integer) -> Integer:
        return Integer(self.value + other.value)

    def __sub__(self, other: Integer) -> Integer:
        return Integer(self.value - other.value)

    def __mul__(self, other: Integer) -> Integer:
        return Integer(self.value * other.value)

    def __truediv__(self, other: Integer) -> Decimal:
        return Decimal(self.value) / Decimal(other.value)

    def __neg__(self: Integer) -> Integer:
        return Integer(- self.value)

    def __lt__(self, other: Integer) -> bool:
        return self.value < other.value

    def __le__(self, other: Integer) -> bool:
        return self.value <= other.value

    def __gt__(self, other: Integer) -> bool:
        return self.value > other.value

    def __ge__(self, other: Integer) -> bool:
        return self.value >= other.value

    def __ne__(self, other: object) -> bool:
        if isinstance(other, Integer):
            return self.value != other.value
        else:
            return True

    def __eq__(self, other: object) -> bool:
        if isinstance(other, Integer):
            return self.value == other.value
        else:
            return False

    def __str__(self) -> str:
        return self.value.__str__()

    def __repr__(self) -> str:
        return f"Integer({self.value.__repr__()})"


class Decimal:
    __slots__ = ( 'value' )

    value: mpq

    def __init__(self, value: Union[str, int, float, Integer]) -> None:
        if isinstance(value, Integer):
            self.value = mpq(value.value)
        else:
            self.value = mpq(value)

    def __add__(self, other: Decimal) -> Decimal:
        return Decimal(self.value + other.value)

    def __sub__(self, other: Decimal) -> Decimal:
        return Decimal(self.value - other.value)

    def __mul__(self, other: Decimal) -> Decimal:
        return Decimal(self.value * other.value)

    def __truediv__(self, other: Decimal) -> Decimal:
        return Decimal(self.value / other.value)

    def __neg__(self: Decimal) -> Decimal:
        return Decimal(- self.value)

    def __lt__(self, other: Decimal) -> bool:
        return self.value < other.value

    def __le__(self, other: Decimal) -> bool:
        return self.value <= other.value

    def __gt__(self, other: Decimal) -> bool:
        return self.value > other.value

    def __ge__(self, other: Decimal) -> bool:
        return self.value >= other.value

    def __ne__(self, other: object) -> bool:
        if isinstance(other, Decimal):
            return self.value != other.value
        else:
            return True

    def __eq__(self, other: object) -> bool:
        if isinstance(other, Decimal):
            return self.value == other.value
        else:
            return False

    def __str__(self) -> str:
        return "{}".format(mpfr(self.value))

    def __repr__(self) -> str:
        return f"Decimal({self.value.__repr__()})"


class Money:
    __slots__ = ( 'value' )

    value: Integer

    def __init__(self, value: Integer) -> None:
        self.value = value

    def __add__(self, other: Money) -> Money:
        return Money(self.value + other.value)

    def __sub__(self, other: Money) -> Money:
        return Money(self.value - other.value)

    def __mul__(self, other: Union [Integer, Decimal]) -> Money:
        rat_result : Decimal = decimal_of_integer(self.value) * Decimal(other.value)
        return Money(round(rat_result))

    def __truediv__(self, other: Union [Integer, Decimal, Money]) -> Union [Decimal, Money]:
        if isinstance(other, Money):
            return self.value / other.value
        else:
            return self * (Decimal(1) / Decimal(other.value))

    def __neg__(self: Money) -> Money:
        return Money(- self.value)

    def __lt__(self, other: Money) -> bool:
        return self.value < other.value

    def __le__(self, other: Money) -> bool:
        return self.value <= other.value

    def __gt__(self, other: Money) -> bool:
        return self.value > other.value

    def __ge__(self, other: Money) -> bool:
        return self.value >= other.value

    def __ne__(self, other: object) -> bool:
        if isinstance(other, Money):
            return self.value != other.value
        else:
            return True

    def __eq__(self, other: object) -> bool:
        if isinstance(other, Money):
            return self.value == other.value
        else:
            return False

    def __str__(self) -> str:
        return "%.2f" % (int(self.value.value) / 100)

    def __repr__(self) -> str:
        return f"Money({self.value.__repr__()})"

class Date:
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
            return Date(self.value + (-other.value))
        else:
            raise Exception("Substracting date and invalid obj")

    def __eq__(self, other: object) -> bool:
        if isinstance(other, Date):
            return self.value == other.value
        else:
            return False

    def __lt__(self, other: Date) -> bool:
        return self.value < other.value

    def __le__(self, other: Date) -> bool:
        return self < other or self == other

    def __gt__(self, other: Date) -> bool:
        return self.value > other.value

    def __ge__(self, other: Date) -> bool:
        return self > other or self == other

    def __ne__(self, other: object) -> bool:
        if isinstance(other, Date):
            return self.value != other.value
        else:
            return True

    def __str__(self) -> str:
        return self.value.__str__()

    def __repr__(self) -> str:
        return f"Date({self.value.__repr__()})"

class Duration:
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

    def __neg__(self: Duration) -> Duration:
        return Duration(- self.value)

    def __eq__(self, other: object, pos: SourcePosition | None = None) -> bool:
        if isinstance(other, Duration):
            if self.value == other.value:
                return True
            elif self.value.days == 0 and other.value.days == 0:
                return self.value.years * 12 + self.value.months == other.value.years * 12 + other.value.months
            elif self.value.years == 0 and other.value.years == 0 and self.value.months == 0 and other.value.months == 0:
               return self.value.days == other.value.days
            else:
                raise UncomparableDurations(pos)
        else:
            return False

    def __ne__(self, other: object, pos: SourcePosition | None = None) -> bool:
        return not(self.__eq__(other, pos))

    def __lt__(self, other: Duration, pos: SourcePosition | None = None) -> bool:
        if self.value.days == 0 and other.value.days == 0:
            return self.value.years * 12 + self.value.months < other.value.years * 12 + other.value.months
        elif self.value.years == 0 and other.value.years == 0 and self.value.months == 0 and other.value.months == 0:
            return self.value.days < other.value.days
        else:
            raise UncomparableDurations(pos)

    def __le__(self, other: Duration, pos: SourcePosition | None = None) -> bool:
        if self.value.days == 0 and other.value.days == 0:
            return self.value.years * 12 + self.value.months <= other.value.years * 12 + other.value.months
        elif self.value.years == 0 and other.value.years == 0 and self.value.months == 0 and other.value.months == 0:
            return self.value.days <= other.value.days
        else:
            raise UncomparableDurations(pos)

    def __gt__(self, other: Duration, pos: SourcePosition | None = None) -> bool:
        if self.value.days == 0 and other.value.days == 0:
            return self.value.years * 12 + self.value.months > other.value.years * 12 + other.value.months
        elif self.value.years == 0 and other.value.years == 0 and self.value.months == 0 and other.value.months == 0:
            return self.value.days > other.value.days
        else:
            raise UncomparableDurations(pos)

    def __ge__(self, other: Duration, pos: SourcePosition | None = None) -> bool:
        if self.value.days == 0 and other.value.days == 0:
            return self.value.years * 12 + self.value.months >= other.value.years * 12 + other.value.months
        elif self.value.years == 0 and other.value.years == 0 and self.value.months == 0 and other.value.months == 0:
            return self.value.days >= other.value.days
        else:
            raise UncomparableDurations(pos)

    def __str__(self) -> str:
        return self.value.__str__()

    def __repr__(self) -> str:
        return f"Duration({self.value.__repr__()})"


class Unit:
    __slots__ = ()

    def __init__(self) -> None:
        ...

    def __eq__(self, other: object) -> bool:
        if isinstance(other, Unit):
            return True
        else:
            return False

    def __ne__(self, other: object) -> bool:
        if isinstance(other, Unit):
            return False
        else:
            return True

    def __str__(self) -> str:
        return "()"

    def __repr__(self) -> str:
        return "Unit()"

class Option(Generic[Alpha]):
    __slots__ = ( 'value' )

    value: Alpha | None

    def __init__(self, value: Alpha | None) -> None:
        self.value = value

    def __eq__(self, other: object) -> bool:
        if isinstance(other, Option):
            return self.value == other.value
        else:
            return False

    def __str__(self) -> str:
        if self.value is None:
            return "Absent"
        else:
            return "Present({})".format(self.value)

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
    sgn = sign(q.value)
    qabs = abs(q.value)
    n = qabs.numerator
    d = qabs.denominator
    abs_round = (2 * n + d) // (2 * d)
    return Integer(sgn * abs_round)

# -----
# Money
# -----


def money_of_cents_string(v: str) -> Money:
    return Money(Integer(v))


def money_of_units_int(v: int) -> Money:
    return Money(Integer(v) * Integer(100))


def money_of_cents_integer(v: Integer) -> Money:
    return Money(v)


def money_to_float(m: Money) -> float:
    return float(mpfr(mpq(m.value.value, 100)))


def money_to_string(m: Money) -> str:
    return str(money_to_float(m))


def money_to_cents(m: Money) -> Integer:
    return m.value

def money_round(m: Money) -> Money:
    units : Decimal = Decimal(m.value) / Decimal(100)
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
    return float(mpfr(d.value))


def decimal_of_float(d: float) -> Decimal:
    return Decimal(d)


def integer_of_decimal(d: Decimal) -> Integer:
    return Integer(d.value)

def decimal_of_integer(d: Integer) -> Decimal:
    return Decimal(d.value)

def decimal_to_string(precision: int, i: Decimal) -> str:
    return "{1:.{0}}".format(precision, mpfr(i.value, precision * 10 // 2))


def decimal_round(q: Decimal) -> Decimal:
    return Decimal(round(q))

def decimal_of_money(m: Money) -> Decimal:
    return Decimal(mpq(qdiv(m.value.value, 100)))

def integer_of_money(m: Money) -> Integer:
    return round(decimal_of_money(m))


# --------
# Integers
# --------


def integer_of_string(s: str) -> Integer:
    return Integer(s)


def integer_to_string(d: Integer) -> str:
    return str(d.value)


def integer_of_int(d: int) -> Integer:
    return Integer(d)


def integer_to_int(d: Integer) -> int:
    return int(d.value)


def integer_exponentiation(i: Integer, e: int) -> Integer:
    return i ** e  # type: ignore


def integer_log2(i: Integer) -> int:
    return int(log2(i.value))

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

# -----
# Lists
# -----


def list_fold_left(f: Callable[[Alpha, Beta], Alpha], init: Alpha, l: List[Beta]) -> Alpha:
    return reduce(f, l, init)


def list_filter(f: Callable[[Alpha], bool], l: List[Alpha]) -> List[Alpha]:
    return [i for i in l if f(i)]


def list_map(f: Callable[[Alpha], Beta], l: List[Alpha]) -> List[Beta]:
    return [f(i) for i in l]

def list_map2(pos: SourcePosition, f: Callable[[Alpha, Beta], Gamma], l1: List[Alpha], l2: List[Beta]) -> List[Gamma]:
    try:
      zipped = zip(l1, l2, strict=True)
    except ValueError: raise NotSameLength(pos)
    return [f(i, j) for i, j in zipped]

def list_reduce(f: Callable[[Alpha, Alpha], Alpha], dft: Callable[[Unit], Alpha], l: List[Alpha]) -> Alpha:
    if l == []:
        return dft(Unit())
    else:
        return reduce(f, l)


def list_length(l: List[Alpha]) -> Integer:
    return Integer(len(l))

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
    VariableDefinition = 0
    BeginCall = 1
    EndCall = 2
    DecisionTaken = 3


class InputIO(Enum):
    NoInput = 0
    OnlyInput = 1
    Reentrant = 2


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
