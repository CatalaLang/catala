"""
.. module:: catala_runtime
   :platform: Unix, Windows
   :synopsis: The Python bindings for the functions used in the generated Catala code
   :noindex:

.. moduleauthor:: Denis Merigoux <denis.merigoux@inria.fr>
"""

# This file should be in sync with compiler/runtime.{ml, mli} !

from gmpy2 import log2, mpz, mpq, mpfr, t_divmod, qdiv, f_div, sign  # type: ignore
import datetime
import calendar
import dateutil.relativedelta
from typing import NewType, List, Callable, Tuple, Optional, TypeVar, Iterable, Union, Any
from functools import reduce
from enum import Enum
import copy

Alpha = TypeVar('Alpha')
Beta = TypeVar('Beta')

# ============
# Type classes
# ============


class Integer:
    def __init__(self, value: Union[str, int]) -> None:
        self.value = mpz(value)

    def __add__(self, other: 'Integer') -> 'Integer':
        return Integer(self.value + other.value)

    def __sub__(self, other: 'Integer') -> 'Integer':
        return Integer(self.value - other.value)

    def __mul__(self, other: 'Integer') -> 'Integer':
        return Integer(self.value * other.value)

    def __truediv__(self, other: 'Integer') -> 'Decimal':
        return Decimal(self.value) / Decimal(other.value)

    def __neg__(self: 'Integer') -> 'Integer':
        return Integer(- self.value)

    def __lt__(self, other: 'Integer') -> bool:
        return self.value < other.value

    def __le__(self, other: 'Integer') -> bool:
        return self.value <= other.value

    def __gt__(self, other: 'Integer') -> bool:
        return self.value > other.value

    def __ge__(self, other: 'Integer') -> bool:
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
    def __init__(self, value: Union[str, int, float]) -> None:
        self.value = mpq(value)

    def __add__(self, other: 'Decimal') -> 'Decimal':
        return Decimal(self.value + other.value)

    def __sub__(self, other: 'Decimal') -> 'Decimal':
        return Decimal(self.value - other.value)

    def __mul__(self, other: 'Decimal') -> 'Decimal':
        return Decimal(self.value * other.value)

    def __truediv__(self, other: 'Decimal') -> 'Decimal':
        return Decimal(self.value / other.value)

    def __neg__(self: 'Decimal') -> 'Decimal':
        return Decimal(- self.value)

    def __lt__(self, other: 'Decimal') -> bool:
        return self.value < other.value

    def __le__(self, other: 'Decimal') -> bool:
        return self.value <= other.value

    def __gt__(self, other: 'Decimal') -> bool:
        return self.value > other.value

    def __ge__(self, other: 'Decimal') -> bool:
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
    def __init__(self, value: Integer) -> None:
        self.value = value

    def __add__(self, other: 'Money') -> 'Money':
        return Money(self.value + other.value)

    def __sub__(self, other: 'Money') -> 'Money':
        return Money(self.value - other.value)

    def __mul__(self, other: Decimal) -> 'Money':
        cents = self.value.value
        coeff = other.value
        # TODO: change, does not work with negative values. Must divide the
        # absolute values and then multiply by the resulting sign.
        rat_result = self.value.value * other.value
        out = Money(Integer(rat_result))
        res, remainder = t_divmod(rat_result.numerator, rat_result.denominator)
        if 2 * remainder >= rat_result.denominator:
            return Money(Integer(res + 1))
        else:
            return Money(Integer(res))

    def __truediv__(self, other: 'Money') -> Decimal:
        if isinstance(other, Money):
            return self.value / other.value
        elif isinstance(other, Decimal):
            return self * (1. / other.value)
        else:
            raise Exception("Dividing money and invalid obj")

    def __neg__(self: 'Money') -> 'Money':
        return Money(- self.value)

    def __lt__(self, other: 'Money') -> bool:
        return self.value < other.value

    def __le__(self, other: 'Money') -> bool:
        return self.value <= other.value

    def __gt__(self, other: 'Money') -> bool:
        return self.value > other.value

    def __ge__(self, other: 'Money') -> bool:
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
        return "${:.2}".format(self.value.value / 100)

    def __repr__(self) -> str:
        return f"Money({self.value.__repr__()})"


class Date:
    def __init__(self, value: datetime.date) -> None:
        self.value = value

    def __add__(self, other: 'Duration') -> 'Date':
        return Date(self.value + other.value)

    def __sub__(self, other: object) -> object:
        if isinstance(other, Date):
            return Duration(dateutil.relativedelta.relativedelta(days=(self.value - other.value).days))
        elif isinstance(other, Duration):
            return Date(self.value - other.value)
        else:
            raise Exception("Substracting date and invalid obj")

    def __lt__(self, other: 'Date') -> bool:
        return self.value < other.value

    def __le__(self, other: 'Date') -> bool:
        return self.value <= other.value

    def __gt__(self, other: 'Date') -> bool:
        return self.value > other.value

    def __ge__(self, other: 'Date') -> bool:
        return self.value >= other.value

    def __ne__(self, other: object) -> bool:
        if isinstance(other, Date):
            return self.value != other.value
        else:
            return True

    def __eq__(self, other: object) -> bool:
        if isinstance(other, Date):
            return self.value == other.value
        else:
            return False

    def __str__(self) -> str:
        return self.value.__str__()

    def __repr__(self) -> str:
        return f"Date({self.value.__repr__()})"


class Duration:
    def __init__(self, value: dateutil.relativedelta.relativedelta) -> None:
        self.value = value

    def __add__(self, other: 'Duration') -> 'Duration':
        return Duration(self.value + other.value)

    def __sub__(self, other: 'Duration') -> 'Duration':
        return Duration(self.value - other.value)

    def __neg__(self: 'Duration') -> 'Duration':
        return Duration(- self.value)

    def __truediv__(self, other: 'Duration') -> Decimal:
        x = self.value.normalized()
        y = other.value.normalized()
        if (x.years != 0 or y.years != 0 or x.months != 0 or y.months != 0):
            raise Exception("Can only divide durations expressed in days")
        else:
            return Decimal(x.days / y.days)

    def __mul__(self: 'Duration', rhs: Integer) -> 'Duration':
        return Duration(
            dateutil.relativedelta.relativedelta(years=self.value.years * rhs.value,
                                                 months=self.value.months * rhs.value,
                                                 days=self.value.days * rhs.value))

    def __lt__(self, other: 'Duration') -> bool:
        x = self.value.normalized()
        y = other.value.normalized()
        if (x.years != 0 or y.years != 0 or x.months != 0 or y.months != 0):
            raise Exception("Can only compare durations expressed in days")
        else:
            return x.days < y.days

    def __le__(self, other: 'Duration') -> bool:
        x = self.value.normalized()
        y = other.value.normalized()
        if (x.years != 0 or y.years != 0 or x.months != 0 or y.months != 0):
            raise Exception("Can only compare durations expressed in days")
        else:
            return x.days <= y.days

    def __gt__(self, other: 'Duration') -> bool:
        x = self.value.normalized()
        y = other.value.normalized()
        if (x.years != 0 or y.years != 0 or x.months != 0 or y.months != 0):
            raise Exception("Can only compare durations expressed in days")
        else:
            return x.days > y.days

    def __ge__(self, other: 'Duration') -> bool:
        x = self.value.normalized()
        y = other.value.normalized()
        if (x.years != 0 or y.years != 0 or x.months != 0 or y.months != 0):
            raise Exception("Can only compare durations expressed in days")
        else:
            return x.days >= y.days

    def __ne__(self, other: object) -> bool:
        if isinstance(other, Duration):
            return self.value != other.value
        else:
            return True

    def __eq__(self, other: object) -> bool:
        if isinstance(other, Duration):
            return self.value == other.value
        else:
            return False

    def __str__(self) -> str:
        return self.value.__str__()

    def __repr__(self) -> str:
        return f"Duration({self.value.__repr__()})"


class Unit:
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


class SourcePosition:
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
        return "in file {}, from {}:{} to {}:{}".format(
            self.filename, self.start_line, self.start_column, self.end_line, self.end_column)

# ==========
# Exceptions
# ==========


class EmptyError(Exception):
    pass


class AssertionFailed(Exception):
    def __init__(self, source_position: SourcePosition) -> None:
        self.source_position = source_position


class ConflictError(Exception):
    def __init__(self, source_position: SourcePosition) -> None:
        self.source_position = source_position


class NoValueProvided(Exception):
    def __init__(self, source_position: SourcePosition) -> None:
        self.source_position = source_position


class AssertionFailure(Exception):
    def __init__(self, source_position: SourcePosition) -> None:
        self.source_position = source_position


# ============================
# Constructors and conversions
# ============================

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
    res, remainder = t_divmod(m.value.value, 100)
    if remainder < 50:
        return Money(Integer(res * 100))
    else:
        return Money(Integer((res + sign(res)) * 100))


def money_of_decimal(d: Decimal) -> Money:
    """
    Warning: rounds to the nearest cent
    """
    return Money(Integer(mpz(d.value) * mpz(100)))


# --------
# Decimals
# --------


def decimal_of_string(d: str) -> Decimal:
    return Decimal(d)


def decimal_to_float(d: Decimal) -> float:
    return float(mpfr(d.value))


def decimal_of_float(d: float) -> Decimal:
    return Decimal(d)


def decimal_of_integer(d: Integer) -> Decimal:
    return Decimal(d.value)


def decimal_to_string(precision: int, i: Decimal) -> str:
    return "{1:.{0}}".format(precision, mpfr(i.value, precision * 10 // 2))


def decimal_round(q: Decimal) -> Decimal:
    """
    Implements the workaround by
    https://gmplib.org/list-archives/gmp-discuss/2009-May/003767.html
    """
    return Decimal(
        mpq(f_div(2*q.value.numerator + q.value.denominator,
            2*q.value.denominator), 1)  # type:ignore
    )


def decimal_of_money(m: Money) -> Decimal:
    return Decimal(mpq(qdiv(m.value.value, 100)))

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


def date_of_numbers(year: int, month: int, day: int) -> Date:
    # The datetime.date does not take year=0 as an entry, we trick it into
    # 1 in that case because year=0 cases don't care about the actual year
    return Date(datetime.date(year if year != 0 else 1, month, day))


def date_of_datetime(d: datetime.date) -> Date:
    return Date(d)


def first_day_of_month(d: Date) -> Date:
    return Date(datetime.date(d.value.year, d.value.month, 1))


def last_day_of_month(d: Date) -> Date:
    return Date(datetime.date(d.value.year, d.value.month, calendar.monthrange(d.value.year, d.value.month)[1]))

# ---------
# Durations
# ---------


def duration_of_numbers(years: int, months: int, days: int) -> Duration:
    return Duration(dateutil.relativedelta.relativedelta(years=years, months=months, days=days))


def duration_to_years_months_days(d: Duration) -> Tuple[int, int, int]:
    return (d.value.years, d.value.months, d.value.days)  # type: ignore


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


def list_reduce(f: Callable[[Alpha, Alpha], Alpha], dft: Alpha, l: List[Alpha]) -> Alpha:
    if l == []:
        return dft
    else:
        return reduce(f, l)


def list_length(l: List[Alpha]) -> Integer:
    return Integer(len(l))

# ========
# Defaults
# ========


def handle_default(
    pos: SourcePosition,
    exceptions: List[Callable[[Unit], Alpha]],
    just: Callable[[Unit], Alpha],
    cons: Callable[[Unit], Alpha]
) -> Alpha:
    acc: Optional[Alpha] = None
    for exception in exceptions:
        new_val: Optional[Alpha]
        try:
            new_val = exception(Unit())
        except EmptyError:
            new_val = None
        if acc is None:
            acc = new_val
        elif not (acc is None) and new_val is None:
            pass  # acc stays the same
        elif not (acc is None) and not (new_val is None):
            raise ConflictError(pos)
    if acc is None:
        if just(Unit()):
            return cons(Unit())
        else:
            raise EmptyError
    else:
        return acc


def handle_default_opt(
    pos: SourcePosition,
    exceptions: List[Optional[Any]],
    just: Callable[[Unit], bool],
    cons: Callable[[Unit], Optional[Alpha]]
) -> Optional[Alpha]:
    acc: Optional[Alpha] = None
    for exception in exceptions:
        if acc is None:
            acc = exception
        elif not (acc is None) and exception is None:
            pass  # acc stays the same
        elif not (acc is None) and not (exception is None):
            raise ConflictError(pos)
    if acc is None:
        b = just(Unit())
        if b:
            return cons(Unit())
        else:
            return None
    else:
        return acc


def no_input() -> Callable[[Unit], Alpha]:
    def closure(_: Unit):
        raise EmptyError
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
    def __init__(self, code: LogEventCode, io: Optional[LogIO], payload: Union[List[str], SourcePosition, Tuple[List[str], Alpha]]) -> None:
        self.code = code
        self.io = io
        self.payload = payload


log: List[LogEvent] = []


def reset_log():
    log = []


def retrieve_log() -> List[LogEvent]:
    return log


def log_variable_definition(headings: List[str], io: LogIO, value: Alpha) -> Alpha:
    log.append(LogEvent(LogEventCode.VariableDefinition, io,
               (headings, copy.deepcopy(value))))
    return value


def log_begin_call(headings: List[str], f: Callable[[Alpha], Beta], value: Alpha) -> Beta:
    log.append(LogEvent(LogEventCode.BeginCall, None, headings))
    return f(value)


def log_end_call(headings: List[str], value: Alpha) -> Alpha:
    log.append(LogEvent(LogEventCode.EndCall, None, headings))
    return value


def log_decision_taken(pos: SourcePosition, value: bool) -> bool:
    log.append(LogEvent(LogEventCode.DecisionTaken, None, pos))
    return value
