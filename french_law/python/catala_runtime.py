"""
.. module:: catala_runtime
   :platform: Unix, Windows
   :synopsis: The Python bindings for the functions used in the generated Catala code
   :noindex:

.. moduleauthor:: Denis Merigoux <denis.merigoux@inria.fr>
"""

# This file should be in sync with compiler/runtime.{ml, mli} !

from gmpy2 import log2, mpz, mpq, mpfr, mpc  # type: ignore
import datetime
import dateutil.relativedelta  # type: ignore
from typing import NewType, List, Callable, Tuple, Optional, TypeVar, Iterable

# =====
# Types
# =====

Integer = NewType('Integer', object)
Decimal = NewType('Decimal', object)
Money = NewType('Money', Integer)
Date = NewType('Date', datetime.date)
Duration = NewType('Duration', object)


class Unit:
    pass


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

# ==========
# Exceptions
# ==========


class EmptyError(Exception):
    pass


class AssertionFailed(Exception):
    pass


class ConflictError(Exception):
    pass


class NoValueProvided(Exception):
    def __init__(self, source_position: SourcePosition) -> None:
        self.source_position = SourcePosition


def raise_(ex):
    raise ex


class TryCatch:
    def __init__(self, fun, *args, **kwargs):
        self.fun = fun
        self.args = args
        self.kwargs = kwargs

        self.exception_types_and_handlers = []
        self.finalize = None

    def rescue(self, exception_types, handler):
        if not isinstance(exception_types, Iterable):
            exception_types = (exception_types,)

        self.exception_types_and_handlers.append((exception_types, handler))
        return self

    def ensure(self, finalize, *finalize_args, **finalize_kwargs):
        if self.finalize is not None:
            raise Exception('ensure() called twice')

        self.finalize = finalize
        self.finalize_args = finalize_args
        self.finalize_kwargs = finalize_kwargs
        return self

    def __call__(self):
        try:
            return self.fun(*self.args, **self.kwargs)

        except BaseException as exc:
            handler = self.find_applicable_handler(exc)
            if handler is None:
                raise
            return handler(exc)

        finally:
            if self.finalize is not None:
                self.finalize()

    def find_applicable_handler(self, exc):
        applicable_handlers = (
            handler
            for exception_types, handler in self.exception_types_and_handlers
            if isinstance(exc, exception_types)
        )
        return next(applicable_handlers, None)

# ============================
# Constructors and conversions
# ============================

# -----
# Money
# -----


def money_of_cents_string(v: str) -> Money:
    return Money(mpz(v))


def money_of_cents_int(v: int) -> Money:
    return Money(mpz(v))


def money_of_cents_integer(v: Integer) -> Money:
    return Money(mpz(v))


def money_to_float(m: Money) -> float:
    return float(mpfr(mpq(m, 100)))


def money_to_string(m: Money) -> str:
    return str(money_to_float(m))


def money_to_cents(m: Money) -> Integer:
    return m

# --------
# Decimals
# --------


def decimal_of_string(d: str) -> Decimal:
    return Decimal(mpq(d))


def decimal_to_float(d: Decimal) -> float:
    return float(mpfr(d))


def decimal_of_float(d: float) -> Decimal:
    return Decimal(mpq(d))


def decimal_of_integer(d: Integer) -> Decimal:
    return Decimal(mpq(d))


def decimal_to_string(precision: int, i: Decimal) -> str:
    return "{1:.{0}}".format(precision, mpfr(i, precision * 10 // 2))

# --------
# Integers
# --------


def integer_of_string(s: str) -> Integer:
    return Integer(mpz(s))


def integer_to_string(d: Integer) -> str:
    return str(d)


def integer_of_int(d: int) -> Integer:
    return Integer(mpz(d))


def integer_to_int(d: Integer) -> int:
    return int(d)  # type: ignore


def integer_exponentiation(i: Integer, e: int) -> Integer:
    return i ** e  # type: ignore


def integer_log2(i: Integer) -> int:
    return int(log2(i))

# -----
# Dates
# -----


def day_of_month_of_date(d: Date) -> Integer:
    return integer_of_int(d.day)


def month_number_of_date(d: Date) -> Integer:
    return integer_of_int(d.month)


def year_of_date(d: Date) -> Integer:
    return integer_of_int(d.year)


def date_to_string(d: Date) -> str:
    return "{}".format(d)


def date_of_numbers(year: int, month: int, day: int) -> Date:
    return Date(datetime.date(year, month, day))

# ---------
# Durations
# ---------


def duration_of_numbers(years: int, months: int, days: int) -> Duration:
    return Duration(dateutil.relativedelta.relativedelta(years=years, months=months, days=days))


def duration_to_years_months_days(d: Duration) -> Tuple[int, int, int]:
    return (d.years, d.months, d.days)  # type: ignore


def duration_to_string(s: Duration) -> str:
    return "{}".format(s)

# ========
# Defaults
# ========


Alpha = TypeVar('Alpha')


def handle_default(
    exceptions: List[Callable[[], Alpha]],
    just: Callable[[], Alpha],
    cons: Callable[[], Alpha]
) -> Alpha:
    acc: Optional[Alpha] = None
    for exception in exceptions:
        new_val: Optional[Alpha]
        try:
            new_val = exception()
        except EmptyError:
            new_val = None
        if acc is None:
            acc = new_val
        elif not (acc is None) and new_val is None:
            pass  # acc stays the same
        elif not (acc is None) and not (new_val is None):
            raise ConflictError
    if acc is None:
        if just():
            return cons()
        else:
            raise EmptyError
    else:
        return acc


def no_input() -> Callable[[], Alpha]:
    # From https://stackoverflow.com/questions/8294618/define-a-lambda-expression-that-raises-an-exception
    return (_ for _ in ()).throw(EmptyError)
