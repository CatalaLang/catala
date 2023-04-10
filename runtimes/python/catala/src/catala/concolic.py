"""

This file implements a concolic execution engine for Catala programs
compiled in a modified Python backend.

The main algorithm in this file is from the DART paper:
    Patrice Godefroid, Nils Klarlund, and Koushik Sen. 2005.
    DART: Directed Automated Random Testing. In Proceedings of
    the 2005 ACM SIGPLAN conference on Programming Language
    Design and Implementation (PLDI '05).
    DOI: https://doi.org/10.1145/1065010.1065036

Copyright (c) 2023 Rohan Padhye <rohanpadhye@cmu.edu>

Licensed under the Apache License, Version 2.0 (the "License"); you may not
use this file except in compliance with the License. You may obtain a copy of
the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
License for the specific language governing permissions and limitations under
the License.

"""

import z3
import traceback
from inspect import currentframe, signature
from dataclasses import dataclass

import calendar
import datetime
import dateutil.relativedelta
import locale
from typing import NewType, List, Dict, Callable, Tuple, Optional, TypeVar, Iterable, Union, Any
from functools import reduce
from enum import Enum
from os.path import basename

from catala.runtime import *
import gmpy2
from fractions import Fraction

# ============
# Type classes
# ============

class ConcolicValue:
    pass

class ConcolicBoolean(ConcolicValue):
    def __init__(self, value: bool, expr: z3.BoolRef) -> None:
        self.value = value
        self.expr = z3.simplify(expr)

    def __and__(self, other: 'ConcolicBoolean') -> 'ConcolicBoolean':
        return ConcolicBoolean(self.value and other.value, z3.And(self.expr, other.expr))

    def __or__(self, other: 'ConcolicBoolean') -> 'ConcolicBoolean':
        return ConcolicBoolean(self.value or other.value, z3.Or(self.expr, other.expr))

    def __neg__(self) -> 'ConcolicBoolean':
        return ConcolicBoolean(self.value, z3.Not(self.expr))

    def __eq__(self, other: object) -> 'ConcolicBoolean':
        if isinstance(other, ConcolicBoolean):
            return ConcolicBoolean(self.value == other.value, self.expr == other.expr)
        else:
            raise Exception("Not expecting comparison with non-concolic type")
            # return ConcolicBoolean(False, z3.BoolVal(False))

    def __ne__(self, other) -> 'ConcolicBoolean':
        if isinstance(other, ConcolicBoolean):
            return ConcolicBoolean(self.value != other.value, self.expr != other.expr)
        else:
            raise Exception("Not expecting comparison with non-concolic type")
            # return ConcolicBoolean(True, z3.BoolVal(True))

    def __bool__(self) -> bool:
        concolic_guard(self.expr if self.value else z3.Not(self.expr), self.value)
        return self.value

    def __str__(self) -> str:
        return self.value.__str__()

    def __repr__(self) -> str:
        return f"ConcolicBoolean({self.value.__repr__()}, {self.expr.__repr__()})"

# Constants for True and False
ConcolicBoolean.T = ConcolicBoolean(True, z3.BoolVal(True))
ConcolicBoolean.F = ConcolicBoolean(False, z3.BoolVal(False))

class ConcolicInteger(ConcolicValue):
    def __init__(self, value: Union[str, int], expr: z3.ArithRef = None) -> None:
        self.value = gmpy2.mpz(value)
        self.expr = z3.simplify(expr)

    def __add__(self, other: 'ConcolicInteger') -> 'ConcolicInteger':
        return ConcolicInteger(self.value + other.value, self.expr + other.expr)

    def __sub__(self, other: 'ConcolicInteger') -> 'ConcolicInteger':
        return ConcolicInteger(self.value - other.value, self.expr - other.expr)

    def __mul__(self, other: 'ConcolicInteger') -> 'ConcolicInteger':
        return ConcolicInteger(self.value * other.value, self.expr * other.expr)

    def __truediv__(self, other: 'ConcolicInteger') -> 'ConcolicDecimal':
        """
        Warning: type change from integers to decimal (z3 ints to reals)
        """
        return ConcolicDecimal(self.value, z3.ToReal(self.expr)) / ConcolicDecimal(other.value, z3.ToReal(other.expr))

    def __neg__(self: 'ConcolicInteger') -> 'ConcolicInteger':
        return ConcolicInteger(-self.value, -self.expr)

    def __lt__(self, other: 'ConcolicInteger') -> 'ConcolicBoolean':
        return ConcolicBoolean(self.value < other.value, self.expr < other.expr)

    def __le__(self, other: 'ConcolicInteger') -> 'ConcolicBoolean':
        return ConcolicBoolean(self.value <= other.value, self.expr <= other.expr)

    def __gt__(self, other: 'ConcolicInteger') -> 'ConcolicBoolean':
        return ConcolicBoolean(self.value > other.value, self.expr > other.expr)

    def __ge__(self, other: 'ConcolicInteger') -> 'ConcolicBoolean':
        return ConcolicBoolean(self.value >= other.value, self.expr >= other.expr)

    def __ne__(self, other: object) -> 'ConcolicBoolean':
        if isinstance(other, ConcolicInteger):
            return ConcolicBoolean(self.value != other.value, self.expr != other.expr)
        else:
            raise Exception("Not expecting comparison with non-concolic type")
            # return ConcolicBoolean.T

    def __eq__(self, other: object) -> 'ConcolicBoolean':
        if isinstance(other, ConcolicInteger):
            return ConcolicBoolean(self.value == other.value, self.expr == other.expr)
        else:
            raise Exception("Not expecting comparison with non-concolic type")
            # return ConcolicBoolean.F

    def __str__(self) -> str:
        return self.value.__str__()

    def __repr__(self) -> str:
        return f"ConcolicInteger({self.value.__repr__()}, {self.expr.__repr__()})"

class ConcolicDecimal(ConcolicValue):
    def __init__(self, value: Union[str, int, float, Fraction], expr: z3.ArithRef) -> None:
        self.value = gmpy2.mpq(value)
        self.expr = z3.simplify(expr)

    def __add__(self, other: 'ConcolicDecimal') -> 'ConcolicDecimal':
        return ConcolicDecimal(self.value + other.value, self.expr + other.expr)

    def __sub__(self, other: 'ConcolicDecimal') -> 'ConcolicDecimal':
        return ConcolicDecimal(self.value - other.value, self.expr - other.expr)

    def __mul__(self, other: 'ConcolicDecimal') -> 'ConcolicDecimal':
        return ConcolicDecimal(self.value * other.value, self.expr * other.expr)

    def __truediv__(self, other: 'ConcolicDecimal') -> 'ConcolicDecimal':
        return ConcolicDecimal(self.value / other.value, self.expr / other.expr)

    def __neg__(self: 'ConcolicDecimal') -> 'ConcolicDecimal':
        return ConcolicDecimal(-self.value, -self.expr)

    def __lt__(self, other: 'ConcolicDecimal') -> 'ConcolicBoolean':
        return ConcolicBoolean(self.value < other.value, self.expr < other.expr)

    def __le__(self, other: 'ConcolicDecimal') -> 'ConcolicBoolean':
        return ConcolicBoolean(self.value <= other.value, self.expr <= other.expr)

    def __gt__(self, other: 'ConcolicDecimal') -> 'ConcolicBoolean':
        return ConcolicBoolean(self.value > other.value, self.expr > other.expr)

    def __ge__(self, other: 'ConcolicDecimal') -> 'ConcolicBoolean':
        return ConcolicBoolean(self.value >= other.value, self.expr >= other.expr)

    def __ne__(self, other: object) -> 'ConcolicBoolean':
        if isinstance(other, ConcolicDecimal):
            return ConcolicBoolean(self.value != other.value, self.expr != other.expr)
        else:
            raise Exception("Not expecting comparison with non-concolic type")
            # return ConcolicBoolean.T

    def __eq__(self, other: object) -> 'ConcolicBoolean':
        if isinstance(other, ConcolicDecimal):
            return ConcolicBoolean(self.value == other.value, self.expr == other.expr)
        else:
            raise Exception("Not expecting comparison with non-concolic type")
            # return ConcolicBoolean.F

    def __str__(self) -> str:
        return self.value.__str__()

    def __repr__(self) -> str:
        return f"ConcolicDecimal({self.value.__repr__()}, {self.expr.__repr__()})"



class ConcolicMoney(ConcolicValue):
    def __init__(self, value: ConcolicInteger) -> None:
        self.value = value

    def __add__(self, other: 'ConcolicMoney') -> 'ConcolicMoney':
        return ConcolicMoney(self.value + other.value)

    def __sub__(self, other: 'ConcolicMoney') -> 'ConcolicMoney':
        return ConcolicMoney(self.value - other.value)

    def __mul__(self, other: ConcolicDecimal) -> 'ConcolicMoney':
        """
        Warning: rounds to the nearest cent
        """
        res_value = round(self.value.value * other.value)
        res_expr = z3.ToInt(self.value.expr * other.expr + 0.5)
        return ConcolicMoney(ConcolicInteger(res_value, res_expr))

    def __truediv__(self, other: 'ConcolicMoney') -> 'ConcolicDecimal':
        if isinstance(other, ConcolicMoney):
            return self.value / other.value
        elif isinstance(other, ConcolicDecimal):
            return self * (1. / other.value)
        else:
          raise Exception("Dividing money and invalid obj")

    def __neg__(self: 'ConcolicMoney') -> 'ConcolicMoney':
        return ConcolicMoney(- self.value)

    def __lt__(self, other: 'ConcolicMoney') -> 'ConcolicBoolean':
        return self.value < other.value

    def __le__(self, other: 'ConcolicMoney') -> 'ConcolicBoolean':
        return self.value <= other.value

    def __gt__(self, other: 'ConcolicMoney') -> 'ConcolicBoolean':
        return self.value > other.value

    def __ge__(self, other: 'ConcolicMoney') -> 'ConcolicBoolean':
        return self.value >= other.value

    def __ne__(self, other: object) -> 'ConcolicBoolean':
        if isinstance(other, ConcolicMoney):
            return self.value != other.value
        else:
            raise Exception("Not expecting comparison with non-concolic type")
            # return ConcolicBoolean.T

    def __eq__(self, other: object) -> 'ConcolicBoolean':
        if isinstance(other, ConcolicMoney):
            return self.value == other.value
        else:
            raise Exception("Not expecting comparison with non-concolic type")
            # return ConcolicBoolean.F

    # Locale for currency formatting (only used for string representations)
    locale = None

    def __str__(self) -> str:
        if ConcolicMoney.locale is not None:
            prev_locale = locale.getlocale(locale.LC_MONETARY)
            locale.setlocale(locale.LC_MONETARY, ConcolicMoney.locale)
            loc_string = locale.currency(self.value.value/100, grouping=True)
            locale.setlocale(locale.LC_MONETARY, prev_locale)
            return loc_string
        return f"¢({self.value.__str__()})"

    def __repr__(self) -> str:
        return f"ConcolicMoney({self.value.__repr__()})"

# Symbolic dates are represented as the number of days since March 1, 2000
# The choice of epoch date allows us to reason about leap years quite efficiently
_concolic_date_epoch = datetime.date(2000, 3, 1)

class ConcolicDate(ConcolicValue):
    def __init__(self, value: Union[datetime.date, int], expr: z3.ArithRef) -> None:
        self.value = value if isinstance(value, datetime.date) else concolic_datetime_of_days_since_epoch(value)
        self.expr = z3.simplify(expr)

    def __add__(self, duration: 'ConcolicDuration') -> 'ConcolicDate':
        # TODO: Check that `duration` is purely concrete; no symbolic inputs in it

        new_value = self.value + duration.value

        # Compute symbolic addition of date to duration step by step
        new_expr = self.expr
        # First, add years
        if duration.value.years != 0:
            # Logic for adding symbolic dates is a bit complex due to leap years.
            # Since all symbolic dates are represented as days since March 1, 2000, we know
            # that every 1461th day is a leap year. So, when adding a symbolic date to a duration
            # in years, we have to identify (1) where the current date is the in the 4-year cycle,
            # and (2) how many four-year cycles do we cross-over.
            cur_leap_idx = (self.expr % 1461) / 365 # A number in [0, 3] (or 4 if date is 29-Feb)
            cur_leap_idx = cur_leap_idx % 4         # Once at 29-Feb, we have wrapped around
            leap_days_to_add = (cur_leap_idx + duration.years_expr) / 4
            new_expr = self.expr + 365 * duration.years_expr + leap_days_to_add
        # We don't yet support concolic addition of months, because the logic is even more complex
        if duration.value.months != 0:
            global _expect_surprises
            _expect_surprises = True
            return concolic_date_of_datetime(new_value)
        # Finally, days can be added directly
        if duration.value.days != 0:
            new_expr = new_expr + duration.days_expr

        return ConcolicDate(new_value, new_expr)

    def __sub__(self, other: Union['ConcolicDuration', 'ConcolicDate']) -> Union['ConcolicDate', 'ConcolicDuration']:
        if isinstance(other, ConcolicDate):
            # A difference between two days is always expressed in days (both concrete and symbolic)
            return ConcolicDuration(dateutil.relativedelta.relativedelta(days=(self.value - other.value).days),
                years_expr=0, months_expr=0, days_expr = self.expr - other.expr)
        elif isinstance(other, ConcolicDuration):
            raise NotImplementedError("Concolic subtraction of dates with duration not yet supported")
        else:
            raise Exception("Substracting date and invalid obj")

    def __lt__(self, other: 'ConcolicDate') -> 'ConcolicBoolean':
        return ConcolicBoolean(self.value < other.value, self.expr < other.expr)

    def __le__(self, other: 'ConcolicDate') -> 'ConcolicBoolean':
        return ConcolicBoolean(self.value <= other.value, self.expr <= other.expr)

    def __gt__(self, other: 'ConcolicDate') -> 'ConcolicBoolean':
        return ConcolicBoolean(self.value > other.value, self.expr > other.expr)

    def __ge__(self, other: 'ConcolicDate') -> 'ConcolicBoolean':
        return ConcolicBoolean(self.value >= other.value, self.expr >= other.expr)

    def __ne__(self, other: object) -> 'ConcolicBoolean':
        if isinstance(other, ConcolicDate):
            return ConcolicBoolean(self.value != other.value, self.expr != other.expr)
        else:
            raise Exception("Not expecting comparison with non-concolic type")
            # return ConcolicBoolean.T

    def __eq__(self, other: object) -> 'ConcolicBoolean':
        if isinstance(other, ConcolicDate):
            return ConcolicBoolean(self.value == other.value, self.expr == other.expr)
        else:
            raise Exception("Not expecting comparison with non-concolic type")
            # return ConcolicBoolean.F

    def __str__(self) -> str:
        return self.value.__str__()

    def __repr__(self) -> str:
        return f"ConcolicDate({self.value.__repr__()}, {self.expr.__repr__()})"

class ConcolicDuration(ConcolicValue):
    def __init__(self, value: dateutil.relativedelta.relativedelta, years_expr: z3.ArithRef, \
        months_expr: z3.ArithRef, days_expr: z3.ArithRef) -> None:
        self.value = value
        self.years_expr = years_expr
        self.months_expr = months_expr
        self.days_expr = days_expr

    def __add__(self, other: 'ConcolicDuration') -> 'ConcolicDuration':
        return ConcolicDuration(self.value + other.value, self.years_expr + other.year_expr,
            self.months_expr + other.months_expr, self.days_expr + other.days_expr)

    def __sub__(self, other: 'ConcolicDuration') -> 'ConcolicDuration':
        return ConcolicDuration(self.value - other.value, self.years_expr - other.year_expr,
            self.months_expr - other.months_expr, self.days_expr - other.days_expr)

    def __neg__(self: 'ConcolicDuration') -> 'ConcolicDuration':
        return ConcolicDuration(- self.value, -self.years_expr, -self.months_expr, -self.days_expr)

    def __truediv__(self, other: 'ConcolicDuration') -> 'ConcolicDuration':
        x = self.value.normalized()
        y = other.value.normalized()
        if (x.years != 0 or y.years != 0 or x.months != 0 or y.months != 0):
            raise Exception("Can only divide durations expressed in days")
        else:
            # Reuse the integer true-division logic from ConcolicInteger instead of repeating it here
            return ConcolicInteger(x.days, self.days_expr) / ConcolicInteger(y.days, other.days_expr)

    def __mul__(self: 'ConcolicDuration', rhs: ConcolicInteger) -> 'ConcolicDuration':
        return ConcolicDuration(
            dateutil.relativedelta.relativedelta(years=self.value.years * rhs.value,
                                                 months=self.value.months * rhs.value,
                                                 days=self.value.days * rhs.value),
            self.years_expr * rhs.expr, self.months_expr * rhs.expr, self.days_expr * rhs.expr)

    def __lt__(self, other: 'ConcolicDuration') -> 'ConcolicBoolean':
        x = self.value.normalized()
        y = other.value.normalized()
        if (x.years != 0 or y.years != 0 or x.months != 0 or y.months != 0):
            raise Exception("Can only compare durations expressed in days")
        else:
            return ConcolicBoolean(x.days < y.days, self.days_expr < other.days_expr)

    def __le__(self, other: 'ConcolicDuration') -> 'ConcolicBoolean':
        x = self.value.normalized()
        y = other.value.normalized()
        if (x.years != 0 or y.years != 0 or x.months != 0 or y.months != 0):
            raise Exception("Can only compare durations expressed in days")
        else:
            return ConcolicBoolean(x.days <= y.days, self.days_expr <= other.days_expr)

    def __gt__(self, other: 'ConcolicDuration') -> 'ConcolicBoolean':
        x = self.value.normalized()
        y = other.value.normalized()
        if (x.years != 0 or y.years != 0 or x.months != 0 or y.months != 0):
            raise Exception("Can only compare durations expressed in days")
        else:
            return ConcolicBoolean(x.days > y.days, self.days_expr > other.days_expr)

    def __ge__(self, other: 'ConcolicDuration') -> 'ConcolicBoolean':
        x = self.value.normalized()
        y = other.value.normalized()
        if (x.years != 0 or y.years != 0 or x.months != 0 or y.months != 0):
            raise Exception("Can only compare durations expressed in days")
        else:
            return ConcolicBoolean(x.days >= y.days, self.days_expr >= other.days_expr)

    def __ne__(self, other: 'ConcolicDuration') -> 'ConcolicBoolean':
        if isinstance(other, ConcolicDuration):
            x = self.value.normalized()
            y = other.value.normalized()
            if (x.years != 0 or y.years != 0 or x.months != 0 or y.months != 0):
                raise Exception("Can only compare durations expressed in days")
            return ConcolicBoolean(x.days != y.days, self.days_expr != other.days_expr)
        else:
            raise Exception("Not expecting comparison with non-concolic type")
            # return ConcolicBoolean.T

    def __eq__(self, other: 'ConcolicDuration') -> 'ConcolicBoolean':
        if isinstance(other, ConcolicDuration):
            x = self.value.normalized()
            y = other.value.normalized()
            if (x.years != 0 or y.years != 0 or x.months != 0 or y.months != 0):
                raise Exception("Can only compare durations expressed in days")
            return ConcolicBoolean(x.days == y.days, self.days_expr == other.days_expr)
        else:
            raise Exception("Not expecting comparison with non-concolic type")
            # return ConcolicBoolean.F

    def __str__(self) -> str:
        return self.value.__str__()

    def __repr__(self) -> str:
        return f"ConcolicDuration({self.value.__repr__()}, {self.years_expr.__repr__()}, {self.months_expr.__repr__()}, {self.days_expr.__repr__()})"

# ============================
# Constructors and conversions
# ============================

def concolic_integer_of_string(s: str) -> ConcolicInteger:
    return ConcolicInteger(s, z3.IntVal(s))

def concolic_integer_of_int(d: int) -> ConcolicInteger:
    return ConcolicInteger(d, z3.IntVal(d))

def concolic_money_of_cents_string(v: str) -> ConcolicMoney:
    return ConcolicMoney(ConcolicInteger(v, z3.IntVal(v)))

def concolic_money_of_cents_int(v: int) -> ConcolicMoney:
    return ConcolicMoney(ConcolicInteger(v, z3.IntVal(v)))

def concolic_money_of_units_int(v: int) -> Money:
    return ConcolicMoney(ConcolicInteger(v * 100, z3.IntVal(v * 100)))

def concolic_date_of_numbers(year: int, month: int, day: int) -> ConcolicDate:
    # The datetime.date does not take year=0 as an entry, we trick it into
    # 1 in that case because year=0 cases don't care about the actual year
    if year == 0:
        year = 1

    date = datetime.date(year, month, day)
    return concolic_date_of_datetime(date)

def concolic_date_of_datetime(date: datetime.date) -> ConcolicDate:
    days_since_epoch = (date - _concolic_date_epoch).days

    return ConcolicDate(date, z3.IntVal(days_since_epoch))

def concolic_datetime_of_days_since_epoch(days_since_epoch: int) -> datetime.date:
    return _concolic_date_epoch + datetime.timedelta(days_since_epoch)

def concolic_duration_of_numbers(years: int, months: int, days: int) -> ConcolicDuration:
    delta = dateutil.relativedelta.relativedelta(years=years, months=months, days=days)
    return ConcolicDuration(delta, z3.IntVal(years), z3.IntVal(months), z3.IntVal(days))

def concolic_decimal_of_float(d: float) -> ConcolicDecimal:
    return ConcolicDecimal(d, z3.RealVal(d))

def concolic_decimal_of_string(d: str) -> ConcolicDecimal:
    return ConcolicDecimal(d, z3.RealVal(d))

def concolic_decimal_of_fraction(f: Fraction) -> ConcolicDecimal:
    return ConcolicDecimal(f, z3.RealVal(f))

# ==================
# Concolic Execution
# ==================

@dataclass
class ConcolicVar:
    """ A description of an input variable which will be managed by the concolic engine. """
    name: str
    type: str                                             # A catala type ('Integer', 'Money', 'Decimal', 'Date', 'Duration')
    initial: Any = None                                   # Depends on type

class ConcolicInput:
    """
        A single object of this class is passed into the runnable given to concolic_run.
        The fields of this class are created dynamically based on the list of concolic vars
        given to `concolic_run`.
    """
    def __init__(self, values: Dict[str, ConcolicValue]):
        for name, value in values.items():
            setattr(self, name, value) # Set field dynamically

@dataclass
class ConcolicPathRecord:
    file: str
    line: int
    branch: bool
    done: bool

    def __repr__(self) -> str:
        return "%s:%d[%s]%s" % (basename(self.file), self.line, self.branch, '!' if self.done else '?')

class ConcolicException(Exception):
    pass

class IncompletenessException(Exception):
    pass

class ConcolicLogLevel(Enum):
    NONE    = 0
    ERROR   = 1
    INFO    = 2
    VERBOSE = 3
    DEBUG   = 4

# Globals (don't export)
_symbols: Dict[str, z3.ExprRef] = None
_current_path: List[z3.ExprRef] = None
_path_record: List[ConcolicPathRecord] = None
_solver: z3.Solver = None
_expect_surprises = False

def concolic_wrap(val: Any, var: ConcolicVar) -> ConcolicValue:
    """ Wraps a native value or Z3 model evaluation result into a concolic object """

    # Decode z3 values for concrete part (if value is not already native)
    if isinstance(val, z3.IntNumRef):
        val = val.as_long()
    elif isinstance(val, z3.RatNumRef):
        val = val.as_fraction()

    # Get z3 symbol for symbolic part
    symbol = _symbols[var.name]

    # Create concolic object based on type
    if var.type == 'Integer':
        return ConcolicInteger(val, symbol)
    elif var.type == 'Money':
        return ConcolicMoney(ConcolicInteger(val, symbol))
    elif var.type == 'Decimal':
        return ConcolicDecimal(val, symbol)
    elif var.type == 'Date':
        return ConcolicDate(val, symbol)
    elif var.type == 'Duration':
        # Symbolic durations can only represent days
        return ConcolicDuration(val, years_expr=0, months_expr=0, days_expr=symbol)

    raise ValueError("Unknown type %s for var %s" % (var.type, var.name))

def concolic_init(vars: List[ConcolicVar],
    constraints: List[Callable[ConcolicInput, ConcolicBoolean]],
    preferences: List[Callable[ConcolicInput, ConcolicBoolean]]) -> ConcolicInput:
    """
        Initializes the state for concolic execution, given a list of concolic vars,
        hard constraints, and soft constraints.

        Returns the ConcolicInput object with initial concrete values set.

        """
    global _symbols, _path_record, _current_path, _solver, _expect_surprises

    def create_symbol(var: ConcolicVar) -> z3.ExprRef:
        """ Creates and returns a Z3 symbol for a given concolic var, based on its type """
        if var.type == 'Integer' or var.type == 'Money' or \
            var.type == 'Date' or var.type == 'Duration':  # these are represented in 'days'
            return z3.Int(var.name)
        elif var.type == 'Decimal':
            return z3.Real(var.name)
        else:
            raise ValueError("Unknown type %s for var %s" % (var.type, var.name))

    # Create z3 symbols for all the concolic vars
    _symbols = {v.name: create_symbol(v) for v in vars}

    # Create the first concolic input with initial values
    input = ConcolicInput({v.name: concolic_wrap(v.initial or 0, v) for v in vars})

    # Initialize max-sat solver
    _solver = z3.Optimize()

    # Add hard and soft constraints
    input_for_constraints = ConcolicInput(_symbols)
    for constraint in constraints:
        _solver.add(constraint(input_for_constraints))
    for preference in preferences:
        _solver.add_soft(preference(input_for_constraints))

    # Ensure that all dates are within 20th or 21st century (since we don't support 1900/2100 being non-leap years)
    for v in vars:
        if v.type == 'Date':
            _solver.add(_symbols[v.name] <  10000)
            _solver.add(_symbols[v.name] > -10000)

    # Save initial constraints
    _solver.push()

    # Initialize path book-keeping
    _path_record = []
    _current_path = []
    _expect_surprises = False

    # Return the initial input
    return input

def concolic_reset_solver() -> None:
    """ Resets the solver to only the initial hard + soft constraints on vars """
    _solver.pop()   # Restore initial constraints
    _solver.push()  # Save initial constraints

def concolic_guard(cond_expr: z3.BoolRef, branch: bool) -> None:
    """ Asserts that a given condition is either true or false in this path """

    # Since this is called from instrumentation, we need a bailout
    if _solver is None:
        return # Concolic testing is not running

    # Add to current path constraint and ensure that it is feasible
    cond_expr = z3.simplify(cond_expr)
    _solver.add(cond_expr)
    assert _solver.check(), "Concolic guard let to UNSAT path: %s" % str(_solver)

    # Get line number of guard
    frame = currentframe().f_back.f_back
    file = frame.f_code.co_filename
    line = frame.f_lineno

    # We are just seeing the k-th branch in this execution
    k = len(_current_path)

    # Append to current path
    _current_path.append(cond_expr)

    # Check if we have an expected k-th branch in the path record else add to it
    if k < len(_path_record):
        if k == len(_path_record)-1:
            # We just got to the last negated guard
            if _path_record[k].branch == branch:
                # We got to an unexpected branch
                if _expect_surprises:
                    raise IncompletenessException()
                raise ConcolicException(("Unsoundness! Current path is %s and I'm back at %s:%d[%s], " +\
                    "but I was expecting to have negated this branch") % (_current_path, file, line, branch))
            else:
                _path_record[k].file = file
                _path_record[k].line = line
                _path_record[k].branch = branch
                _path_record[k].done = True
        elif _path_record[k].branch != branch:
            # We got to an unexpected branch
            if _expect_surprises:
                raise IncompletenessException()
            raise ConcolicException(("Unsoundness! Current path is %s and I'm at %s:%d[%s], " +\
                "but I was expecting to go to line %s:%d[%s].") % (_current_path, file, line, branch, \
                _path_record[k].file, _path_record[k].line, _path_record[k].branch))
        # else: do nothing, we saw an expected branch
    else:
        _path_record.append(ConcolicPathRecord(file, line, branch, False)) # Set `done`=False initially

def concolic_inputs_str(input: ConcolicInput) -> None:
    """ Returns a string representation of inputs in concrete form """
    return ", ".join([str(k) + "=" + str(v) for k,v in input.__dict__.items()])

# Top-level runner
def concolic_run(func: Callable[ConcolicInput, Any], vars: List[ConcolicVar],
    constraints: List[Callable[ConcolicInput, ConcolicBoolean]] = None,
    preferences: List[Callable[ConcolicInput, ConcolicBoolean]] = None,
    logging: ConcolicLogLevel = ConcolicLogLevel.NONE, currency_locale:str = None) -> Tuple[int, bool]:
    """
        Performs systematic testing of a given runnable and returns the number of paths + whether a bug was found.

        Args:
            func: A runnable that expects a `ConcolicInput` parameter. A `ConcolicInput` is an object whose
                fields correspond to the names of `vars`. The runnable is the test method which will be
                executed with concolic inputs, once per execution path. This function can return any value;
                the returned value will be printed on screen when using log level of `INFO` or above.
            vars: A list of input variables whose values will be generated automatically using concolic execution.
                Each `ConcolicVar` contains a `name` string, a `type` string (one of [`Integer`, `Money`, `Decimal`, `Date`]),
                and an `initial` value.
            constraints: A collection of constraints that must be satisfied by the input variables. Each constraint
                is represented as a function (typically a lambda) whose parameter is a `ConcolicInput` object having
                each field be a Z3 variable corresponding to input variables.
            preferences: Similar to `constraints`, but this collection represents constraints that we would like
                to satisfy on a best-effort basis. Typically, these include constraints for making values "look nice",
                such as asking money values to be a nice round number or expecting dates to be within some range close
                to the present day.
            logging: A `ConcolicLogLevel` that determines how much output is printed to stdout during the concolic testing
                process. Options (in increasing order) are `ConcolicLogLevel.NONE`, `.ERROR`, `.INFO`, `.VERBOSE`, `.DEBUG`.
            currency_locale: An optional config string representing the locale for formatting currency values when printing.
                Examples include `en_US` or `fr_FR`. If not specified, money values are printed in cents with no thousands
                separator.

        Returns:
            A tuple whose first component is the number of distinct execution paths (and corresponding test inputs) exercised
            by the concolic execution engine, the second component is a boolean which indicates if all execution paths
            were systematically explored (which is only `False` if the executor encountered some logic that was not modeled
            by the theorem prover), and the third component is a boolean which is `True` if and only if
            any test failed (for example, because of an `AssertionError` or a `ConflictError`).

    """
    global _current_path, _path_record, _solver, _expect_surprises

    # Initialize state and inputs
    input = concolic_init(vars, constraints or [], preferences or [])

    # Configure logger
    def log(level:ConcolicLogLevel, msg: str):
        if logging.value >= level.value:
            print(msg)
    INFO = ConcolicLogLevel.INFO
    VERBOSE = ConcolicLogLevel.VERBOSE
    ERROR = ConcolicLogLevel.ERROR
    DEBUG = ConcolicLogLevel.DEBUG

    # Set locale for use by ConcolicMoney.__str__()
    ConcolicMoney.locale = currency_locale

    # Run the DART algorithm for concolic execution
    total_paths = 0
    complete = True
    bug_found = False

    while True:
        try :
            log(INFO, "") # Newline between inputs
            log(INFO, "Running with inputs: %s" % concolic_inputs_str(input))
            total_paths += 1
            out = func(input)
            log(INFO, "... Output = %s" % str(out))
        except (AssertionFailed, AssertionFailure,
            ConflictError, NoValueProvided) as e:
            log(ERROR, "*** %s encountered in %s!" % (e.__class__.__name__, e.source_position))
            log(ERROR, "****** Offending inputs are: %s" % concolic_inputs_str(input))
            bug_found = True
        except IncompletenessException:
            log(INFO, "*** Scratch that. This input does not correspond to an interesting path. " +
                "Warning: Results will be incomplete. ")
            total_paths -= 1
            complete = False
            _expect_surprises = False
        finally:
            log(VERBOSE, "... Path collected: %s" % _current_path)
            log(DEBUG,   "... Path Record: %s" % _path_record)

        # Figure out the next guard to negate
        next = len(_current_path)-1
        while True:
            while next >= 0 and _path_record[next].done:
                next = next - 1

            if next == -1:
                if complete:
                    log(INFO, "Concolic execution complete! %d paths explored." % total_paths)
                else:
                    log(INFO, "Concolic execution finished, but not complete. %d paths explored." % total_paths)
                # TODO: Actually do a random restart if there was incompleteness
                return total_paths, complete, bug_found
            else:
                log(DEBUG, "...... next idx=%d" % next)
                # Create a new path constraint up to `next` with the condition at index `next` negated
                _current_path = _current_path[:next] + [z3.Not(_current_path[next])]
                _path_record = _path_record[:next+1]
                concolic_reset_solver()
                _solver.add(_current_path)
                log(DEBUG,   "...... Path Record: %s" % _path_record)
                log(VERBOSE, "...... Negating the condition at %s:%d...." % (basename(_path_record[-1].file), _path_record[-1].line))
                log(VERBOSE, "...... New candidate path: %s" % _current_path)
                # log(DEBUG,   "...... Solver constraints: %s" % _solver.__str__())
                is_sat = _solver.check()
                if is_sat == z3.sat:
                    model = _solver.model()
                    input = ConcolicInput({var.name:
                        concolic_wrap(model.eval(_symbols[var.name], model_completion=True), var) for var in vars})
                    log(VERBOSE, "...... SAT! New inputs are: %s" % concolic_inputs_str(input))
                    _current_path = []  # Reset current_path for next run
                    break
                elif is_sat == z3.unsat:
                    log(VERBOSE, "...... UNSAT!")
                    next = next - 1
                    continue # Go look for the next branch to negate
                else:
                    raise Exception("You should not get a z3 result of %s." % is_sat)
            return
