# This is a template file following the expected interface and declarations to
# implement the corresponding Catala module.
#
# You should replace all `raise Impossible` place-holders with your
# implementation and rename it to remove the ".template" suffix.

from catala_runtime import *
from typing import Any, List, Callable, Tuple
from enum import Enum
from sys import stderr
from . import Stdlib_en as stdlib_en
from . import Date_en as date_en
from . import List_en as list_en
from . import Duration_en as duration_en
from . import MonthYear_en as month_year_en
from . import Period_en as period_en
from . import Money_en as money_en
from . import Integer_en as integer_en
from . import Decimal_en as decimal_en

class String(Value, str):
    def __str__(self, indent: int = 0) -> str:
        return (f'"{super().__str__()}"')

foo = String("foo\\")

bar = String("bąr")

fortytwo = String("42")

def of_int(x:Integer) -> String:
    return String(x)
