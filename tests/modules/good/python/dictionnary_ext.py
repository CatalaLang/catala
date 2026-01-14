# This is a template file following the expected interface and declarations to
# implement the corresponding Catala module.
#
# You should replace all `raise Impossible` place-holders with your
# implementation and rename it to remove the ".template" suffix.

from catala_runtime import *
from typing import Any, List, Callable, Tuple
from enum import Enum
from . import Stdlib_en as stdlib_en
from . import Date_en as date_en
from . import MonthYear_en as month_year_en
from . import Period_en as period_en
from . import Money_en as money_en
from . import Integer_en as integer_en
from . import Decimal_en as decimal_en
from . import List_en as list_en

Dictionnary = dict

empty = {}

def store(dict:Dictionnary, key:Integer, value:Money):
    ret = dict.copy()
    ret[key.value] = value
    return ret

def find(dict:Dictionnary, key:Integer):
    if key.value in dict:
        return Option(dict[key.value])
    else:
        return Option(None)
