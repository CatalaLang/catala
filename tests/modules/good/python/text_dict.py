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
from . import Text as text
from . import Mod_def as mod_def

class Text_dict(Value, dict):
    pass

empty = Text_dict({})

def store(dict:Text_dict, key:text, value:mod_def):
    ret = dict.copy()
    ret[key] = value
    return ret

def find(dict:Text_dict, key:text):
    if key in dict:
        return Option(dict[key])
    else:
        return Option(None)
