from catala_runtime import *
from typing import Any, List, Callable, Tuple
from enum import Enum
from dateutil import relativedelta

def sort(p:List[Tuple[Tuple[Date, Date], Any]]):
    return sorted(p, key=(lambda p: p[0][0]))

def split_by_month(p:Tuple[Date, Date]):
    cur = p[0]
    end = p[1]
    result = []
    while cur < end:
        cut = min(end, first_day_of_month(cur + Duration(relativedelta.relativedelta(months=1))))
        result.append((cur, cut))
        cur = cut
    return result

def first_day_of_next_rolling_year(start_month:int, date:Date):
    assert (1 <= start_month and start_month <= 12)
    if date.value.month < start_month:
        yr = date.value.year
    else:
        yr = date.value.year + 1
    return Date(datetime.date(year=yr, month=start_month, day=1))

def split_by_year(start_month:Integer, p:Tuple[Date, Date]):
    cur = p[0]
    end = p[1]
    result = []
    while cur < end:
        cut = min(end, first_day_of_next_rolling_year(start_month.value, cur))
        result.append((cur, cut))
        cur = cut
    return result
