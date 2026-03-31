from catala_runtime import *
from enum import Enum

def sort(p:Array[CatalaTuple[CatalaTuple[Date, Date], Alpha]]) -> Array[CatalaTuple[CatalaTuple[Date, Date], Alpha]]:
    return Array(sorted(p, key=(lambda p: p[0][0])))

one_day = Duration((0,0,1))
one_month = Duration((0,1,0))

def split_by_month(p:CatalaTuple[Date, Date]) -> Array[CatalaTuple[Date, Date]]:
    cur = p[0]
    end = p[1]
    result = []
    while cur < end:
        cut = min(end, Date(cur.value.first_day_of_month()) + one_month)
        if (cut == end):
          result.append((cur, cut))
        else:
          result.append((cur, cut - one_day))
        cur = cut
    return Array(result)

def first_day_of_next_rolling_year(start_month:Integer, date:Date) -> Date:
    assert (1 <= int(start_month) and int(start_month) <= 12)
    if date.value.month < int(start_month):
        yr = date.value.year
    else:
        yr = date.value.year + 1
    return Date((yr, start_month, 1))

def split_by_year(start_month:Integer, p:CatalaTuple[Date, Date]) -> Array[CatalaTuple[Date, Date]]:
    cur = p[0]
    end = p[1]
    result = []
    while cur < end:
        cut = min(end, first_day_of_next_rolling_year(start_month, cur))
        if (cut == end):
          result.append((cur, cut))
        else:
          result.append((cur, cut - one_day))
        cur = cut
    return Array(result)
