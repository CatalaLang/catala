# This is a template file following the expected interface and declarations to
# implement the corresponding Catala module.
#
# You should replace all `raise Impossible` place-holders with your
# implementation and rename it to remove the ".template" suffix.

from catala_runtime import *
from typing import Any, List, Callable, Tuple
from enum import Enum


def sort(p:List[Tuple[Date, Date]]):
    pos = (SourcePosition(filename="stdlib/period_internal.catala_en",
               start_line=6, start_column=13, end_line=6, end_column=17,
               law_headings=[]))
    raise Impossible(pos)
    return sort__1

def split_by_month(p:Tuple[Date, Date]):
    pos = (SourcePosition(filename="stdlib/period_internal.catala_en",
               start_line=9, start_column=13, end_line=9, end_column=27,
               law_headings=[]))
    raise Impossible(pos)
    return split_by_month__1

def split_by_year(start_month:Integer, p:Tuple[Date, Date]):
    pos = (SourcePosition(filename="stdlib/period_internal.catala_en",
               start_line=12, start_column=13, end_line=12, end_column=26,
               law_headings=[]))
    raise Impossible(pos)
    return split_by_year__1
