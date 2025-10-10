# This is a template file following the expected interface and declarations to
# implement the corresponding Catala module.
#
# You should replace all `raise Impossible` place-holders with your
# implementation and rename it to remove the ".template" suffix.

from catala_runtime import *
from typing import Any, List, Callable, Tuple
from enum import Enum


def sequence(begin:Integer, end:Integer):
    return [begin + Integer(i) for i in range(0, integer_to_int(end - begin))]

def nth_element(lst:List[Any], index:Integer):
    i = integer_to_int(index) - 1
    if i < 0 or len(lst) <= i:
        return None
    else:
        return lst[i]

def remove_nth_element(lst:List[Any], index:Integer):
    i = integer_to_int(index) - 1
    if i < 0 or len(lst) <= i:
        return lst
    else:
        return lst[:i] + lst [i+1:]
