from catala_runtime import *
from typing import Any, List, Callable, Tuple
from enum import Enum


def sequence(begin:Integer, end:Integer):
    return [begin + Integer(i) for i in range(0, integer_to_int(end - begin))]

def nth_element(lst:List[Any], index:Integer):
    i = integer_to_int(index) - 1
    if i < 0 or len(lst) <= i:
        return Option(None)
    else:
        return Option(lst[i])

def remove_nth_element(lst:List[Any], index:Integer):
    i = integer_to_int(index) - 1
    if i < 0 or len(lst) <= i:
        return lst
    else:
        return lst[:i] + lst [i+1:]

def reverse(lst:List[Any]):
    return list(reversed(lst))
