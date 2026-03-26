from catala_runtime import *
from typing import Any, List, Callable, Tuple
from enum import Enum


def sequence(begin:Integer, end:Integer):
    return Array([begin + Integer(i) for i in range(0, integer_to_int(end - begin))])

def nth_element(lst:Array[Any], index:Integer):
    i = int(index) - 1
    if i < 0 or len(lst) <= i:
        return Option(None)
    else:
        return Option(lst[i])

def remove_nth_element(lst:Array[Any], index:Integer):
    i = integer_to_int(index) - 1
    if i < 0 or len(lst) <= i:
        return lst
    else:
        return Array(lst[:i] + lst [i+1:])

def reverse(lst:Array[Any]):
    return Array(reversed(lst))
