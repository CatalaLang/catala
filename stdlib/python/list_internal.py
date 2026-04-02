from catala_runtime import *
from enum import Enum


def sequence(begin:Integer, end:Integer) -> Array[Integer]:
    return Array([begin + Integer(i) for i in range(0, end - begin)])

def nth_element(lst:Array[Alpha], index:Integer) -> Option[Alpha]:
    i = int(index) - 1
    if i < 0 or len(lst) <= i:
        return Option(None)
    else:
        return Option(lst[i])

def remove_nth_element(lst:Array[Alpha], index:Integer) -> Array[Alpha]:
    i = index - 1
    if i < Integer(0) or Integer(len(lst)) <= i:
        return lst
    else:
        return Array(lst[:i] + lst [i+1:])

def reverse(lst:Array[Any]) -> Array[Any]:
    return Array(reversed(lst))
