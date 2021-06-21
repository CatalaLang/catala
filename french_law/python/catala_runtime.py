# This file should be in sync with compiler/runtime.{ml, mli} !

from gmpy2 import mpz, mpq, mpfr, mpc  # type: ignore
import datetime
from typing import NewType, List

# Types

Money = NewType('Money', object)
Integer = NewType('Integer', object)
Decimal = NewType('Decimal', object)
Date = NewType('Date', datetime.date)
Duration = NewType('Duration', datetime.timedelta)


class SourcePosition:
    def __init__(self,
                 filename: str,
                 start_line: int,
                 start_column: int,
                 end_line: int,
                 end_column: int,
                 law_headings: List[str]) -> None:
        self.filename = filename
        self.start_line = start_line
        self.start_column = start_column
        self.end_line = end_line
        self.end_column = end_column
        self.law_headings = law_headings

# Exceptions


class EmptyError(Exception):
    pass


class AssertionFailed(Exception):
    pass


class ConflictError(Exception):
    pass


class UncomparableDurations(Exception):
    pass


class IndivisableDurations(Exception):
    pass


class ImpossibleDate(Exception):
    pass


class NoValueProvided(Exception):
    def __init__(self, source_position: SourcePosition) -> None:
        self.source_position = SourcePosition

# TODO: Value embedding

# TODO: logging
