"""
Performs concolic testing on the Catala tutorial (https://catala-lang.org/en/examples/tutorial)

First, run `catala Concolic examples/us_tax_code/tests/test_section_121.catala_en`
Then, run this file
"""

from datetime import date
from catala.concolic import *
from examples.us_tax_code.tests.test_section_121 import *

concolic_run(
    # The entry point to execute with concolically generated inputs
    #   (returns whether the criteria were met and the exclusion amount)
    lambda input: fuzz_section121_single_person(FuzzSection121SinglePersonIn(input.sale_date, input.gain,
        input.ownership_begin, input.ownership_end, input.usage_begin, input.usage_end)),
    # A specification for input vars
    [
        ConcolicVar('sale_date',       type='Date', initial=date(2021, 1, 1)),
        ConcolicVar('gain',            type='Money', initial=350_000_00),
        ConcolicVar('ownership_begin', type='Date', initial=date(2017, 1, 1)),
        ConcolicVar('ownership_end',   type='Date', initial=date(2021, 1, 1)),
        ConcolicVar('usage_begin',     type='Date', initial=date(2017, 1, 1)),
        ConcolicVar('usage_end',       type='Date', initial=date(2021, 1, 1)),
    ],
    constraints = [
        # These invariants must be true for the test case to make sense
        lambda input: input.gain > 0,
        lambda input: input.sale_date == input.ownership_end,
        lambda input: input.ownership_end > input.ownership_begin,
        lambda input: input.usage_end > input.usage_begin,
    ],
    preferences = [
        # We prefer test cases where usage period is within ownership period (this is not strictly needed)
        lambda input: input.usage_begin >= input.ownership_begin,
        lambda input: input.usage_end <= input.ownership_end,
        # We prefer the gain to be in the hundreds of thousands, and be a multiple of $1000
        lambda input: input.gain >= 100_000_00,
        lambda input: input.gain % 1000_00 == 0
    ],
    logging=ConcolicLogLevel.INFO, currency_locale='en_US')
