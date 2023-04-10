"""
Performs concolic testing on the Catala tutorial (https://catala-lang.org/en/examples/tutorial)

First, run `catala Concolic examples/us_tax_code/tests/test_section_132.catala_en`
Then, run this file
"""

from datetime import date
from catala.concolic import *
from examples.us_tax_code.tests.test_section_132 import *

concolic_run(
    lambda input: fuzz_section132_property(FuzzSection132PropertyIn(input.customer_price, input.employee_price, input.aggregate_cost)),
    [
        ConcolicVar('customer_price', type='Money', initial=1500_00),
        ConcolicVar('employee_price', type='Money', initial=1000_00),
        ConcolicVar('aggregate_cost', type='Money', initial=900_00),
    ],
    constraints = [
        # These invariants must be true for the test case to make sense
        lambda input: input.customer_price >= input.employee_price,
        lambda input: input.customer_price >= input.aggregate_cost
    ],
    preferences = [
        # We prefer test cases with non-zero input prices (all zeros are boring)
        lambda input: input.customer_price > 0,
        lambda input: input.employee_price > 0,
        lambda input: input.aggregate_cost > 0,
        # We prefer test cases with an actual discount
        lambda input: input.customer_price > input.employee_price,
        # We prefer test cases with prices rounding to the nearest $100
        lambda input: input.customer_price % 100_00 == 0,
        lambda input: input.employee_price % 100_00 == 0,
        lambda input: input.aggregate_cost % 100_00 == 0,
    ],
    logging=ConcolicLogLevel.INFO, currency_locale='en_US')
