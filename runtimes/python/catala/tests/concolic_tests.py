"""

Tests the concolic execution engine against the tutorial example
To run (after setting PATH and PYTHONPATH to appropriate _build directories):
1. catala Concolic examples/tutorial_en/tutorial_en.catala_en
2. python runtimes/python/catala/tests/concolic_tutorial_test.py

Copyright (c) 2023 Rohan Padhye <rohanpadhye@cmu.edu>

Licensed under the Apache License, Version 2.0 (the "License"); you may not
use this file except in compliance with the License. You may obtain a copy of
the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
License for the specific language governing permissions and limitations under
the License.

"""

from catala.concolic import *
from datetime import date

# The Catala tutorial!
import examples.tutorial_en.tutorial_en as tutorial

# Tests the buggy version of the tutorial
assert concolic_run(lambda input: tutorial.new_income_tax_computation(
    tutorial.NewIncomeTaxComputationIn(tutorial.Individual(input.income, input.children))).income_tax, [
    ConcolicVar('income', type='Money', initial=230_000_00),
    ConcolicVar('children', type='Integer')]) == (3, True, True) # 3 paths, with a bug

# Tests the fixed version of the tutorial
assert concolic_run(lambda input: tutorial.new_income_tax_computation_fixed(
    tutorial.NewIncomeTaxComputationFixedIn(tutorial.Individual(input.income, input.children), lambda _: None)).income_tax, [
    ConcolicVar('income', type='Money', initial=4_000_00),
    ConcolicVar('children', type='Integer')]) == (4, True, False) # 4 paths, no bug

# Section 132 from the US Tax Code
import examples.us_tax_code.tests.test_section_132 as section_132

# Tests Section 132 for properties
assert concolic_run(lambda input: section_132.fuzz_section132_property(
    section_132.FuzzSection132PropertyIn(input.customer_price, input.employee_price, input.aggregate_cost)),
    [
        ConcolicVar('customer_price', type='Money', initial=1500_00),
        ConcolicVar('employee_price', type='Money', initial=1000_00),
        ConcolicVar('aggregate_cost', type='Money', initial=900_00),
    ],
    constraints = [
        # These invariants must be true for the test case to make sense
        lambda input: input.customer_price >= input.employee_price,
        lambda input: input.customer_price >= input.aggregate_cost
    ]) == (2, True, False) # 2 paths, no bug

# Tests Section 132 for services
assert concolic_run(lambda input: section_132.fuzz_section132_services(
    section_132.FuzzSection132ServicesIn(input.customer_price, input.employee_price)), [
        ConcolicVar('customer_price', type='Money', initial=1500_00),
        ConcolicVar('employee_price', type='Money', initial=1000_00),
    ], constraints = [
        # These invariants must be true for the test case to make sense
        lambda input: input.customer_price >= input.employee_price
    ]) == (2, True, False) # 2 paths, no bug

# Section 121 from the US Tax Code
import examples.us_tax_code.tests.test_section_121 as section_121

# Tests Section 121 for a single person
assert concolic_run(lambda input: section_121.fuzz_section121_single_person(section_121.FuzzSection121SinglePersonIn(
    input.sale_date, input.gain, input.ownership_begin, input.ownership_end, input.usage_begin, input.usage_end)),
    [
        ConcolicVar('sale_date', type='Date', initial=date(2021, 1, 1)),
        ConcolicVar('gain', type='Money', initial=350_000_00),
        ConcolicVar('ownership_begin', type='Date', initial=date(2017, 1, 1)),
        ConcolicVar('ownership_end', type='Date', initial=date(2021, 1, 1)),
        ConcolicVar('usage_begin', type='Date', initial=date(2017, 1, 1)),
        ConcolicVar('usage_end', type='Date', initial=date(2021, 1, 1)),
    ],
    constraints = [
        # These invariants must be true for the test case to make sense
        lambda input: input.gain > 0,
        lambda input: input.sale_date == input.ownership_end,
        lambda input: input.ownership_end > input.ownership_begin,
        lambda input: input.usage_end > input.usage_begin,
    ]) == (19, True, False)