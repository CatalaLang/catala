"""
Performs concolic testing on the Catala tutorial (https://catala-lang.org/en/examples/tutorial)

First, run `catala Concolic examples/tutorial_en/tutorial_en.catala_en`
Then, run this file
"""

from catala.concolic import *
from examples.tutorial_en.tutorial_en import *

concolic_run(
	# The entry point to execute with concolically generated inputs (returns the computed income tax)
	lambda input: new_income_tax_computation_fixed(
        NewIncomeTaxComputationFixedIn(Individual(input.income, input.children), lambda _: None)).income_tax,
    # A specification for input vars (in this case, we have two inputs)
    vars = [
        ConcolicVar('income', type='Money', initial=230_000_00),
        ConcolicVar('children', type='Integer', initial=0)
    ], constraints = [
        # We only care about non-negative incomes
        lambda input: input.income >= 0
    ], preferences = [
        # Prefer incomes rounding to $1000.00 are nice
        lambda input: input.income % 1000_00 == 0,
        # Prefer incomes of more than $10,000.00
        lambda input: input.income > 10_000_00,
    ], logging=ConcolicLogLevel.INFO,  currency_locale='en_US')