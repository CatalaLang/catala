from catala_runtime import *


def round_to_decimal(variable: Money, n_decimal: Integer) -> Money:
    if n_decimal >= Integer(2):
        return variable
    elif n_decimal == Integer(1):
        return Money(Integer(money_round(Money(variable.value * Integer(10))).value / Integer(10)))
    else:
        pow_10 = Integer(pow(10, -int(n_decimal.value)))
        r: Money = money_round(Money(Integer(variable.value / pow_10)))
        return Money(r.value * pow_10)
