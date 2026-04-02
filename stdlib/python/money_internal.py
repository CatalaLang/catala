from catala_runtime import *


def round_to_decimal(variable: Money, n_decimal: Integer) -> Money:
    if n_decimal >= Integer(2):
        return variable
    elif n_decimal == Integer(1):
        return Money(Integer(Money(variable * Integer(10)).round()) / Integer(10))
    else:
        pow_10 = Integer(pow(10, -int(n_decimal)))
        r: Money = Money(Integer(variable / pow_10)).round()
        return Money(r * pow_10)
