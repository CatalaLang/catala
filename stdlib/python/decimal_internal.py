from catala_runtime import *

def round_to_decimal2(variable: Decimal, n_decimal: Integer) -> Decimal:
    if n_decimal == Integer(0):
        return decimal_round(variable)
    pow_10 = Decimal(pow(10, abs(int(n_decimal.value))))
    if n_decimal > Integer(0):
        return decimal_round(variable * pow_10) / pow_10
    else:
        return decimal_round(variable / pow_10) * pow_10

def round_to_decimal(variable: Decimal, n_decimal: Integer) -> Decimal:
    r = round_to_decimal2(variable, n_decimal)
    return r
