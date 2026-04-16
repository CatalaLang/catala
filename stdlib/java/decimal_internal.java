package catala.stdlib;

import catala.runtime.*;
import java.math.BigInteger;

public class Decimal_internal {

    public static class Globals {

        public static final CatalaFunction<CatalaTuple, CatalaDecimal> roundToDecimal
                = tup_arg -> {
                    CatalaDecimal m = CatalaValue.<CatalaDecimal>cast(tup_arg.get(0));
                    BigInteger n = CatalaValue.<CatalaInteger>cast(tup_arg.get(1)).asBigInteger();
                    int cmp = n.compareTo(BigInteger.ZERO);
                    if (cmp == 0) {
                        return m.round().asDecimal();
                    } else {
                        CatalaInteger pow_ten = new CatalaInteger(BigInteger.valueOf(10).pow(n.abs().intValueExact()));
                        if (cmp > 0) {
                            return m.multiply(pow_ten).round().asDecimal().divide(pow_ten);
                        } else {
                            return m.divide(pow_ten).round().multiply(pow_ten).asDecimal();
                        }
                    }
                };
    }
}
