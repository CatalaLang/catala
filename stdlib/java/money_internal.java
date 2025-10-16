import catala.runtime.*;
import catala.runtime.exception.*;
import java.math.BigInteger;

public class Money_internal {
    public static class Globals {
      public static final CatalaFunction<CatalaTuple, CatalaMoney> roundToDecimal
            = tup_arg -> {
                CatalaMoney m = CatalaValue.<CatalaMoney>cast(tup_arg.get(0));
                int n = CatalaValue.<CatalaInteger>cast(tup_arg.get(1)).asBigInteger().intValue();
                if (n >= 2) {
                    return m;
                } else {
                    BigInteger x = m.asCents();
                    CatalaInteger ten = CatalaInteger.valueOf(10);
                    if (n == 1) {
                        CatalaPosition dummy_pos
                        = new CatalaPosition("none", 1, 1, 1, 1, null);
                        return m.multiply(ten).round().divide(dummy_pos, ten);
                    } else {
                        BigInteger pow_ten = ten.asBigInteger().pow(-n);
                        return CatalaMoney.ofCents(CatalaMoney.ofCents(x.divide(pow_ten)).round().asCents().multiply(pow_ten));
                    }
                }
            };
    }
}
