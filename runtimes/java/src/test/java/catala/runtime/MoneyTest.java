package catala.runtime;

import java.math.BigInteger;

import static org.junit.jupiter.api.Assertions.assertEquals;
import org.junit.jupiter.api.Test;

public class MoneyTest {

    @Test
    public void testRounding() {
        CatalaMoney money = CatalaMoney.ofCents(344).round();

        assertEquals(CatalaMoney.ofCents(300), money);
    }

    @Test
    public void testMultiplyDecimal() {
      CatalaMoney cm = CatalaMoney.ofCents(BigInteger.valueOf(1345));
      CatalaDecimal cd = new CatalaDecimal(1, 10);
      CatalaMoney res = cm.multiply(cd);
      assertEquals(CatalaMoney.ofCents(135), res);
    }
}
