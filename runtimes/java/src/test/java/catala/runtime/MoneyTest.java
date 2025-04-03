package catala.runtime;

import java.math.BigInteger;

import static org.junit.jupiter.api.Assertions.assertEquals;
import org.junit.jupiter.api.Test;

public class MoneyTest {
  @Test
  public void testRounding(){
    CatalaMoney money = CatalaMoney.ofCents(BigInteger.valueOf(344)).round();

    assertEquals(money, CatalaMoney.ofCents(BigInteger.valueOf(300)));
  }
}