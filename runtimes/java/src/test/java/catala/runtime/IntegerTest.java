package catala.runtime;

import java.math.BigInteger;

import static org.junit.jupiter.api.Assertions.assertEquals;
import org.junit.jupiter.api.Test;

public class IntegerTest {

    @Test
    public void testOfDecimal() {
        CatalaDecimal dec = new CatalaDecimal(22, 7);

        assertEquals(new CatalaInteger(BigInteger.valueOf(3)), CatalaInteger.ofDecimal(dec));
    }
}