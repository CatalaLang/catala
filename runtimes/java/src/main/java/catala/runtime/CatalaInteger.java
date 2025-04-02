package catala.runtime;

import java.math.BigInteger;

// We could make this class extend java.math.BigInteger
// for consumer friendliness?
public final class CatalaInteger implements CatalaValue, Comparable<CatalaInteger> {

    private final BigInteger value;

    public CatalaInteger(BigInteger value) {
        this.value = value;
    }

    public final BigInteger asBigInteger() {
        return this.value;
    }

    // ToInt_rat
    public static final CatalaInteger ofDecimal(CatalaDecimal dec) {
        return new CatalaInteger(dec.getNumerator().divide(dec.getDenominator()));
    }

    /**
     * @param other {@inheritDoc}
     * @return {@inheritDoc}
     */
    @Override
    public int compareTo(CatalaInteger other) {
        return this.value.compareTo(other.value);
    }
}
