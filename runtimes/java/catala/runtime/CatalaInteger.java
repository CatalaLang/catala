package catala.runtime;

import java.math.BigInteger;

import catala.runtime.exception.CatalaError;

public final class CatalaInteger implements CatalaValue, Comparable<CatalaInteger> {

    public final static CatalaInteger ZERO = new CatalaInteger(0);
    public final static CatalaInteger ONE = new CatalaInteger(1);

    private final BigInteger value;

    public CatalaInteger(BigInteger value) {
        this.value = value;
    }

    public CatalaInteger(int value) {
        this.value = BigInteger.valueOf(value);
    }

    public CatalaInteger(String value) {
        this.value = new BigInteger(value);
    }

    public final BigInteger asBigInteger() {
        return this.value;
    }

    public final CatalaDecimal asDecimal() {
        return new CatalaDecimal(this, new CatalaInteger(BigInteger.ONE));
    }

    public final CatalaMoney asMoney() {
        return CatalaMoney.ofCents(BigInteger.valueOf(100)).multiply(this);
    }

    // ToInt_rat
    public static final CatalaInteger ofDecimal(CatalaDecimal dec) {
        return new CatalaInteger(dec.getNumerator().divide(dec.getDenominator()));
    }

    public static final CatalaInteger valueOf(long value) {
        return new CatalaInteger(BigInteger.valueOf(value));
    }

    public final CatalaInteger negate() {
        return new CatalaInteger(this.value.negate());
    }

    /**
     * @param other {@inheritDoc}
     * @return {@inheritDoc}
     */
    @Override
    public int compareTo(CatalaInteger other) {
        return this.value.compareTo(other.value);
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null || getClass() != obj.getClass()) {
            return false;
        }
        CatalaInteger other = (CatalaInteger) obj;
        return this.value.equals(other.value);
    }

    @Override
    public int hashCode() {
        return this.value.hashCode();
    }

    @Override
    public String toString() {
        return this.value.toString();
    }

    public CatalaInteger add(CatalaInteger i) {
        return new CatalaInteger(this.value.add(i.value));
    }

    public CatalaInteger subtract(CatalaInteger i) {
        return new CatalaInteger(this.value.subtract(i.value));
    }

    public CatalaInteger multiply(CatalaInteger i) {
        return new CatalaInteger(this.value.multiply(i.value));
    }

    // TODO: add throws
    public CatalaDecimal divide(CatalaPosition pos, CatalaInteger denum) {
        if (denum.value.equals(BigInteger.ZERO)) {
            throw new CatalaError(CatalaError.Error.DivisionByZero, pos);
        }
        return new CatalaDecimal(this, denum);
    }

    public CatalaBool lessThan(CatalaInteger other) {
        return CatalaBool.fromBoolean(this.compareTo(other) < 0);
    }

    public CatalaBool lessEqThan(CatalaInteger other) {
        return CatalaBool.fromBoolean(this.compareTo(other) <= 0);
    }

    public CatalaBool greaterThan(CatalaInteger other) {
        return CatalaBool.fromBoolean(this.compareTo(other) > 0);
    }

    public CatalaBool greaterEqThan(CatalaInteger other) {
        return CatalaBool.fromBoolean(this.compareTo(other) >= 0);
    }

    public CatalaBool equalsTo(CatalaInteger other) {
        return CatalaBool.fromBoolean(this.compareTo(other) == 0);
    }

    @Override
    public CatalaBool equalsTo(CatalaValue other) {
        if (other instanceof CatalaInteger catalaInteger) {
            return this.equalsTo(catalaInteger);
        } else {
            return CatalaBool.FALSE;
        }
    }

}
