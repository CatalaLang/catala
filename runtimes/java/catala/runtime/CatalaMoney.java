package catala.runtime;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;

import catala.runtime.exception.CatalaError;

public final class CatalaMoney implements CatalaValue, Comparable<CatalaMoney> {

    // cents
    private final BigInteger value;

    private CatalaMoney(BigInteger valueCents) {
        this.value = valueCents;
    }

    public final BigInteger asCents() {
        return this.value;
    }

    public final CatalaDecimal asDecimal() {
        return new CatalaDecimal(new CatalaInteger(this.value),
                new CatalaInteger(BigInteger.valueOf(100)));
    }

    public final CatalaInteger asInteger() {
        BigDecimal bd = new BigDecimal(this.value).divide(new BigDecimal(100), 0, RoundingMode.HALF_UP);
        return new CatalaInteger(bd.toBigInteger());
    }

    public final CatalaMoney negate() {
        return new CatalaMoney(this.value.negate());
    }

    // Round to the nearest unit (multiple of 100 cents)
    public final CatalaMoney round() {
        BigDecimal bd = new BigDecimal(this.value).divide(new BigDecimal(100), 0, RoundingMode.HALF_UP);
        BigInteger result = bd.toBigInteger().multiply(BigInteger.valueOf(100));
        return new CatalaMoney(result);
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null || getClass() != obj.getClass()) {
            return false;
        }
        CatalaMoney other = (CatalaMoney) obj;
        return this.value.equals(other.value);
    }

    @Override
    public int hashCode() {
        return this.value.hashCode();
    }

    /**
     * @param other {@inheritDoc}
     * @return {@inheritDoc}
     */
    @Override
    public int compareTo(CatalaMoney other) {
        return this.value.compareTo(other.value);
    }

    @Override
    public String toString() {
        BigInteger[] unitsAndCents = this.value.divideAndRemainder(BigInteger.valueOf(100));
        return String.format("$%d.%02d", (Object[]) unitsAndCents);
    }

    public final CatalaMoney subtract(CatalaMoney other) {
        return new CatalaMoney(this.value.subtract(other.asCents()));
    }

    public final CatalaMoney add(CatalaMoney other) {
        return new CatalaMoney(this.value.add(other.asCents()));
    }

    public final CatalaMoney multiply(CatalaInteger other) {
        return new CatalaMoney(this.value.multiply(other.asBigInteger()));
    }

    public final CatalaMoney multiply(CatalaMoney other) {
        return new CatalaMoney(this.value.multiply(other.value));
    }

    /**
     * Rounds to the nearest cent
     */
    public final CatalaMoney multiply(CatalaDecimal other) {
        CatalaDecimal thisDecCents
                = CatalaDecimal.ofMoneyAsCents(this);
        CatalaDecimal resDecimal = thisDecCents.multiply(other);
        BigDecimal resBigDecimal = resDecimal.bigDecimalValue(0, RoundingMode.HALF_UP);
        return new CatalaMoney(resBigDecimal.toBigIntegerExact());
    }

    // Div_mon_mon
    public final CatalaDecimal divide(CatalaPosition pos, CatalaMoney other) {
        if (other.value.equals(BigInteger.ZERO)) {
            throw new CatalaError(CatalaError.Error.DivisionByZero, pos);
        }
        return new CatalaDecimal(new CatalaInteger(this.value), new CatalaInteger(other.value));
    }

    // Div_mon_int
    public final CatalaMoney divide(CatalaPosition pos, CatalaInteger other) {
        if (other.asBigInteger().equals(BigInteger.ZERO)) {
            throw new CatalaError(CatalaError.Error.DivisionByZero, pos);
        }
        return this.multiply(new CatalaDecimal(
                new CatalaInteger(BigInteger.ONE),
                other));
    }

    // Div_mon_rat
    public final CatalaMoney divide(CatalaPosition pos, CatalaDecimal other) {
        return this.multiply(other.inverse(pos));
    }

    public static final CatalaMoney ofCents(String cents) {
        return new CatalaMoney(new BigInteger(cents));
    }

    public static final CatalaMoney ofCents(BigInteger cents) {
        return new CatalaMoney(cents);
    }

    public static final CatalaMoney ofCents(long cents) {
        return new CatalaMoney(BigInteger.valueOf(cents));
    }

    public CatalaBool lessThan(CatalaMoney other) {
        return CatalaBool.fromBoolean(this.compareTo(other) < 0);
    }

    public CatalaBool lessEqThan(CatalaMoney other) {
        return CatalaBool.fromBoolean(this.compareTo(other) <= 0);
    }

    public CatalaBool greaterThan(CatalaMoney other) {
        return CatalaBool.fromBoolean(this.compareTo(other) > 0);
    }

    public CatalaBool greaterEqThan(CatalaMoney other) {
        return CatalaBool.fromBoolean(this.compareTo(other) >= 0);
    }

    public CatalaBool equalsTo(CatalaMoney other) {
        return CatalaBool.fromBoolean(this.compareTo(other) == 0);
    }

    @Override
    public CatalaBool equalsTo(CatalaValue other) {
        if (other instanceof CatalaMoney catalaMoney) {
            return this.equalsTo(catalaMoney);
        } else {
            return CatalaBool.FALSE;
        }
    }

}
