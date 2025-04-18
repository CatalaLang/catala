package catala.runtime;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;

import org.apache.commons.numbers.fraction.BigFraction;

import catala.runtime.exception.CatalaException;

// We probably want to keep this class as-is
// and not derive it from BigFraction as BigFraction is not part of
// java's stdlib and is currently vendored. (note: should we shadow
// the package name for BigFraction?)
public final class CatalaDecimal implements CatalaValue, Comparable<CatalaDecimal> {

    private final BigFraction value;

    private /* not sure of this? We might want to avoid let BigFraction escape in the API? */ CatalaDecimal(BigFraction value) {
        this.value = value;
    }

    //keeping this package-level for now
    CatalaDecimal(int num, int den) {
        this.value = BigFraction.of(num, den);
    }

    public CatalaDecimal(CatalaInteger num, CatalaInteger den) {
        this.value = BigFraction.of(num.asBigInteger(), den.asBigInteger());
    }

    public CatalaInteger asInteger() {
        return new CatalaInteger(this.value.getNumerator().divide(this.value.getDenominator()));
    }

    public CatalaDecimal negate() {
        return new CatalaDecimal(this.value.negate());
    }

    /**
     * @param other {@inheritDoc}
     * @return {@inheritDoc}
     */
    @Override
    public int compareTo(CatalaDecimal other) {
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
        CatalaDecimal other = (CatalaDecimal) obj;
        return this.value.equals(other.value);
    }

    @Override
    public CatalaBool equalsTo(CatalaValue v) {
        if (v instanceof CatalaDecimal catalaDecimal) {
            return CatalaBool.fromBoolean(this.compareTo(catalaDecimal) == 0);
        } else {
            return CatalaBool.FALSE;
        }
    }

    @Override
    public int hashCode() {
        return this.value.hashCode();
    }

    public final BigInteger getDenominator() {
        return this.value.getDenominator();
    }

    public final BigInteger getNumerator() {
        return this.value.getNumerator();
    }

    public final CatalaDecimal add(CatalaDecimal other) {
        return new CatalaDecimal(this.value.add(other.value));
    }

    public final CatalaDecimal add(CatalaInteger other) {
        return new CatalaDecimal(this.value.add(other.asBigInteger()));
    }

    public final CatalaDecimal subtract(CatalaDecimal other) {
        return new CatalaDecimal(this.value.subtract(other.value));
    }

    public final CatalaDecimal subtract(CatalaInteger other) {
        return new CatalaDecimal(this.value.subtract(other.asBigInteger()));
    }

    public final CatalaDecimal multiply(CatalaDecimal other) {
        return new CatalaDecimal(this.value.multiply(other.value));
    }

    public final CatalaDecimal multiply(CatalaInteger other) {
        return new CatalaDecimal(this.value.multiply(other.asBigInteger()));
    }

    public final CatalaDecimal inverse(SourcePosition pos) {
        if (this.value.getNumerator().equals(BigInteger.ZERO)) {
            throw new CatalaException("division by zero: " + pos);
        }
        return new CatalaDecimal(new CatalaInteger(this.getDenominator()), new CatalaInteger(this.getNumerator()));
    }

    // TODO: add throws
    public CatalaDecimal divide(SourcePosition pos, CatalaDecimal denum) {
        try {
            return new CatalaDecimal(this.value.divide(denum.value));
        } catch (ArithmeticException e) {
            throw new CatalaException("division by zero: " + pos);
        }
    }

    public CatalaDecimal round() {
        return new CatalaDecimal(new CatalaInteger(this.bigDecimalValue(0, RoundingMode.HALF_UP).toBigInteger()),
                new CatalaInteger(1));
    }

    final BigDecimal bigDecimalValue(int scale, RoundingMode roundingMode) {
        return this.value.bigDecimalValue(scale, roundingMode);
    }

    // ToRat_int
    public static final CatalaDecimal ofInteger(CatalaInteger ci) {
        return new CatalaDecimal(BigFraction.of(ci.asBigInteger()));
    }

    // ToRat_mon
    public static final CatalaDecimal ofMoney(CatalaMoney cm) {
        return new CatalaDecimal(BigFraction.of(cm.asCents(), BigInteger.valueOf(100)));
    }

    public final CatalaMoney asMoney() {
        return CatalaMoney.ofCents(BigInteger.valueOf(100)).multiply(this);
    }

    static final CatalaDecimal ofMoneyAsCents(CatalaMoney cm) {
        return new CatalaDecimal(BigFraction.of(cm.asCents()));
    }

    public final CatalaDecimal divide(CatalaDecimal other) {
        return new CatalaDecimal(this.value.multiply(other.value));
    }

    public CatalaBool lessThan(CatalaDecimal other) {
        return CatalaBool.fromBoolean(this.compareTo(other) < 0);
    }

    public CatalaBool lessEqThan(CatalaDecimal other) {
        return CatalaBool.fromBoolean(this.compareTo(other) <= 0);
    }

    public CatalaBool greaterThan(CatalaDecimal other) {
        return CatalaBool.fromBoolean(this.compareTo(other) > 0);
    }

    public CatalaBool greaterEqThan(CatalaDecimal other) {
        return CatalaBool.fromBoolean(this.compareTo(other) >= 0);
    }

    public CatalaBool equalsTo(CatalaDecimal other) {
        return CatalaBool.fromBoolean(this.compareTo(other) == 0);
    }

    @Override
    public String toString() {
        return this.bigDecimalValue(10, RoundingMode.HALF_UP).toPlainString();
    }
}
