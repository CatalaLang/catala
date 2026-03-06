package catala.runtime;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;

import catala.runtime.exception.CatalaError;

public final class CatalaMoney extends CatalaValue<CatalaMoney> {

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
            throw CatalaError.error(CatalaError.Error.DivisionByZero, pos);
        }
        return new CatalaDecimal(new CatalaInteger(this.value), new CatalaInteger(other.value));
    }

    // Div_mon_int
    public final CatalaMoney divide(CatalaPosition pos, CatalaInteger other) {
        return this.divide(pos, new CatalaDecimal(other, new CatalaInteger(BigInteger.ONE)));
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

    @Override
    public int hashCode() {
        return this.value.hashCode();
    }

    @Override
    public int compareTo(CatalaPosition p, CatalaMoney o) {
        return this.value.compareTo(o.value);
    }

    @Override
    public CatalaBool equalsTo(CatalaPosition p, CatalaMoney o) {
        return CatalaBool.fromBoolean(this.value.equals(o.value));

    }

    @Override
    public String toString() {
        BigInteger[] unitsAndCents = this.value.divideAndRemainder(BigInteger.valueOf(100));
        return String.format("$%d.%02d", (Object[]) unitsAndCents);
    }
}
