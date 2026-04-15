package catala.runtime;

import catala.runtime.exception.CatalaError;
import java.math.BigInteger;

public final class CatalaInteger extends CatalaValue<CatalaInteger> {

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

    public static final CatalaInteger ofDecimal(CatalaDecimal dec) {
        return new CatalaInteger(dec.getNumerator().divide(dec.getDenominator()));
    }

    public static final CatalaInteger valueOf(long value) {
        return new CatalaInteger(BigInteger.valueOf(value));
    }

    public final CatalaInteger negate() {
        return new CatalaInteger(this.value.negate());
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
            throw CatalaError.error(CatalaError.Error.DivisionByZero, pos);
        }
        return new CatalaDecimal(this, denum);
    }

    /**
     * @return this {@code CatalaInteger} converted to a {@code long}.
     */
    public long longValue() {
        return this.value.longValue();
    }

    /**
     * @return this {@code CatalaInteger} converted to a {@code long}.
     * @throws ArithmeticException if the value of {@code this} will not exactly
     * fit in a {@code long}.
     */
    public long longValueExact() {
        return this.value.longValueExact();
    }

    /**
     * @return this {@code CatalaInteger} converted to an {@code int}.
     */
    public int intValue() {
        return this.value.intValue();
    }

    /**
     * @return this {@code CatalaInteger} converted to an {@code int}.
     * @throws ArithmeticException if the value of {@code this} will not exactly
     * fit in an {@code int}.
     */
    public int intValueExact() {
        return this.value.intValueExact();
    }

    @Override
    public int compareTo(CatalaPosition p, CatalaInteger o) {
        return this.value.compareTo(o.value);
    }

    @Override
    public CatalaBool equalsTo(CatalaPosition p, CatalaInteger o) {
        return CatalaBool.fromBoolean(this.value.equals(o.value));
    }

    @Override
    public int hashCode() {
        return this.value.hashCode();
    }

    @Override
    public String toString() {
        return this.value.toString();
    }

    @Override
    public String toJSONString() {
        return '"' + this.value.toString() + '"';
    }
}
