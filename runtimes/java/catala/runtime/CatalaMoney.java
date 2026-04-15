package catala.runtime;

import catala.runtime.exception.CatalaError;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;
import java.text.NumberFormat;
import java.util.Locale;

public final class CatalaMoney extends CatalaValue<CatalaMoney> {

    // cents
    private final BigInteger value;

    private CatalaMoney(BigInteger valueCents) {
        this.value = valueCents;
    }

    public CatalaMoney(long value) {
        this.value = BigInteger.valueOf(value * 100);
    }

    public CatalaMoney(int value) {
        this((long) value);
    }

    public CatalaMoney(final CatalaDecimal dec) {
        this.value = dec.multiply(CatalaInteger.of(100)).round().asBigInteger();
    }

    public CatalaMoney(final CatalaInteger i) {
        this.value = i.multiply(CatalaInteger.of(100)).asBigInteger();
    }

    public CatalaMoney(Double dec) {
        this(new CatalaDecimal(dec));
    }

    public static final CatalaMoney ofCents(final String cents) {
        return new CatalaMoney(new BigInteger(cents));
    }

    public static final CatalaMoney ofCents(final BigInteger cents) {
        return new CatalaMoney(cents);
    }

    public static final CatalaMoney ofCents(final CatalaInteger cents) {
        return new CatalaMoney(cents.asBigInteger());
    }

    public static final CatalaMoney ofCents(long cents) {
        return new CatalaMoney(BigInteger.valueOf(cents));
    }

    public static final CatalaMoney ofCents(int cents) {
        return new CatalaMoney(BigInteger.valueOf(cents));
    }

    public static final CatalaMoney of(String units) {
        return new CatalaMoney(new BigInteger(units).multiply(BigInteger.valueOf(100)));
    }

    public static final CatalaMoney of(BigInteger units) {
        return new CatalaMoney(units.multiply(BigInteger.valueOf(100)));
    }

    public static final CatalaMoney of(final CatalaInteger cents) {
        return new CatalaMoney(cents);
    }

    public static final CatalaMoney of(long cents) {
        return new CatalaMoney(cents);
    }

    public static final CatalaMoney of(int cents) {
        return new CatalaMoney(cents);
    }

    public final BigInteger asBigIntegerCents() {
        return this.value;
    }

    public final CatalaInteger asIntegerCents() {
        return new CatalaInteger(this.value);
    }

    public final int asIntCents() {
        return this.value.intValueExact();
    }

    public final long asLongCents() {
        return this.value.longValueExact();
    }

    public final CatalaDecimal asDecimal() {
        return new CatalaDecimal(new CatalaInteger(this.value),
                new CatalaInteger(BigInteger.valueOf(100)));
    }

    public final double asDouble() {
        return new CatalaDecimal(new CatalaInteger(this.value),
                new CatalaInteger(BigInteger.valueOf(100))).asDouble();
    }

    public final CatalaInteger asInteger() {
        BigDecimal bd = new BigDecimal(this.value).divide(new BigDecimal(100), 0, RoundingMode.HALF_UP);
        return new CatalaInteger(bd.toBigInteger());
    }

    public final int asInt() {
        return this.value.intValueExact() / 100;
    }

    public final long asLong() {
        return this.value.longValueExact() / 100l;
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
        return new CatalaMoney(this.value.subtract(other.asBigIntegerCents()));
    }

    public final CatalaMoney add(CatalaMoney other) {
        return new CatalaMoney(this.value.add(other.asBigIntegerCents()));
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
        CatalaDecimal cd = CatalaDecimal.of(this.multiply(CatalaInteger.of(100)));
        CatalaDecimal resDecimal = cd.multiply(other);
        BigDecimal resBigDecimal = resDecimal.asBigDecimal(0, RoundingMode.HALF_UP);
        return new CatalaMoney(resBigDecimal.toBigIntegerExact());
    }

    public final CatalaDecimal divide(CatalaPosition pos, CatalaMoney other) {
        if (other.value.equals(BigInteger.ZERO)) {
            throw CatalaError.error(CatalaError.Error.DivisionByZero, pos);
        }
        return new CatalaDecimal(new CatalaInteger(this.value), new CatalaInteger(other.value));
    }

    public final CatalaMoney divide(CatalaPosition pos, CatalaInteger other) {
        return this.divide(pos, new CatalaDecimal(other, new CatalaInteger(BigInteger.ONE)));
    }

    public final CatalaMoney divide(CatalaPosition pos, CatalaDecimal other) {
        return this.multiply(other.inverse(pos));
    }

    @Override
    public int compareTo(CatalaPosition p, CatalaMoney o) {
        return this.value.compareTo(o.value);
    }

    @Override
    public CatalaBool equalsTo(CatalaPosition p, CatalaMoney o) {
        return CatalaBool.of(this.value.equals(o.value));

    }

    @Override
    public String toString() {
        BigInteger[] unitsAndCents = this.value.divideAndRemainder(BigInteger.valueOf(100));
        if (CatalaGlobals.lang == CatalaGlobals.Language.EN) {
            NumberFormat nf = NumberFormat.getInstance(Locale.ENGLISH);
            return String.format("$%s.%02d", nf.format(unitsAndCents[0]), unitsAndCents[1]);
        } else {
            NumberFormat nf = NumberFormat.getInstance(Locale.FRENCH);
            return String.format("%s,%02d€", nf.format(unitsAndCents[0]), unitsAndCents[1]);
        }
    }

    @Override
    public String toJSONString() {
        return '"' + this.value.toString() + '"';
    }
}
