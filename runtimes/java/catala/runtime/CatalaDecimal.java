package catala.runtime;

import catala.runtime.exception.CatalaError;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.Locale;
import org.apache.commons.numbers.fraction.BigFraction;

// We probably want to keep this class as-is
// and not derive it from BigFraction as BigFraction is not part of
// java's stdlib and is currently vendored. (note: should we shadow
// the package name for BigFraction?)
public final class CatalaDecimal extends CatalaValue<CatalaDecimal> {

    private final BigFraction value;

    private /* not sure of this? We might want to avoid let BigFraction escape in the API? */ CatalaDecimal(BigFraction value) {
        this.value = value;
    }

    public CatalaDecimal(int numerator, int denominator) {
        this.value = BigFraction.of(numerator, denominator);
    }

    public CatalaDecimal(final CatalaInteger numerator, final CatalaInteger denominator) {
        this.value = BigFraction.of(numerator.asBigInteger(), denominator.asBigInteger());
    }

    public CatalaDecimal(Double d) {
        this.value = BigFraction.from(d);
    }

    public CatalaDecimal(int d) {
        this(d, 1);
    }

    public static final CatalaDecimal of(Double d) {
        return new CatalaDecimal(d);
    }

    public static final CatalaDecimal of(CatalaInteger ci) {
        return new CatalaDecimal(BigFraction.of(ci.asBigInteger()));
    }

    public static final CatalaDecimal of(int i) {
        return new CatalaDecimal(i, 1);
    }

    public static final CatalaDecimal of(long l) {
        return new CatalaDecimal(CatalaInteger.of(l), CatalaInteger.ONE);
    }

    public static final CatalaDecimal of(String s) {
        return new CatalaDecimal(BigFraction.parse(s));
    }

    public static final CatalaDecimal of(CatalaMoney cm) {
        return new CatalaDecimal(cm.asIntegerCents(), new CatalaInteger(100));
    }

    public CatalaInteger asInteger() {
        return new CatalaInteger(this.value.getNumerator().divide(this.value.getDenominator()));
    }

    public int asInt() {
        return this.asInteger().asInt();
    }

    public long asLong() {
        return this.asInteger().asLong();
    }

    final BigDecimal asBigDecimal(int scale, RoundingMode roundingMode) {
        return this.value.bigDecimalValue(scale, roundingMode);
    }

    final BigDecimal asBigDecimal() {
        return this.value.bigDecimalValue();
    }

    final Double asDouble() {
        return this.value.bigDecimalValue().doubleValue();
    }

    public final CatalaMoney asMoney() {
        return new CatalaMoney(this);
    }

    public CatalaDecimal negate() {
        return new CatalaDecimal(this.value.negate());
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

    public final CatalaDecimal inverse(CatalaPosition pos) {
        if (this.value.getNumerator().equals(BigInteger.ZERO)) {
            throw CatalaError.error(CatalaError.Error.DivisionByZero, pos);
        }
        return new CatalaDecimal(new CatalaInteger(this.getDenominator()), new CatalaInteger(this.getNumerator()));
    }

    public CatalaDecimal divide(CatalaPosition pos, CatalaDecimal denum) {
        try {
            return new CatalaDecimal(this.value.divide(denum.value));
        } catch (ArithmeticException e) {
            throw CatalaError.error(CatalaError.Error.DivisionByZero, pos);
        }
    }

    public CatalaDecimal roundDecimal() {
        return new CatalaDecimal(this.round(), new CatalaInteger(1));
    }

    public CatalaInteger round() {
        return new CatalaInteger(this.asBigDecimal(0, RoundingMode.HALF_UP).toBigInteger());
    }

    public final CatalaDecimal divide(CatalaDecimal other) {
        return new CatalaDecimal(this.value.divide(other.value));
    }

    public final CatalaDecimal divide(CatalaInteger other) {
        return new CatalaDecimal(this.value.divide(other.asBigInteger()));
    }

    @Override
    public int compareTo(CatalaDecimal other) {
        return this.value.compareTo(other.value);
    }

    @Override
    public int compareTo(CatalaPosition p, CatalaDecimal o) {
        return this.value.compareTo(o.value);
    }

    @Override
    public CatalaBool equalsTo(CatalaPosition p, CatalaDecimal o) {
        return CatalaBool.of(this.value.equals(o.value));
    }

    @Override
    public String toString() {
        NumberFormat nf;
        BigDecimal f = this.asBigDecimal(10, RoundingMode.HALF_UP);
        if (CatalaGlobals.lang == CatalaGlobals.Language.EN) {
            nf = NumberFormat.getInstance(Locale.ENGLISH);
        } else {
            nf = NumberFormat.getInstance(Locale.FRENCH);
        }
        DecimalFormat x = (DecimalFormat) nf;
        x.setParseBigDecimal(true);
        x.setMaximumFractionDigits(20);
        return x.format(f);
    }

    @Override
    public String toJSONString() {
        return '"' + this.value.bigDecimalValue().toString() + '"';
    }
}
