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

    //keeping this package-level for now
    CatalaDecimal(int num, int den) {
        this.value = BigFraction.of(num, den);
    }

    public CatalaDecimal(CatalaInteger num, CatalaInteger den) {
        this.value = BigFraction.of(num.asBigInteger(), den.asBigInteger());
    }

    /**
     * @throws IllegalArgumentException if the given {@code value} is NaN or
     * infinite.
     */
    public CatalaDecimal(Double d) {
        this.value = BigFraction.from(d);
    }

    public CatalaInteger asInteger() {
        return new CatalaInteger(this.value.getNumerator().divide(this.value.getDenominator()));
    }

    public CatalaDecimal negate() {
        return new CatalaDecimal(this.value.negate());
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

    public CatalaDecimal round() {
        return new CatalaDecimal(new CatalaInteger(this.bigDecimalValue(0, RoundingMode.HALF_UP).toBigInteger()),
                new CatalaInteger(1));
    }

    final BigDecimal bigDecimalValue(int scale, RoundingMode roundingMode) {
        return this.value.bigDecimalValue(scale, roundingMode);
    }

    final BigDecimal bigDecimalValue() {
        return this.value.bigDecimalValue();
    }

    final Double doubleValue() {
        return this.value.bigDecimalValue().doubleValue();
    }

    public static final CatalaDecimal ofInteger(CatalaInteger ci) {
        return new CatalaDecimal(BigFraction.of(ci.asBigInteger()));
    }

    public static final CatalaDecimal ofMoney(CatalaMoney cm) {
        return new CatalaDecimal(BigFraction.of(cm.asCents(), BigInteger.valueOf(100)));
    }

    public final CatalaMoney asMoney() {
        return CatalaMoney.ofCents(BigInteger.valueOf(100)).multiply(this);
    }

    static final CatalaDecimal ofMoneyAsCents(CatalaMoney cm) {
        return new CatalaDecimal(BigFraction.of(cm.asCents()));
    }

    public static final CatalaDecimal ofDouble(Double d) {
        return new CatalaDecimal(d);
    }

    public final CatalaDecimal divide(CatalaDecimal other) {
        return new CatalaDecimal(this.value.divide(other.value));
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
        return CatalaBool.fromBoolean(this.value.equals(o.value));
    }

    @Override
    public String toString() {
        NumberFormat nf;
        BigDecimal f = this.bigDecimalValue(10, RoundingMode.HALF_UP);
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
