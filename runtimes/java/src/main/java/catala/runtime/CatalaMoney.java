package catala.runtime;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;

public final class CatalaMoney implements CatalaValue, Comparable<CatalaMoney> {

    // cents
    private final BigInteger value;

    private CatalaMoney(BigInteger valueCents) {
        this.value = valueCents;
    }

    public final BigInteger asCents() {
        return this.value;
    }

    public final CatalaMoney negate() {
        return new CatalaMoney(this.value.negate());
    }

    /**
     * Round to the nearest unit (multiple of 100 cents)
     */
    public final CatalaMoney round() {
       return new CatalaMoney(this.value.divide(BigInteger.valueOf(100)).multiply(BigInteger.valueOf(100)));
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (obj == null || getClass() != obj.getClass()) return false;
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
    public String toString(){
        BigInteger[] unitsAndCents = this.value.divideAndRemainder(BigInteger.valueOf(100));
        return String.format("Money(%d.%02d)", (Object[]) unitsAndCents);
    }

    public final CatalaMoney multiply(CatalaInteger other){
        return new CatalaMoney(this.value.multiply(other.asBigInteger()));
    }

    /**
     * Rounds to the nearest cent
     */
    public final CatalaMoney multiply(CatalaDecimal other){
        CatalaDecimal thisDecCents = 
          CatalaDecimal.ofMoneyAsCents(this);
        CatalaDecimal resDecimal = thisDecCents.multiply(other);
        BigDecimal resBigDecimal = resDecimal.bigDecimalValue(0, RoundingMode.HALF_UP);
        return new CatalaMoney(resBigDecimal.toBigIntegerExact());
    }

    public static final CatalaMoney ofCents(String cents){
      return new CatalaMoney(new BigInteger(cents));
    }

    public static final CatalaMoney ofCents(BigInteger cents){
      return new CatalaMoney(cents);
    }

    public static final CatalaMoney ofCents(long cents){
        return new CatalaMoney(BigInteger.valueOf(cents));
    }
}
