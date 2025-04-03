package catala.runtime;

import java.math.BigInteger;

public final class CatalaMoney implements CatalaValue, Comparable<CatalaMoney> {

    // cents
    private final BigInteger value;

    CatalaMoney(BigInteger valueCents) {
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
        return String.format("%d.%02d", (Object[]) unitsAndCents);
    }

    public static final CatalaMoney ofCents(String cents){
      return new CatalaMoney(new BigInteger(cents));
    }

    public static final CatalaMoney ofCents(BigInteger cents){
      return new CatalaMoney(cents);
    }

    public static final CatalaMoney ofUnits(String units){
        return new CatalaMoney(new BigInteger(units).multiply(BigInteger.valueOf(100)));
    }

    public static final CatalaMoney ofUnits(BigInteger units){
        return new CatalaMoney(units.multiply(BigInteger.valueOf(100)));
    }
}
