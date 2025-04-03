package catala.runtime;

import java.math.BigInteger;

// We could make this class extend java.math.BigInteger
// for consumer friendliness?
public final class CatalaInteger implements CatalaValue, Comparable<CatalaInteger> {

    private final BigInteger value;

    public CatalaInteger(BigInteger value) {
        this.value = value;
    }

    public CatalaInteger(String value) {
        this.value = new BigInteger(value);
    }

    public final BigInteger asBigInteger() {
        return this.value;
    }

    // ToInt_rat
    public static final CatalaInteger ofDecimal(CatalaDecimal dec) {
        return new CatalaInteger(dec.getNumerator().divide(dec.getDenominator()));
    }

    public static final CatalaInteger valueOf(long value){
        return new CatalaInteger(BigInteger.valueOf(value));
    }

    public final CatalaInteger negate(){
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
        if (this == obj) return true;
        if (obj == null || getClass() != obj.getClass()) return false;
        CatalaInteger other = (CatalaInteger) obj;
        return this.value.equals(other.value);
    }

    @Override
    public int hashCode() {
        return this.value.hashCode();
    }

    @Override
    public String toString(){
        return this.value.toString();
    }

    public CatalaInteger add(CatalaInteger i) {
        return new CatalaInteger(this.value.add(i.value));
    }

    public CatalaInteger sub(CatalaInteger i) {
        return new CatalaInteger(this.value.subtract(i.value));
    }

}
