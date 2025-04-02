package catala.runtime;

import java.math.BigInteger;

public final class CatalaMoney implements CatalaValue, Comparable<CatalaMoney> {

    // cents
    private final BigInteger value;

    public CatalaMoney(BigInteger value) {
        this.value = value;
    }

    public final BigInteger asCents() {
        return this.value;
    }

    public final CatalaMoney negate() {
        return new CatalaMoney(this.value.negate());
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
}
