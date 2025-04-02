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

    /**
     * @param other {@inheritDoc}
     * @return {@inheritDoc}
     */
    @Override
    public int compareTo(CatalaMoney other) {
        return this.value.compareTo(other.value);
    }
}
