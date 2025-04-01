package catala.runtime;

import java.math.BigInteger;

public final class CatalaMoney implements CatalaValue {
    // cents
    private final BigInteger value;

    public CatalaMoney(BigInteger value){
        this.value = value;
    }

    public final BigInteger asCents(){
        return this.value;
    }
}