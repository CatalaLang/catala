package catala.runtime;

import java.math.BigInteger;

public final class CatalaInteger implements CatalaValue {
    private final BigInteger value;

    public CatalaInteger(BigInteger value){
        this.value = value;
    }

    public final BigInteger asBigInteger(){
        return this.value;
    }
}