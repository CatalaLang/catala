package catala.runtime;

public final class CatalaTuple implements CatalaValue {

    public final CatalaValue[] values;

    public CatalaTuple(CatalaValue... values){
        this.values = values;
    }

    public final CatalaValue get(int index){
        return this.values[index];
    }

}
