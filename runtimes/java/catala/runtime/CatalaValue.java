package catala.runtime;

public interface CatalaValue {

    public CatalaBool equalsTo(CatalaValue v);

    @Override
    public String toString();
}
