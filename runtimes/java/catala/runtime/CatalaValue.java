package catala.runtime;

public interface CatalaValue {

    public CatalaBool equalsTo(CatalaValue v);

    @Override
    public String toString();

    @SuppressWarnings("unchecked")
    public static <T extends CatalaValue> T cast(CatalaValue v) {
        return (T) v;
    }
}
