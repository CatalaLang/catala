package catala.runtime;

public final class CatalaOption<T extends CatalaValue> implements CatalaValue {

    public final T value;

    public static final CatalaOption<CatalaValue> NONE = new CatalaOption<>(null);

    public static CatalaOption<CatalaValue> some(CatalaValue value){
        if (value == null)
            throw new IllegalArgumentException("'CatalaOption.some' requires a non-null value");
        return new CatalaOption<>(value);
    };

    private CatalaOption(T in) {
        this.value = in;
    }

    public boolean isNone() {
        return (this.value == null);
    }

    public boolean isSome() {
        return (this.value != null);
    }

    public T get() {
        return this.value;
    }
}
