package catala.runtime;

public final class CatalaOption<T extends CatalaValue> implements CatalaValue {

    public final T value;

    @SuppressWarnings("rawtypes")
    public static final CatalaOption NONE = new CatalaOption<>(null);

    public static <T extends CatalaValue> CatalaOption<T> some(T value) {
        if (value == null) {
            throw new IllegalArgumentException("'CatalaOption.some' requires a non-null value");
        }
        return new CatalaOption<>(value);
    }

    ;

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

    @Override
    public CatalaBool equalsTo(CatalaValue other) {
        if (other instanceof CatalaOption catalaOption) {
            return this.equalsTo(catalaOption);
        } else {
            return CatalaBool.FALSE;
        }
    }

    @Override
    public String toString() {
        if (this.isNone()) {
            return "None";
        } else {
            return "Some " + get().toString();
        }
    }
}
