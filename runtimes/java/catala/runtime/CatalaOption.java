package catala.runtime;

public final class CatalaOption<T extends CatalaValue> implements CatalaValue {

    public final T value;

    public static final CatalaOption NONE = new CatalaOption<>(null);

    public static final <T extends CatalaValue> CatalaOption<T> none() {
        @SuppressWarnings("unchecked")
        CatalaOption<T> t = (CatalaOption<T>) NONE;
        return t;
    }

    public static <T extends CatalaValue> CatalaOption<T> some(T value) {
        if (value == null) {
            throw new IllegalArgumentException("'CatalaOption.some' requires a non-null value");
        }
        return new CatalaOption<>(value);
    }

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
            if (this == NONE)
                return CatalaBool.fromBoolean(catalaOption == NONE);
            else if (catalaOption == NONE)
                return CatalaBool.FALSE;
            else
                return this.value.equalsTo(catalaOption.value);
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
