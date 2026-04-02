package catala.runtime;

public final class CatalaOption<T extends CatalaValue<?>> extends CatalaValue<CatalaOption<T>> {

    public final T value;

    private static final CatalaOption<? extends CatalaValue<?>> NONE = new CatalaOption<>(null);

    public static final <U extends CatalaValue<?>> CatalaOption<U> none() {
        @SuppressWarnings("unchecked")
        CatalaOption<U> t = (CatalaOption<U>) NONE;
        return t;
    }

    public static <U extends CatalaValue<?>> CatalaOption<U> some(U value) {
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
    public CatalaBool equalsTo(CatalaPosition p, CatalaOption<T> o) {
        if (this == NONE) {
            return CatalaBool.fromBoolean(o == NONE);
        } else if (o == NONE) {
            return CatalaBool.FALSE;
        } else {
            return this.value.equalsTo(p, o.value);
        }
    }

    @Override
    public int compareTo(CatalaPosition p, CatalaOption<T> o) {
        if (this.isNone() && o.isNone()) {
            return 0;
        }
        if (this.isNone()) {
            return -1;
        }
        if (o.isNone()) {
            return 1;
        }
        return this.get().compareTo(p, o.get());
    }

    @Override
    public String toString() {
        if (this.isNone()) {
            return "Absent";
        } else {
            if (CatalaGlobals.lang == CatalaGlobals.Language.FR) {
                return "Présent contenu " + get().toString();
            } else {
                return "Present content " + get().toString();
            }
        }
    }

    @Override
    public String toJSONString() {
        // Warning: structures with optional fields should skip this function and
        // print either nothing either directly the value skipping the enum repr
        if (this.isNone()) {
            return "Absent";
        } else {
            return String.format("{ \"Present\": %s }", get().toJSONString());
        }
    }
}
