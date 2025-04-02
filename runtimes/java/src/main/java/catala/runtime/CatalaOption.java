package catala.runtime;

public final class CatalaOption<T extends CatalaValue> implements CatalaValue {

    public final T value;

    public CatalaOption(T in) {
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
