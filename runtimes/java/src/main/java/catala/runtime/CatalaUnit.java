package catala.runtime;

public final class CatalaUnit implements CatalaValue {

    private CatalaUnit() {
    }

    public static final CatalaUnit INSTANCE = new CatalaUnit();

    @Override
    public final String toString() {
        return "Unit()";
    }
}
