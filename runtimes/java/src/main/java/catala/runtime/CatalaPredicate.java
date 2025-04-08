package catala.runtime;

import java.util.function.Predicate;

public interface CatalaPredicate<T extends CatalaValue> extends CatalaValue, Predicate<T>, CatalaFunction<T, CatalaBool> {

    public static <T extends CatalaValue> CatalaPredicate<T> ofCatalaFunction(CatalaFunction<T, CatalaBool> func) {
        return new CatalaPredicate<T>() {
            @Override
            public CatalaBool apply(T t) {
                return func.apply(t);
            }

            @Override
            public boolean test(T t) {
                return func.apply(t).asBoolean();
            }
        };
    }
}