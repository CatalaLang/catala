package catala.runtime;

import java.util.function.BiFunction;
import java.util.stream.Stream;

public final class ListOp {

    public static <T extends CatalaValue, R extends CatalaValue> R[] map(CatalaFunction<T, R> func, T[] arr) {
        return Stream.of(arr).map(func).toArray(size -> (R[]) new CatalaValue[size]);
    }

    private static CatalaTuple zip(CatalaValue u, CatalaValue v) {
        return new CatalaTuple(u, v);
    }

    public static <T extends CatalaValue, U extends CatalaValue, R extends CatalaValue> R[] map2(
            CatalaFunction<CatalaTuple, R> func, T[] arr1, U[] arr2) {
        int minLength = Math.min(arr1.length, arr2.length);

        CatalaTuple[] tuples = new CatalaTuple[minLength];
        for (int i = 0; i < minLength; i++) {
            tuples[i] = zip(arr1[i], arr2[i]);
        }

        return map(func, tuples);
    }

    public static <T extends CatalaValue> T[] filter(CatalaFunction<T, CatalaBool> func, T[] arr) {
        return Stream.of(arr).filter(CatalaPredicate.ofCatalaFunction(func)).toArray(size -> (T[]) new CatalaValue[size]);
    }

    public static <T extends CatalaValue> T reduce(BiFunction<T, T, T> reducer, CatalaFunction<CatalaUnit, T> dflt, T[] arr) {
        return Stream.of(arr).reduce((a, b) -> reducer.apply(a, b)).orElse(dflt.apply(CatalaUnit.INSTANCE));
    }

}
