package catala.runtime;

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

    public static <T extends CatalaValue, U extends CatalaValue> U foldLeft(CatalaFunction<CatalaTuple, U> folder, U init, T[] arr) {
        U result = init;
        for (T element : arr) {
            result = folder.apply(new CatalaTuple(element, result));
        }
        return result;
    }

    public static <T extends CatalaValue> T reduce(CatalaFunction<CatalaTuple, T> reducer, CatalaFunction<CatalaUnit, T> dflt, T[] arr) {
        if (arr.length == 0) {
            return dflt.apply(CatalaUnit.INSTANCE);
        } else {
            T result = arr[0];
            for (int i = 1; i < arr.length; i++) {
                T elt = arr[i];
                result = reducer.apply(new CatalaTuple(elt, result));
            }
            return result;
        }
    }

}
