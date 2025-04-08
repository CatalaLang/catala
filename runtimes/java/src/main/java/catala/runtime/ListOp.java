package catala.runtime;

import java.util.function.BiFunction;
import java.util.stream.Stream;


public final class ListOp {
    public static <T extends CatalaValue, R extends CatalaValue> R[] map(CatalaFunction<T,R> func, T[] arr){
        return Stream.of(arr).map(func).toArray(size -> (R[]) new CatalaValue[size]);
    }

    public static <T extends CatalaValue> T[] filter(CatalaFunction<T, CatalaBool> func, T[] arr){
        return Stream.of(arr).filter(CatalaPredicate.ofCatalaFunction(func)).toArray(size -> (T[]) new CatalaValue[size]);
    }
    
    //map2

    //reduce
    public static <T extends CatalaValue> T reduce(BiFunction<T, T, T> reducer, CatalaFunction<CatalaUnit, T> dflt, T[] arr){
        return Stream.of(arr).reduce((a, b) -> reducer.apply(a, b)).orElse(dflt.apply(CatalaUnit.INSTANCE));
    }

    //fold_left

}