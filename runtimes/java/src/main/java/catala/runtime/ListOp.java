package catala.runtime;

import java.util.function.Predicate;
import java.util.stream.Stream;


public class ListOp {
    public static <T extends CatalaValue, R extends CatalaValue> R[] map(CatalaFunction<T,R> func, T[] arr){
        return Stream.of(arr).map(func).toArray(size -> (R[]) new CatalaValue[size]);
    }

    public static <T extends CatalaValue> filter(CatalaPredicate<T> func, T[] arr){
        return Stream.of(arr).filter();
    }


}