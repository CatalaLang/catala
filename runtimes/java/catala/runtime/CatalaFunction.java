package catala.runtime;

import catala.runtime.exception.CatalaError;
import java.util.function.Function;

public interface CatalaFunction<Tin extends CatalaValue, Tout extends CatalaValue>
        extends Function<Tin, Tout> {

    default public CatalaBool equalsTo(Object o) {
        // Functions are not comparable
        return CatalaBool.FALSE;
    }

    default public CatalaBool equalsTo(CatalaPosition p, Object o) {
        // Functions are not comparable
        return CatalaBool.FALSE;
    }

    default public int compareTo(CatalaPosition p, Object o) {
        throw CatalaError.error(CatalaError.Error.UncomparableValues, p);
    }

    default public int compareTo(Object o) {
        throw CatalaError.error(CatalaError.Error.UncomparableValues);
    }

    default public CatalaBool lessThan(CatalaPosition p, Object o) {
        throw CatalaError.error(CatalaError.Error.UncomparableValues);
    }

    default public CatalaBool lessThan(Object o) {
        throw CatalaError.error(CatalaError.Error.UncomparableValues);
    }

    default public CatalaBool lessEqThan(CatalaPosition p, Object o) {
        throw CatalaError.error(CatalaError.Error.UncomparableValues);
    }

    default public CatalaBool lessEqThan(Object o) {
        throw CatalaError.error(CatalaError.Error.UncomparableValues);
    }

    default public CatalaBool greaterThan(CatalaPosition p, Object o) {
        throw CatalaError.error(CatalaError.Error.UncomparableValues);
    }

    default public CatalaBool greaterThan(Object o) {
        throw CatalaError.error(CatalaError.Error.UncomparableValues);
    }

    default public CatalaBool greaterEqThan(CatalaPosition p, Object o) {
        throw CatalaError.error(CatalaError.Error.UncomparableValues);
    }

    default public CatalaBool greaterEqThan(Object o) {
        throw CatalaError.error(CatalaError.Error.UncomparableValues);
    }
}
