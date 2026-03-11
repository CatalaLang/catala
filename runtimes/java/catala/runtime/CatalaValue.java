package catala.runtime;

import catala.runtime.exception.CatalaError;

public abstract class CatalaValue<T extends CatalaValue<?>> implements Comparable<T> {

    public abstract CatalaBool equalsTo(CatalaPosition p, T v);

    @Override
    public abstract String toString();

    @SuppressWarnings("unchecked")
    public static <U extends CatalaValue<?>> U cast(CatalaValue<?> v) {
        return (U) v;
    }

    public abstract int compareTo(CatalaPosition p, T o);

    @Override
    public int compareTo(T o) {
        return compareTo(CatalaPosition.empty, o);
    }

    @SuppressWarnings("unchecked")
    public int compareTo(CatalaPosition p, Object o) {
        if (this.getClass().equals(o.getClass())) {
            return this.compareTo(p, (T) o);
        } else {
            throw CatalaError.error(CatalaError.Error.UncomparableValues, p);
        }
    }

    public CatalaBool equalsTo(T v) {
        return this.equalsTo(CatalaPosition.empty, v);
    }

    @SuppressWarnings("unchecked")
    public CatalaBool equalsTo(CatalaPosition p, Object o) {
        if (this.getClass().equals(o.getClass())) {
            return this.equalsTo(p, (T) o);
        } else {
            return CatalaBool.FALSE;
        }
    }

    @Override
    @SuppressWarnings("unchecked")
    public boolean equals(Object o) {
        if (this.getClass().equals(o.getClass())) {
            return this.equalsTo(CatalaPosition.empty, (T) o).asBoolean();
        } else {
            return false;
        }
    }

    public CatalaBool lessThan(CatalaPosition p, T o) {
        return CatalaBool.fromBoolean(this.compareTo(p, o) < 0);
    }

    public CatalaBool lessThan(T o) {
        return lessThan(CatalaPosition.empty, o);
    }

    public CatalaBool lessEqThan(CatalaPosition p, T o) {
        return CatalaBool.fromBoolean(this.compareTo(p, o) <= 0);
    }

    public CatalaBool lessEqThan(T o) {
        return lessEqThan(CatalaPosition.empty, o);
    }

    public CatalaBool greaterThan(CatalaPosition p, T o) {
        return CatalaBool.fromBoolean(this.compareTo(p, o) > 0);
    }

    public CatalaBool greaterThan(T o) {
        return greaterThan(CatalaPosition.empty, o);
    }

    public CatalaBool greaterEqThan(CatalaPosition p, T o) {
        return CatalaBool.fromBoolean(this.compareTo(p, o) >= 0);
    }

    public CatalaBool greaterEqThan(T o) {
        return greaterEqThan(CatalaPosition.empty, o);
    }

}
