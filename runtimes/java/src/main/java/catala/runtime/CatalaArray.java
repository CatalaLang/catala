package catala.runtime;

import java.util.stream.Stream;

import catala.runtime.exception.CatalaException;

public final class CatalaArray<T extends CatalaValue> implements CatalaValue {

    private final T[] values;

    public CatalaArray(T... values) {
        this.values = values;
    }

    public T[] asArray() {
        return this.values;
    }

    public final CatalaInteger length() {
        return new CatalaInteger(this.values.length);
    }

    public final T get(int index) {
        return this.values[index];
    }

    public final T get(int index, Class<T> clazz) {
        return (T) this.values[index];
    }

    public <R extends CatalaValue> CatalaArray<R> map(CatalaFunction<T, R> func) {
        return new CatalaArray<>(Stream.of(this.values).map(func).toArray(size -> (R[]) new CatalaValue[size]));
    }

    public <U extends CatalaValue, R extends CatalaValue> CatalaArray<R> map2(
            SourcePosition pos, CatalaFunction<CatalaTuple, R> func, CatalaArray<U> other) {
        if (this.values.length != other.values.length) {
            throw new CatalaException("traversing multiple lists of different lengths at " + pos);
        }
        int length = this.values.length;
        R[] tuples = (R[]) new CatalaValue[length];
        for (int i = 0; i < length; i++) {
            tuples[i] = func.apply(new CatalaTuple(this.values[i], this.values[i]));
        }
        return new CatalaArray<>(tuples);
    }

    public CatalaArray<T> filter(CatalaFunction<T, CatalaBool> func) {
        return new CatalaArray<>(Stream.of(this.values).filter(
                x -> func.apply(x) == CatalaBool.TRUE
        ).toArray(size -> (T[]) new CatalaValue[size]));
    }

    public <U extends CatalaValue> U foldLeft(CatalaFunction<CatalaTuple, U> folder, U init) {
        U result = init;
        for (T element : this.values) {
            result = folder.apply(new CatalaTuple(element, result));
        }
        return result;
    }

    public T reduce(CatalaFunction<CatalaTuple, T> reducer, CatalaFunction<CatalaUnit, T> dflt) {
        if (this.values.length == 0) {
            return dflt.apply(CatalaUnit.INSTANCE);
        } else {
            T result = this.values[0];
            for (int i = 1; i < this.values.length; i++) {
                T elt = this.values[i];
                result = reducer.apply(new CatalaTuple(elt, result));
            }
            return result;
        }
    }

    public CatalaArray<T> append(CatalaArray<T> other) {
        return new CatalaArray<>(
                Stream.concat(Stream.of(this.values),
                        Stream.of(other.values)).toArray(size -> (T[]) new CatalaValue[size]));
    }

    @Override
    public CatalaBool equalsTo(CatalaValue v) {
        if (v instanceof CatalaArray catalaArray) {
            CatalaArray<CatalaValue> va  = catalaArray;
            if (this.values.length != va.values.length) {
                return CatalaBool.FALSE;
            } else {
                for (int i = 0; i < this.values.length; i++) {
                    if (!(this.values[i].equalsTo(va.values[i]).asBoolean())) {
                        return CatalaBool.FALSE;
                    }
                }
                return CatalaBool.TRUE;
            }
        } else {
            return CatalaBool.FALSE;
        }
    }
}
