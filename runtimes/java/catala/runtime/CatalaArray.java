package catala.runtime;

import catala.runtime.exception.CatalaError;
import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.SortedMap;
import java.util.TreeMap;
import java.util.stream.Stream;

public final class CatalaArray<T extends CatalaValue<?>> extends CatalaValue<CatalaArray<T>> {

    private final T[] values;

    @SafeVarargs
    @SuppressWarnings("varargs")
    public CatalaArray(T... values) {
        this.values = Arrays.copyOf(values, values.length);
    }

    public CatalaArray(Stream<T> values) {
        Object[] l = values.toList().toArray();
        @SuppressWarnings("unchecked")
        T[] arr = (T[]) Arrays.copyOf(l, l.length, CatalaValue[].class);
        this.values = arr;
    }

    public T[] asArray() {
        return Arrays.copyOf(this.values, this.values.length);
    }

    public Stream<T> asStream() {
        return Stream.of(this.values);
    }

    public List<T> asList() {
        return this.asStream().toList();
    }

    public final CatalaInteger length() {
        return new CatalaInteger(this.values.length);
    }

    public final T get(int index) {
        return this.values[index];
    }

    public final T get(int index, Class<T> clazz) {
        return this.values[index];
    }

    public <R extends CatalaValue<?>> CatalaArray<R> map(CatalaFunction<T, R> func) {
        return new CatalaArray<>(Stream.of(this.values).map(func));
    }

    @SuppressWarnings("unchecked")
    public <U extends CatalaValue<?>, R extends CatalaValue<?>> CatalaArray<R> map2(
            CatalaPosition pos, CatalaFunction<CatalaTuple, R> func, CatalaArray<U> other) {
        if (this.values.length != other.values.length) {
            throw CatalaError.error(CatalaError.Error.NotSameLength, pos);
        }
        int length = this.values.length;
        if (this.values.length == 0) {
            return new CatalaArray<>();
        } else {
            R first = func.apply(new CatalaTuple(this.values[0], other.values[0]));
            R[] r = (R[]) Array.newInstance(first.getClass(), this.values.length);
            r[0] = first;
            for (int i = 1; i < length; i++) {
                r[i] = func.apply(new CatalaTuple(this.values[i], other.values[i]));
            }
            return new CatalaArray<>(r);
        }
    }

    public CatalaArray<T> filter(CatalaFunction<T, CatalaBool> func) {
        return new CatalaArray<>(Stream.of(this.values).filter(
                x -> func.apply(x) == CatalaBool.TRUE
        ));
    }

    public <U extends CatalaValue<?>> U foldLeft(CatalaFunction<CatalaTuple, U> folder, U init) {
        U result = init;
        for (T element : this.values) {
            result = folder.apply(new CatalaTuple(result, element));
        }
        return result;
    }

    public CatalaOption<T> reduce(CatalaFunction<CatalaTuple, T> reducer) {
        if (this.values.length == 0) {
            return CatalaOption.none();
        } else {
            T result = this.values[0];
            for (int i = 1; i < this.values.length; i++) {
                T elt = this.values[i];
                result = reducer.apply(new CatalaTuple(result, elt));
            }
            return CatalaOption.some(result);
        }
    }

    public CatalaArray<T> append(CatalaArray<T> other) {
        T[] newArray = Arrays.copyOf(this.values, this.values.length + other.values.length);
        System.arraycopy(other.values, 0, newArray, this.values.length, other.values.length);
        return new CatalaArray<>(newArray);
    }

    public CatalaOption<T> find(CatalaFunction<T, CatalaBool> f) {
        for (T elt : this.values) {
            if (f.apply(elt).asBoolean()) {
                return CatalaOption.some(elt);
            }
        }
        return CatalaOption.none();
    }

    private <U extends CatalaValue<?>> CatalaArray<T> sort(CatalaPosition pos, Comparator<U> cmp, CatalaFunction<T, U> f) {
        SortedMap<U, List<T>> m = new TreeMap<>(cmp);
        for (T value : this.values) {
            U k = f.apply(value);
            List<T> curr = m.get(k);
            if (curr == null) {
                List<T> l = new ArrayList<>();
                l.add(value);
                m.put(k, l);

            } else {
                curr.addLast(value);
            }
        }
        return new CatalaArray<>(m.values().stream().flatMap(Collection::stream));
    }

    public <U extends CatalaValue<?>> CatalaArray<T> sort_asc(CatalaPosition pos, CatalaFunction<T, U> f) {
        Comparator<U> cmp = (U o1, U o2) -> o1.compareTo(pos, o2);
        return sort(pos, cmp, f);
    }

    public <U extends CatalaValue<?>> CatalaArray<T> sort_desc(CatalaPosition pos, CatalaFunction<T, U> f) {
        Comparator<U> cmp = (U o1, U o2) -> o2.compareTo(pos, o1);
        return sort(pos, cmp, f);
    }

    @Override
    public CatalaBool equalsTo(CatalaPosition p, CatalaArray<T> v) {
        if (v instanceof CatalaArray<T> other_array
                && other_array.getClass().isAssignableFrom(this.getClass())) {
            if (other_array.values.length != this.values.length) {
                return CatalaBool.FALSE;
            } else {
                for (int i = 0; i < this.values.length; i++) {
                    if (!(this.values[i].equalsTo(p, other_array.values[i]).asBoolean())) {
                        return CatalaBool.FALSE;
                    }
                }
                return CatalaBool.TRUE;
            }
        } else {
            return CatalaBool.FALSE;
        }
    }

    @Override
    public int compareTo(CatalaPosition p, CatalaArray<T> o) {
        T[] l = this.asArray();
        T[] r = o.asArray();
        for (int i = 0; i < Integer.min(l.length, r.length); i++) {
            int cmp = l[i].compareTo(p, r[i]);
            if (cmp == 0) {
                continue;
            }
            return cmp;
        }
        return Integer.compare(l.length, r.length);
    }

    @Override
    public String toString() {
        if (this.values.length == 0) {
            return "[ ]";
        }
        StringBuilder b = new StringBuilder();
        b.append("[\n");
        StringBuilder newb = new StringBuilder();
        for (int i = 0; i < values.length; i++) {
            newb.append(values[i].toString());
            if (i < values.length - 1) {
                newb.append("; ");
            }
        }
        // indent adds a newline for some reason, thus, we do not add one.
        b.append(newb.toString().indent(2)).append("]");
        return b.toString();
    }

    @Override
    public String toJSONString() {
        StringBuilder b = new StringBuilder();
        b.append("[ ");
        for (int i = 0; i < values.length; i++) {
            b.append(values[i].toJSONString());
            if (i < values.length - 1) {
                b.append(", ");
            }
        }
        b.append(" ]");
        return b.toString();
    }
}
