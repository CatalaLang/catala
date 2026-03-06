package catala.runtime;

import catala.runtime.exception.CatalaError;

public final class CatalaTuple extends CatalaValue<CatalaTuple> {

    public final CatalaValue[] values;

    public CatalaTuple(CatalaValue... values) {
        this.values = values;
    }

    @SuppressWarnings("unchecked")
    public final CatalaValue get(int index) {
        return this.values[index];
    }

    public final <T extends CatalaValue> T get(int index, Class<T> clazz) {
        return clazz.isInstance(this.values[index]) ? clazz.cast(this.values[index]) : null;
    }

    @Override
    public CatalaBool equalsTo(CatalaPosition p, CatalaTuple v) {
        if (this.values.length != v.values.length) {
            return CatalaBool.FALSE;
        } else {
            for (int i = 0; i < this.values.length; i++) {
                if (!(this.values[i].equalsTo(v.values[i]).asBoolean())) {
                    return CatalaBool.FALSE;
                }
            }
            return CatalaBool.TRUE;
        }
    }

    @Override
    public int compareTo(CatalaPosition p, CatalaTuple o) {
        if (this.values.length != o.values.length) {
            throw CatalaError.error(CatalaError.Error.UncomparableValues, p);
        }
        int cmp = 0;
        for (int i = 0; i < this.values.length; i++) {
            cmp = this.values[i].compareTo(p, o.values[i]);
            if (cmp == 0) {
                continue;
            }
            return cmp;
        }
        return cmp;
    }

    @Override
    public String toString() {
        StringBuilder s = new StringBuilder();
        s.append('(');
        int len = this.values.length;
        for (int i = 0; i < len; i++) {
            s.append(this.values[i].toString());
            if (i < len - 1) {
                s.append(", ");
            }
        }
        s.append(')');
        return s.toString();
    }
}
