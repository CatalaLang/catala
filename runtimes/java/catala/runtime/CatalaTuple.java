package catala.runtime;

public final class CatalaTuple implements CatalaValue {

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
    public CatalaBool equalsTo(CatalaValue v) {
        if (v instanceof CatalaTuple catalaArray) {
            CatalaTuple va  = catalaArray;
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
