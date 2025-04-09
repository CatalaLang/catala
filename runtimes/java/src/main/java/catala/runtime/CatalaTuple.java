package catala.runtime;

public final class CatalaTuple implements CatalaValue {

    public final CatalaValue[] values;

    public CatalaTuple(CatalaValue... values) {
        this.values = values;
    }

    public final CatalaValue get(int index) {
        return this.values[index];
    }

    public final <T extends CatalaValue> T get(int index, Class<T> clazz) {
        return (T) this.values[index];
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
}
