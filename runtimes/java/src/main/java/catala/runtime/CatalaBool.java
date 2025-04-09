package catala.runtime;

public final class CatalaBool implements CatalaValue {

    public static final CatalaBool TRUE = new CatalaBool(true);
    public static final CatalaBool FALSE = new CatalaBool(false);
    private final boolean value;

    private CatalaBool(boolean value) {
        this.value = value;
    }

    public final boolean asBoolean() {
        return value;
    }

    public static CatalaBool fromBoolean(boolean b) {
        if (b) {
            return TRUE;
        } else {
            return FALSE;
        }
    }

    public CatalaBool and(CatalaBool other) {
        return new CatalaBool(this.value && other.value);
    }

    public CatalaBool or(CatalaBool other) {
        return new CatalaBool(this.value || other.value);
    }

    public CatalaBool xor(CatalaBool other) {
        return new CatalaBool(this.value ^ other.value);
    }

    @Override
    public CatalaBool equalsTo(CatalaValue other) {
        if (other instanceof CatalaBool catalaBool) {
            return new CatalaBool(this.value == catalaBool.value);
        } else {
            return FALSE;
        }
    }

}
