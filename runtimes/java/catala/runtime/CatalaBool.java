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

    public CatalaBool not() {
        return fromBoolean(!this.value);
    }

    public CatalaBool and(CatalaBool other) {
        return fromBoolean(this.value && other.value);
    }

    public CatalaBool or(CatalaBool other) {
        return fromBoolean(this.value || other.value);
    }

    public CatalaBool xor(CatalaBool other) {
        return fromBoolean(this.value ^ other.value);
    }

    @Override
    public CatalaBool equalsTo(CatalaValue other) {
        if (other instanceof CatalaBool catalaBool) {
            return fromBoolean(this.value == catalaBool.value);
        } else {
            return FALSE;
        }
    }

    @Override
    public String toString() {
        if (this.value) {
            return "true";
        } else {
            return "false";
        }
    }

}
