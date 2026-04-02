package catala.runtime;

public final class CatalaBool extends CatalaValue<CatalaBool> {

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
    public CatalaBool equalsTo(CatalaPosition p, CatalaBool other) {
        return fromBoolean(this.value == other.value);
    }

    @Override
    public int compareTo(CatalaPosition p, CatalaBool o) {
        if (this.equals(o)) {
            return 0;
        }
        if (this.asBoolean()) {
            return 1;
        } else {
            return -1;
        }
    }

    @Override
    public String toString() {
        if (this.value) {
            if (CatalaGlobals.lang == CatalaGlobals.Language.FR) {
                return "vrai";
            } else {
                return "true";
            }
        } else {
            if (CatalaGlobals.lang == CatalaGlobals.Language.FR) {
                return "faux";
            } else {
                return "false";
            }
        }
    }

    @Override
    public String toJSONString() {
        return this.toString();
    }
}
