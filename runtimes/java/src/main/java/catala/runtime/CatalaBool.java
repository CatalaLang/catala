package catala.runtime;

public final class CatalaBool implements CatalaValue {

    public static final CatalaBool TRUE = new CatalaBool(true);
    public static final CatalaBool FALSE = new CatalaBool(false);
    private final boolean value;

    private CatalaBool(boolean value) {
        this.value = value;
    }

    public boolean asBoolean() {
        return value;
    }

    public static CatalaBool fromBoolean(boolean b) {
        if (b) {
            return TRUE;
        } else {
            return FALSE;
        }
    }
}
