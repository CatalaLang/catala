package catala.runtime;

public final class CatalaUnit extends CatalaValue<CatalaUnit> {

    private CatalaUnit() {
    }

    public static final CatalaUnit INSTANCE = new CatalaUnit();

    @Override
    public final String toString() {
        return "unit";
    }

    @Override
    public CatalaBool equalsTo(CatalaPosition p, CatalaUnit o) {
        return CatalaBool.TRUE;
    }

    @Override
    public int compareTo(CatalaPosition p, CatalaUnit o) {
        return 0;
    }

    @Override
    public String toJSONString() {
        // Should not happen
        return "{}";
    }
}
