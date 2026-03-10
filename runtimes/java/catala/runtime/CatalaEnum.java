package catala.runtime;

import catala.runtime.exception.CatalaError;
import java.lang.reflect.Field;

public class CatalaEnum extends CatalaValue<CatalaEnum> {

    private static Enum getKind(CatalaEnum o) throws IllegalAccessException, NoSuchFieldException {
        Field f = o.getClass().getDeclaredField("kind");
        f.setAccessible(true);
        return ((Enum) (f.get(o)));
    }

    private static CatalaValue getContents(CatalaEnum o) throws IllegalAccessException, NoSuchFieldException {
        Field f = o.getClass().getDeclaredField("contents");
        f.setAccessible(true);
        return ((CatalaValue) (f.get(o)));
    }

    @Override
    public CatalaBool equalsTo(CatalaPosition p, CatalaEnum o) {
        if (this.getClass() != o.getClass()) {
            return CatalaBool.FALSE;
        }
        try {
            Enum k1 = getKind(this);
            Enum k2 = getKind(o);
            if (k1.ordinal() != k2.ordinal()) {
                return CatalaBool.FALSE;
            }
            return getContents(this).equalsTo(getContents(o));
        } catch (IllegalAccessException | NoSuchFieldException e) {
            throw CatalaError.error(CatalaError.Error.UncomparableValues, p);
        }
    }

    @Override
    public int compareTo(CatalaPosition p, CatalaEnum o) {
        if (this.getClass() != o.getClass()) {
            throw CatalaError.error(CatalaError.Error.UncomparableValues, p);
        }
        try {
            Enum k1 = getKind(this);
            Enum k2 = getKind(o);
            if (k1.ordinal() != k2.ordinal()) {
                return k1.compareTo(k2);
            }
            return getContents(this).compareTo(getContents(o));
        } catch (IllegalAccessException | NoSuchFieldException e) {
            throw CatalaError.error(CatalaError.Error.UncomparableValues, p);
        }
    }

    @Override
    public String toString() {
        try {
            StringBuilder b = new StringBuilder();
            Enum k = getKind(this);
            CatalaValue v = getContents(this);
            b.append(k.toString());
            if (v != null && !(v instanceof CatalaUnit)) {
                if (CatalaGlobals.lang == CatalaGlobals.Language.FR) {
                    b.append(" contenu ");
                } else {
                    b.append(" content ");
                }
                b.append(v.toString());
            }
            return b.toString();
        } catch (IllegalAccessException | NoSuchFieldException e) {
            throw new RuntimeException(this.getClass().getName() + " is not a valid enum:\n" + e.toString());
        }
    }
}
