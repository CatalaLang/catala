package catala.runtime;

import catala.runtime.exception.CatalaError;
import java.lang.reflect.Field;

public class CatalaEnum extends CatalaValue<CatalaEnum> {

    private Enum<?> getKind() throws IllegalAccessException, NoSuchFieldException {
        Field f = this.getClass().getDeclaredField("kind");
        f.setAccessible(true);
        return ((Enum) (f.get(this)));
    }

    private CatalaValue<?> getContents() throws IllegalAccessException, NoSuchFieldException {
        Field f = this.getClass().getDeclaredField("contents");
        f.setAccessible(true);
        return ((CatalaValue) (f.get(this)));
    }

    @Override
    public CatalaBool equalsTo(CatalaPosition p, CatalaEnum o) {
        if (this.getClass() != o.getClass()) {
            return CatalaBool.FALSE;
        }
        try {
            Enum<?> k1 = this.getKind();
            Enum<?> k2 = o.getKind();
            if (k1.ordinal() != k2.ordinal()) {
                return CatalaBool.FALSE;
            }
            return this.getContents().equalsTo(p, o.getContents());
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
            Enum<?> k1 = this.getKind();
            Enum<?> k2 = o.getKind();
            if (k1.getClass().equals(k2.getClass())) {
                if (k1.ordinal() != k2.ordinal()) {
                    return Integer.compare(k1.ordinal(), k2.ordinal());
                }
            }
            return this.getContents().compareTo(p, o.getContents());
        } catch (IllegalAccessException | NoSuchFieldException e) {
            throw CatalaError.error(CatalaError.Error.UncomparableValues, p);
        }
    }

    @Override
    public String toString() {
        try {
            StringBuilder b = new StringBuilder();
            Enum<?> k = this.getKind();
            CatalaValue<?> v = this.getContents();
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
            throw CatalaError.error(CatalaError.Error.GenericError, this.getClass().getName() + " is not a valid enum:\n" + e.toString());
        }
    }

    @Override
    public String toJSONString() {
        try {
            Enum<?> k = this.getKind();
            CatalaValue<?> v = this.getContents();
            StringBuilder b = new StringBuilder();
            boolean is_none = v != null && !(v instanceof CatalaUnit);
            if (is_none) {
                b.append("{ ");
            }
            b.append('"').append(k.name()).append('"');
            if (v != null && !(v instanceof CatalaUnit)) {
                b.append(": ").append(v.toJSONString());
            }
            if (is_none) {
                b.append(" }");
            }
            return b.toString();
        } catch (IllegalAccessException | NoSuchFieldException e) {
            throw CatalaError.error(CatalaError.Error.GenericError, this.getClass().getName() + " is not a valid enum:\n" + e.toString());
        }
    }
}
