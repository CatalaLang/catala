package catala.runtime;

import catala.runtime.exception.CatalaError;
import java.lang.reflect.Field;

public class CatalaStruct extends CatalaValue<CatalaStruct> {

    @Override
    public CatalaBool equalsTo(CatalaPosition p, CatalaStruct o) {
        if (!this.getClass().isInstance(o)) {
            return CatalaBool.FALSE;
        }
        Field[] fields = this.getClass().getDeclaredFields();
        for (Field field : fields) {
            try {
                field.setAccessible(true);
                CatalaValue<?> v1 = (CatalaValue) (field.get(this));
                CatalaValue<?> v2 = (CatalaValue) (field.get(o));
                if (!(v1.equalsTo(p, v2)).asBoolean()) {
                    return CatalaBool.FALSE;
                }
            } catch (IllegalAccessException | IllegalArgumentException e) {
                throw CatalaError.error(CatalaError.Error.UncomparableValues, p);
            }
        }
        return CatalaBool.TRUE;
    }

    @Override
    public int compareTo(CatalaPosition p, CatalaStruct o) {
        if (!this.getClass().isInstance(o)) {
            throw CatalaError.error(CatalaError.Error.UncomparableValues, p);
        }
        int cmp;
        Field[] fields = this.getClass().getDeclaredFields();
        for (Field field : fields) {
            try {
                field.setAccessible(true);
                CatalaValue<?> v1 = (CatalaValue) (field.get(this));
                CatalaValue<?> v2 = (CatalaValue) (field.get(o));
                if ((cmp = v1.compareTo(p, v2)) != 0) {
                    return cmp;
                }
            } catch (IllegalAccessException | IllegalArgumentException e) {
                throw CatalaError.error(CatalaError.Error.UncomparableValues, p);
            }
        }
        return 0;
    }

    public String toString(String qualified_name) {
        Field[] fields = this.getClass().getDeclaredFields();
        if (fields.length == 0) {
            return qualified_name + " { }";
        }
        StringBuilder b = new StringBuilder();
        b.append(qualified_name).append(" {\n");
        StringBuilder subb = new StringBuilder();
        for (int i = 0; i < fields.length; i++) {
            Field f = fields[i];
            f.setAccessible(true);
            Class<?> c = f.getType();
            subb.append("-- ").append(f.getName()).append(": ");
            if (c.isAssignableFrom(CatalaFunction.class)) {
                subb.append("<function>");
                continue;
            }
            try {
                if (CatalaStruct.class.isAssignableFrom(c)) {
                    subb.append(((CatalaStruct) (f.get(this))).toString(c.getCanonicalName()));
                } else {
                    subb.append(f.get(this).toString());
                }
            } catch (IllegalAccessException | IllegalArgumentException e) {
                throw CatalaError.error(CatalaError.Error.GenericError, "failed to introspect value of field " + f.getName());
            }
            if (i < fields.length - 1) {
                subb.append('\n');
            }
        }
        // indent adds a newline for no intelligible reason: we do not add a new one.
        b.append(subb.toString().indent(2)).append("}");
        return b.toString();
    }

    @Override
    public String toString() {
        return toString(this.getClass().getSimpleName());
    }

    @Override
    public String toJSONString() {
        Field[] fields = this.getClass().getDeclaredFields();
        if (fields.length == 0) {
            return "{ }";
        }
        StringBuilder b = new StringBuilder();
        b.append('{');
        if (fields.length > 1) {
            b.append('\n');
        } else {
            b.append(' ');
        }
        StringBuilder subb = new StringBuilder();
        for (int i = 0; i < fields.length; i++) {
            Field f = fields[i];
            try {
                f.setAccessible(true);
                Class<?> c = f.getType();
                if (c.isAssignableFrom(CatalaFunction.class)) {
                    throw CatalaError.error(CatalaError.Error.GenericError, "Cannot serialize functional value of field " + f.getName());
                }
                CatalaValue<?> v = (CatalaValue<?>) (f.get(this));
                if (c.isAssignableFrom(CatalaOption.class)) {
                    CatalaOption<?> o = (CatalaOption) f.get(this);
                    if (o.isNone()) {
                        // Print nothing if the field is an unset optional
                        continue;
                    } else {
                        subb.append('"').append(f.getName()).append("\": ").append(o.get().toJSONString());
                    }
                } else {
                    subb.append('"').append(f.getName()).append("\": ").append(v.toJSONString());
                }
                if (i < fields.length - 1) {
                    subb.append(",\n");
                }
            } catch (IllegalAccessException | IllegalArgumentException | ClassCastException e) {
                throw CatalaError.error(CatalaError.Error.GenericError, "failed to introspect value of field " + f.getName());
            }
        }
        if (fields.length > 1) {
            // indent adds a newline for no intelligible reason: we do not add a new one.
            b.append(subb.toString().indent(2)).append("}");
        } else {
            b.append(subb.toString()).append(" }");
        }
        return b.toString();

    }
}
