/* This is a template file following the expected interface and declarations to
 * implement the corresponding Catala module.
 *
 * You should replace all `Error.Impossible` place-holders with your
 * implementation and rename it to remove the ".template" suffix. */

import catala.runtime.*;
import catala.runtime.exception.*;
import java.util.TreeMap;

public class Dictionnary_ext {

    public static class Dictionnary
        implements CatalaValue {
        TreeMap<CatalaInteger,CatalaMoney> map;

        public Dictionnary() {
            map = new TreeMap<>();
        }

        @Override
        public CatalaBool equalsTo(CatalaValue other) {
          if (other instanceof Dictionnary v) {
              return CatalaBool.TRUE; // TODO
          } else { return CatalaBool.FALSE; }
        }
        @Override
        public String toString() {
            return "MAP"; // TODO
        }
    }

    public static class Globals {

        public static final Dictionnary empty = new Dictionnary();

        public static final CatalaFunction<CatalaTuple,Dictionnary> store =
            tup_arg_26 -> {
            Dictionnary dict = CatalaValue.<Dictionnary>cast
               (tup_arg_26.get(0));
            CatalaInteger key = CatalaValue.<CatalaInteger>cast
               (tup_arg_26.get(1));
            CatalaMoney value = CatalaValue.<CatalaMoney>cast
               (tup_arg_26.get(2));
            final Dictionnary store__1 = new Dictionnary();
            store__1.map = new TreeMap<>(dict.map);
            store__1.map.put(key, value);
            return store__1;
        };

        public static final CatalaFunction<CatalaTuple,CatalaOption<CatalaMoney>> find =
            tup_arg_27 -> {
            Dictionnary dict = CatalaValue.<Dictionnary>cast
               (tup_arg_27.get(0));
            CatalaInteger key = CatalaValue.<CatalaInteger>cast
               (tup_arg_27.get(1));
            CatalaMoney v = dict.map.get(key);
            if (v == null) return CatalaOption.NONE;
            else return CatalaOption.some(v);
        };
    }
}
