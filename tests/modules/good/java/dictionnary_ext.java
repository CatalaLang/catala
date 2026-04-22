import catala.runtime.*;
import java.util.Map;
import java.util.TreeMap;

public class Dictionnary_ext {

    public static class Dictionnary
        extends CatalaValue<Dictionnary> {
        TreeMap<CatalaInteger,CatalaMoney> map;

        public Dictionnary() {
            map = new TreeMap<>();
        }

        @Override
        public CatalaBool equalsTo(CatalaPosition p, Dictionnary other) {
           return CatalaBool.of(this.map.equals(other.map));
        }

        @Override
        public int compareTo(CatalaPosition p, Dictionnary o) {
            return 0;
        }

        @Override
        public String toString() {
            return "MAP"; // TODO
        }

        @Override
        public String toJSONString() {
            return "\"<Dictionnary>\"";
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
            if (v == null) return CatalaOption.none();
            else return CatalaOption.some(v);
        };
    }
}
