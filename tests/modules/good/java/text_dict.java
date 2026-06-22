import catala.runtime.*;
import catala.runtime.exception.*;
import catala.stdlib.*;
import java.util.Map;
import java.util.TreeMap;

public class Text_dict {

    public static class TextDict
        extends CatalaValue<TextDict> {
        TreeMap<Text.Text__1,Mod_def.ModDef> map;

        public TextDict() {
            map = new TreeMap<>();
        }

        @Override
        public CatalaBool equalsTo(CatalaPosition p, TextDict other) {
           return CatalaBool.of(this.map.equals(other.map));
        }

        @Override
        public int compareTo(CatalaPosition p, TextDict o) {
            return 0;
        }

        @Override
        public String toString() {
            return "MAP"; // TODO
        }

        @Override
        public String toJSONString() {
            return "\"<Text.Text__1_dict_1>\"";
        }
    }

    public static class Globals {

        public static final TextDict empty = new TextDict();

        public static final CatalaFunction<CatalaTuple,TextDict> store =
            tup_arg_26 -> {
            TextDict dict = CatalaValue.<TextDict>cast
               (tup_arg_26.get(0));
            Text.Text__1 key = CatalaValue.<Text.Text__1>cast
               (tup_arg_26.get(1));
            Mod_def.ModDef value = CatalaValue.<Mod_def.ModDef>cast
               (tup_arg_26.get(2));
            final TextDict store__1 = new TextDict();
            store__1.map = new TreeMap<>(dict.map);
            store__1.map.put(key, value);
            return store__1;
        };

        public static final CatalaFunction<CatalaTuple,CatalaOption<Mod_def.ModDef>> find =
            tup_arg_27 -> {
            TextDict dict = CatalaValue.<TextDict>cast
               (tup_arg_27.get(0));
            Text.Text__1 key = CatalaValue.<Text.Text__1>cast
               (tup_arg_27.get(1));
            Mod_def.ModDef v = dict.map.get(key);
            if (v == null) return CatalaOption.none();
            else return CatalaOption.some(v);
        };
    }
}
