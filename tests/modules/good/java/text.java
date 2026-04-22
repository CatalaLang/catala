import catala.runtime.*;

public class Text {

    public static class Globals {

        public static final CatalaArray<CatalaPosition> loc
                = new CatalaArray<>(new CatalaPosition("tests/modules/good/text.catala_en", 8, 13, 8, 16,
                        new String[]{"Text types in Catala"}),
                        new CatalaPosition("tests/modules/good/text.catala_en", 9, 13, 9, 16,
                                new String[]{"Text types in Catala"}),
                        new CatalaPosition("tests/modules/good/text.catala_en", 10, 13, 10, 21,
                                new String[]{"Text types in Catala"}),
                        new CatalaPosition("tests/modules/good/text.catala_en", 11, 13, 11, 19,
                                new String[]{"Text types in Catala"}));

        static final CatalaFunction<CatalaUnit, Text__1> fooInit = unit -> {
            return new Text__1("foo\\");
        };

        public static final Text__1 foo = fooInit.apply(CatalaUnit.INSTANCE);

        static final CatalaFunction<CatalaUnit, Text__1> barInit = unit -> {
            return new Text__1("bąr");
        };

        public static final Text__1 bar = barInit.apply(CatalaUnit.INSTANCE);

        static final CatalaFunction<CatalaUnit, Text__1> fortytwoInit = unit -> {
            return new Text__1("42");
        };

        public static final Text__1 fortytwo = fortytwoInit.apply(CatalaUnit.INSTANCE);

        public static final CatalaFunction<CatalaInteger, Text__1> ofInt = x -> {
            return new Text__1(x.asBigInteger().toString());
        };
    }

    public static class Text__1 extends CatalaValue<Text__1> {

        public String s;

        public Text__1(String s) {
            this.s = s;
        }

        @Override
        public CatalaBool equalsTo(CatalaPosition p, Text__1 o) {
            return CatalaBool.of(this.s.equals(o.s));
        }

        @Override
        public int compareTo(CatalaPosition p, Text__1 o) {
            return this.s.compareTo(o.s);
        }

        @Override
        public String toString() {
            return this.toJSONString();
        }

        @Override
        public String toJSONString() {
            return '"' + s + '"';
        }

        public static Text__1 fromJSONString(CatalaPosition p, String json) {
            // FIXME
            String unescaped = json.substring(1, json.length() - 1).replaceAll("\\\\\\\\", "\\\\");
            return new Text__1(unescaped);
        }
    }

}
