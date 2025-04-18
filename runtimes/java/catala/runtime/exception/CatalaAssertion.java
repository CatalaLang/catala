package catala.runtime.exception;

import catala.runtime.CatalaBool;
import catala.runtime.SourcePosition;

public class CatalaAssertion extends CatalaException {

    public CatalaAssertion(String message) {
        super(message);
    }

    public static void check(SourcePosition pos, CatalaBool b) {
        if (!b.asBoolean()) {
            throw new CatalaAssertion("Assertion failed: " + pos.toString());
        }
    }
}
