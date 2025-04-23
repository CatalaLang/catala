package catala.runtime.exception;

import catala.runtime.CatalaBool;
import catala.runtime.CatalaPosition;

public class CatalaAssertion {

    public static void check(CatalaPosition pos, CatalaBool b) {
        if (!b.asBoolean()) {
            throw new CatalaError(CatalaError.Error.AssertionFailed, pos);
        }
    }
}
