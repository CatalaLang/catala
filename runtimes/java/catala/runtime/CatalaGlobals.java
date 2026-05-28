package catala.runtime;

import catala.runtime.exception.CatalaError;
import java.util.stream.Stream;

public class CatalaGlobals {

    public enum Language {
        EN, FR
    }

    public static Language lang = Language.EN;

    public static CatalaValue<?> fromJSONString(CatalaPosition p, String json){
        throw CatalaError.error(CatalaError.Error.NotImplemented, p);
    }

    public static void displayResult(String scope, CatalaValue<?> result, boolean test_mode, boolean json_mode) {
        System.err.println("\u001B[32m[RESULT]\u001B[0m Scope " + scope + " executed successfully.");
        if (!test_mode) {
            if (json_mode) {
                System.out.println(result.toJSONString());
            } else {
                System.out.println(result.toString());
            }
        }
    }

    private static final String ERR_PREFIX = "\033[1;31m[ERROR]\033[m ";

    public static void displayError(String scope, RuntimeException e) {
        if (e instanceof CatalaError ce) {
            System.err.println(ERR_PREFIX + "While executing scope " + scope + " " + ce.getMessage());
            if (ce.kind == CatalaError.Error.GenericError) {
                e.printStackTrace(System.err);
            }
        } else {
            System.err.println("\033[1;31m[ERROR]\033[m Unexpected exception");
            throw e;
        }
    }

}
