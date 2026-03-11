package catala.runtime;

import catala.runtime.exception.CatalaError;
import java.util.stream.Stream;

public class CatalaGlobals {

    public enum Language {
        EN, FR
    }

    public static Language lang = Language.EN;

    public static void displayResult(String[] args, String scope, CatalaValue<?> result) {
        Stream<String> args_s = Stream.of(args);
        boolean is_test = args_s.anyMatch(s -> s.equals("--test"));
        System.out.println("\u001B[32m[RESULT]\u001B[0m Scope " + scope + " executed successfully.");
        if (!is_test) {
            System.out.println(result);
        }
    }

    private static final String error_prefix = "\033[1;31m[ERROR]\033[m ";

    public static void displayError(String scope, RuntimeException e) {
        if (e instanceof CatalaError ce) {
            System.err.println(error_prefix + "While executing scope " + scope + " " + ce.getMessage()
            );
        } else {
            System.err.println("\033[1;31m[ERROR]\033[m Unexpected exception");
            throw e;
        }
    }

}
