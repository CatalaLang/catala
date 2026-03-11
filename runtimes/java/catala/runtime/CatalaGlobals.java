package catala.runtime;

import java.util.stream.Stream;

public class CatalaGlobals {

    public enum Language {
        EN, FR
    }

    public static Language lang = Language.EN;

    public static void displayResult(String[] args, String scope, CatalaValue<?> result) {
        Stream<String> args_s = Stream.of(args);
        boolean is_test = args_s.anyMatch(s -> s.equals("--test"));
        // Poor man's isatty...
        if (System.console() == null) {
            System.out.println("[RESULT] Scope TEnum executed successfully.");
        } else {
            System.out.println("\u001B[32m[RESULT]\u001B[0m Scope TEnum executed successfully.");
        }
        if (!is_test) {
            System.out.println(result);
        }

    }
}
