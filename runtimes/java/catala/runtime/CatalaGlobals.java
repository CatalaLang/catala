package catala.runtime;

import catala.runtime.exception.CatalaError;
import java.text.NumberFormat;
import java.text.DecimalFormat;
import java.math.RoundingMode;
import java.util.Locale;

public class CatalaGlobals {

    public enum Language {
        EN, FR, PL
    }

    public static Language lang = Language.EN;

    public static Integer max_decimals = 6;

    public static Boolean tracing = false;

    public static NumberFormat number_format() {
        NumberFormat nf;
        if (CatalaGlobals.lang == CatalaGlobals.Language.FR) {
            return NumberFormat.getInstance(Locale.FRENCH);
        } else if (CatalaGlobals.lang == CatalaGlobals.Language.PL) {
            return NumberFormat.getInstance(Locale.forLanguageTag("pl-PL"));
        } else {
            return NumberFormat.getInstance(Locale.US);
        }
    }

    public static DecimalFormat decimal_format() {
        DecimalFormat df = (DecimalFormat) (number_format());
        df.setParseBigDecimal(true);
        df.setDecimalSeparatorAlwaysShown(true);
        df.setMinimumIntegerDigits(1);
        df.setMinimumFractionDigits(1);
        df.setMaximumFractionDigits(max_decimals);
        df.setRoundingMode(RoundingMode.DOWN);
        return df;
    }

    public static CatalaValue<?> fromJSONString(CatalaPosition p, String json) {
        throw CatalaError.error(CatalaError.Error.NotImplemented, p);
    }

    public static void displayResult(String scope, CatalaValue<?> result, boolean test_mode, boolean json_mode) {
        if (tracing) {
            System.out.println(CatalaTrace.retrieve().toJSONString());
        }
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
        if (tracing) {
            System.out.println(CatalaTrace.retrieve().toJSONString());
        }
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
