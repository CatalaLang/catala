package catala.runtime.exception;

import java.util.List;
import java.util.stream.Collectors;

import catala.runtime.CatalaPosition;

public class CatalaError extends RuntimeException {

    private static final long serialVersionUID = 1L;

    public enum Error {
        AssertionFailed,
        NoValue,
        Conflict,
        DivisionByZero,
        ListEmpty,
        NotSameLength,
        InvalidDate,
        UncomparableDurations,
        AmbiguousDateRounding,
        IndivisibleDurations,
        Impossible,
        UncomparableValues;

        @Override
        public String toString() {
            switch (this) {
                case AssertionFailed:
                    return "an assertion doesn't hold";
                case NoValue:
                    return "no applicable rule to define this variable in this situation";
                case Conflict:
                    return "conflict between multiple valid consequences for assigning the same variable";
                case DivisionByZero:
                    return "a value is being used as denominator in a division and it computed to zero";
                case ListEmpty:
                    return "the list was empty";
                case NotSameLength:
                    return "traversing multiple lists of different lengths";
                case InvalidDate:
                    return "the provided numbers do not correspond to a valid date";
                case UncomparableDurations:
                    return "ambiguous comparison between durations in different units (e.g. months vs. days)";
                case AmbiguousDateRounding:
                    return "ambiguous date computation, and rounding mode was not specified";
                case IndivisibleDurations:
                    return "dividing durations that are not in days";
                case Impossible:
                    return "\"impossible\" computation reached";
                case UncomparableValues:
                    return "comparing values of different types";
            }
            return "";
        }
    }

    private CatalaError(Error err) {
        super("\n\033[1;31m[ERROR]\033[m" + err.toString());
    }

    public static CatalaError error(Error err) {
        return new CatalaError(err);
    }

    private CatalaError(Error err, String note) {
        super("\n\033[1;31m[ERROR]\033[m " + err.toString() + ". " + note);
    }

    public static CatalaError error(Error err, String note) {
        return new CatalaError(err, note);
    }

    private CatalaError(Error err, CatalaPosition pos) {
        super("\n\033[1;31m[ERROR]\033[m At " + pos.toString() + ": " + err.toString());
    }

    public static CatalaError error(Error err, CatalaPosition pos) {
        if (pos == null || pos == CatalaPosition.empty) {
            return new CatalaError(err);
        } else {
            return new CatalaError(err, pos);
        }
    }

    private CatalaError(Error err, CatalaPosition pos, String note) {
        super("\n\033[1;31m[ERROR]\033[m At " + pos.toString() + ": " + err.toString() + ". " + note);
    }

    public static CatalaError error(Error err, CatalaPosition pos, String note) {
        if (pos == null || pos == CatalaPosition.empty) {
            return new CatalaError(err, note);
        } else {
            return new CatalaError(err, pos, note);
        }
    }

    private CatalaError(Error err, List<CatalaPosition> lpos) {
        super("\n\033[1;31m[ERROR]\033[m At " + lpos.stream()
                .map(CatalaPosition::toString)
                .collect(Collectors.joining(", ")) + ": " + err.toString());
    }

    public static CatalaError error(Error err, List<CatalaPosition> lpos) {
        return new CatalaError(err, lpos);
    }
}
