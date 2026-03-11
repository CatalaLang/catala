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
                    return "Assertion failure";
                case NoValue:
                    return "No applicable rule to define this variable in this situation";
                case Conflict:
                    return "Conflict between multiple valid consequences for assigning the same variable";
                case DivisionByZero:
                    return "A value is being used as denominator in a division and it computed to zero";
                case ListEmpty:
                    return "The list was empty";
                case NotSameLength:
                    return "Traversing multiple lists of different lengths";
                case InvalidDate:
                    return "The provided numbers do not correspond to a valid date";
                case UncomparableDurations:
                    return "Ambiguous comparison between durations in different units (e.g. months vs. days)";
                case AmbiguousDateRounding:
                    return "Ambiguous date computation, and rounding mode was not specified";
                case IndivisibleDurations:
                    return "Dividing durations that are not in days";
                case Impossible:
                    return "\"impossible\" computation reached";
                case UncomparableValues:
                    return "Comparing values of different types";
            }
            return "";
        }
    }
 
    private CatalaError(Error err) {
        super(err.toString());
    }

    public static CatalaError error(Error err) {
        return new CatalaError(err);
    }

    private CatalaError(Error err, String note) {
        super(err.toString() + ". " + note);
    }

    public static CatalaError error(Error err, String note) {
        return new CatalaError(err, note);
    }

    private CatalaError(Error err, CatalaPosition pos) {
        super("at " + pos.toString() + ": " + err.toString());
    }

    public static CatalaError error(Error err, CatalaPosition pos) {
        if (pos == null || pos == CatalaPosition.empty) {
            return new CatalaError(err);
        } else {
            return new CatalaError(err, pos);
        }
    }

    private CatalaError(Error err, CatalaPosition pos, String note) {
        super("at " + pos.toString() + ": " + err.toString() + ". " + note);
    }

    public static CatalaError error(Error err, CatalaPosition pos, String note) {
        if (pos == null || pos == CatalaPosition.empty) {
            return new CatalaError(err, note);
        } else {
            return new CatalaError(err, pos, note);
        }
    }

    private CatalaError(Error err, List<CatalaPosition> lpos) {
        super("at " + lpos.stream()
                .map(CatalaPosition::toString)
                .collect(Collectors.joining(", ")) + ": " + err.toString());
    }

    public static CatalaError error(Error err, List<CatalaPosition> lpos) {
        return new CatalaError(err, lpos);
    }
}
