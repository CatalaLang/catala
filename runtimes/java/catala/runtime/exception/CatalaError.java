package catala.runtime.exception;

import java.util.List;
import java.util.stream.Collectors;

import catala.runtime.CatalaPosition;

public class CatalaError extends RuntimeException {

    public enum Error {
        AssertionFailed,
        NoValue,
        Conflict,
        DivisionByZero,
        ListEmpty,
        NotSameLength,
        UncomparableDurations,
        AmbiguousDateRounding,
        IndivisibleDurations;

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
                case UncomparableDurations:
                    return "ambiguous comparison between durations in different units (e.g. months vs. days)";
                case AmbiguousDateRounding:
                    return "ambiguous date computation, and rounding mode was not specified";
                case IndivisibleDurations:
                    return "dividing durations that are not in days";
            }
            return "";
        }
    }

    public CatalaError(Error err, CatalaPosition pos) {
        super("[ERROR] At " + pos.toString() + ": " + err.toString());
    }

    public CatalaError(Error err, List<CatalaPosition> lpos) {
        super("[ERROR] At " + lpos.stream()
                .map(CatalaPosition::toString)
                .collect(Collectors.joining(", ")) + ": " + err.toString());
    }
}
