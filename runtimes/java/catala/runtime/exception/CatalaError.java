package catala.runtime.exception;

import catala.runtime.CatalaGlobals;
import catala.runtime.CatalaPosition;
import catala.runtime.CatalaTrace;
import java.util.List;
import java.util.stream.Collectors;

public class CatalaError extends RuntimeException {

    private static final long serialVersionUID = 1L;

    public enum Error {
        AssertionFailed,
        NoValue,
        Conflict,
        DivisionByZero,
        ListEmpty,
        NotSameLength,
        UncomparableValues,
        DateError,
        Impossible,
        GenericError,
        NotImplemented;

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
                case UncomparableValues:
                    return "attempting to compare values with uncomparable types";
                case DateError:
                    return "date error";
                case Impossible:
                    return "\"impossible\" computation reached";
                case GenericError:
                    return "Generic error";
                case NotImplemented:
                    return "Not implemented";
            }
            return "";
        }
    }

    public Error kind;

    private CatalaError(Error err) {
        super(err.toString());
        this.kind = err;
    }

    public static CatalaError error(Error err) {
        if (CatalaGlobals.tracing) {
            CatalaTrace.error(err, CatalaPosition.empty);
        }
        return new CatalaError(err);
    }

    private CatalaError(Error err, String note) {
        super(err.toString() + ". " + note);
        this.kind = err;
    }

    public static CatalaError error(Error err, String note) {
        if (CatalaGlobals.tracing) {
            CatalaTrace.error(err, null, null, note);
        }
        return new CatalaError(err, note);
    }

    private CatalaError(Error err, CatalaPosition pos) {
        super("at " + pos.toString() + ": " + err.toString());
        this.kind = err;
    }

    public static CatalaError error(Error err, CatalaPosition pos) {
        if (CatalaGlobals.tracing) {
            CatalaTrace.error(err, pos);
        }
        if (pos == null || pos == CatalaPosition.empty) {
            return new CatalaError(err);
        } else {
            return new CatalaError(err, pos);
        }
    }

    private CatalaError(Error err, CatalaPosition pos, String note) {
        super("at " + pos.toString() + ": " + err.toString() + ". " + note);
        this.kind = err;
    }

    public static CatalaError error(Error err, CatalaPosition pos, String note) {
        if (CatalaGlobals.tracing) {
            CatalaTrace.error(err, pos, null, note);
        }
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
        this.kind = err;
    }

    public static CatalaError error(Error err, List<CatalaPosition> lpos) {
        if (CatalaGlobals.tracing) {
            CatalaTrace.error(err, lpos);
        }
        return new CatalaError(err, lpos);
    }
}
