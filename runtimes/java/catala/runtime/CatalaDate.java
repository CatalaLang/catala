package catala.runtime;

import catala.dates_calc.AmbiguousComputationException;
import catala.dates_calc.Date;
import catala.dates_calc.Date.Rounding;
import catala.runtime.exception.CatalaError;

public final class CatalaDate extends CatalaValue<CatalaDate> {

    public final Date date;

    public CatalaDate(Date date) {
        this.date = date;
    }

    public static CatalaDate of(int year, int month, int day) {
        try {
            return new CatalaDate(Date.of(year, month, day));
        } catch (IllegalArgumentException e) {
            throw CatalaError.error(CatalaError.Error.DateError,
                    String.format("|%04d-%02d-%02d| is not a valid date", year, month, day));
        }
    }

    /**
     * Parses a date in the format '|YYYY-MM-DD|'
     */
    public static final CatalaDate parse(CharSequence catalaDateLiteral) {
        if (catalaDateLiteral.length() != 12
                || catalaDateLiteral.charAt(0) != '|'
                || catalaDateLiteral.charAt(11) != '|') {
            throw (CatalaError.error(CatalaError.Error.DateError,
                    String.format("'%s' is not a valid date", catalaDateLiteral)));
        }
        try {
            return new CatalaDate(Date.fromString(catalaDateLiteral.subSequence(1, 11).toString()));
        } catch (IllegalArgumentException e) {
            throw (CatalaError.error(CatalaError.Error.DateError,
                    String.format("'%s' is not a valid date", catalaDateLiteral)));
        }
    }

    public final CatalaInteger getYear() {
        return new CatalaInteger(this.date.year);
    }

    public final CatalaInteger getMonth() {
        return new CatalaInteger(this.date.month);
    }

    public final CatalaInteger getDay() {
        return new CatalaInteger(this.date.day);
    }

    public final CatalaDate getFirstDayOfMonth() {
        return new CatalaDate(this.date.firstDayOfMonth());
    }

    public final CatalaDate getLastDayOfMonth() {
        return new CatalaDate(this.date.lastDayOfMonth());
    }

    public CatalaDate addDurationAbortOnRound(CatalaPosition pos, CatalaDuration dur) {
        try {
            return new CatalaDate(this.date.add(dur.period));
        } catch (AmbiguousComputationException e) {
            throw CatalaError.error(CatalaError.Error.DateError, pos, "ambiguous date computation with no rounding mode specified");
        }
    }

    public CatalaDate addDurationRoundUp(CatalaPosition pos, CatalaDuration dur) {
        return new CatalaDate(this.date.add(dur.period, Rounding.ROUND_UP));
    }

    public CatalaDate addDurationRoundDown(CatalaPosition pos, CatalaDuration dur) {
        return new CatalaDate(this.date.add(dur.period, Rounding.ROUND_DOWN));
    }

    public CatalaDate subDurationAbortOnRound(CatalaPosition pos, CatalaDuration dur) {
        try {
            return new CatalaDate(this.date.add(dur.period.negate()));
        } catch (AmbiguousComputationException e) {
            throw CatalaError.error(CatalaError.Error.DateError, pos, "ambiguous date computation with no rounding mode specified");
        }
    }

    public CatalaDate subDurationRoundUp(CatalaPosition pos, CatalaDuration dur) {
        return new CatalaDate(this.date.add(dur.period.negate(), Rounding.ROUND_UP));
    }

    public CatalaDate subDurationRoundDown(CatalaPosition pos, CatalaDuration dur) {
        return new CatalaDate(this.date.add(dur.period.negate(), Rounding.ROUND_DOWN));
    }

    public CatalaDuration subtract(CatalaDate date) {
        return new CatalaDuration(this.date.sub(date.date));
    }

    @Override
    public int compareTo(CatalaPosition p, CatalaDate o) {
        return this.date.compareTo(o.date);
    }

    @Override
    public CatalaBool equalsTo(CatalaPosition p, CatalaDate o) {
        return CatalaBool.of(this.date.equals(o.date));
    }

    @Override
    public String toString() {
        return String.format("%1$04d-%2$02d-%3$02d", this.date.year,
                this.date.month, this.date.day);
    }

    @Override
    public String toJSONString() {
        return '"' + this.toString() + '"';
    }
}
