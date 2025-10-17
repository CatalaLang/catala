package catala.runtime;

import catala.dates_calc.AmbiguousComputationException;
import catala.dates_calc.Date;
import catala.dates_calc.Date.Rounding;
import catala.runtime.exception.CatalaError;

public final class CatalaDate implements CatalaValue, Comparable<CatalaDate> {

    public final Date date;

    public CatalaDate(Date date) {
        this.date = date;
    }

    public static CatalaDate of(int year, int month, int day) {
        return new CatalaDate(Date.of(year, month, day));
    }

    /**
     * Parses a date in the format '|2024-03-12|'
     */
    public static final CatalaDate parse(CharSequence catalaDateLiteral) {
        if (catalaDateLiteral.length() != 12
                || catalaDateLiteral.charAt(0) != '|'
                || catalaDateLiteral.charAt(11) != '|') {
            throw new IllegalArgumentException("Expected Catala date literal");
        }
        return new CatalaDate(Date.fromString(catalaDateLiteral.subSequence(1, 11).toString()));
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

    @Override
    public int compareTo(CatalaDate t) {
        return this.date.compareTo(t.date);
    }

    public CatalaBool lessThan(CatalaDate other) {
        return CatalaBool.fromBoolean(this.compareTo(other) < 0);
    }

    public CatalaBool lessEqThan(CatalaDate other) {
        return CatalaBool.fromBoolean(this.compareTo(other) <= 0);
    }

    public CatalaBool greaterThan(CatalaDate other) {
        return CatalaBool.fromBoolean(this.compareTo(other) > 0);
    }

    public CatalaBool greaterEqThan(CatalaDate other) {
        return CatalaBool.fromBoolean(this.compareTo(other) >= 0);
    }

    public CatalaBool equalsTo(CatalaDate other) {
        return CatalaBool.fromBoolean(this.compareTo(other) == 0);
    }

    public CatalaDate addDurationAbortOnRound(CatalaPosition pos, CatalaDuration dur) {
        try {
            return new CatalaDate(this.date.add(dur.period));
        } catch (AmbiguousComputationException e) {
            throw new CatalaError(CatalaError.Error.AmbiguousDateRounding, pos);
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
            throw new CatalaError(CatalaError.Error.AmbiguousDateRounding, pos);
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
    public CatalaBool equalsTo(CatalaValue v) {
        if (v instanceof CatalaDate catalaDate) {
            return CatalaBool.fromBoolean(this.compareTo(catalaDate) == 0);
        } else {
            return CatalaBool.FALSE;
        }
    }

    @Override
    public String toString() {
        return String.format("|%1$04d-%2$02d-%3$02d|", this.date.year,
                this.date.month, this.date.day);
    }
}
