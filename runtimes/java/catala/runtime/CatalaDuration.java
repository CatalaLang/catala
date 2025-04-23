package catala.runtime;

import catala.dates_calc.Period;
import catala.runtime.exception.CatalaError;

public final class CatalaDuration implements CatalaValue {

    Period period;

    public CatalaDuration(Period period) {
        this.period = period;
    }

    public static final CatalaDuration of(int years, int months, int days) {
        return new CatalaDuration(new Period(years, months, days));
    }

    public final int getYears() {
        return this.period.years;
    }

    public final int getMonths() {
        return this.period.months;
    }

    public final int getDays() {
        return this.period.days;
    }

    public final CatalaDuration add(CatalaDuration other) {
        return new CatalaDuration(this.period.add(other.period));
    }

    public final CatalaDuration subtract(CatalaDuration other) {
        return new CatalaDuration(this.period.add(other.period.negate()));
    }

    public final CatalaDuration negate() {
        return new CatalaDuration(this.period.negate());
    }

    // Mult_dur_int
    public final CatalaDuration multiply(CatalaInteger other) {
        return new CatalaDuration(this.period.mul(other.asBigInteger().intValue()));
    }

    public final CatalaDecimal divide(CatalaDuration other) {
        if (this.getYears() != 0 || this.getMonths() != 0
                || other.getYears() != 0 || other.getMonths() != 0) {
            throw new IllegalArgumentException("Can only divide durations expressed in days");
        }
        if (other.getDays() == 0) {
            throw new IllegalArgumentException("Duration: divide by zero");
        }
        return new CatalaDecimal(this.getDays(), other.getDays());

    }

    // No override
    public int compareTo(CatalaPosition pos, CatalaDuration t) {
        if (this.getYears() != 0 || t.getYears() != 0) {
            if (this.getMonths() != 0 || t.getMonths() != 0 && this.getDays() != 0 || t.getDays() != 0) {
                throw new CatalaError(CatalaError.Error.UncomparableDurations, pos);
            }
            return ((Integer) (this.getYears())).compareTo(t.getYears());
        }
        if (this.getMonths() != 0 || t.getMonths() != 0) {
            if (this.getYears() != 0 || t.getYears() != 0 && this.getDays() != 0 || t.getDays() != 0) {
                throw new CatalaError(CatalaError.Error.UncomparableDurations, pos);
            }
            return ((Integer) (this.getMonths())).compareTo(t.getMonths());
        }

        return ((Integer) (this.getDays())).compareTo(t.getDays());
    }

    public CatalaBool lessThan(CatalaPosition pos, CatalaDuration other) {
        return CatalaBool.fromBoolean(this.compareTo(pos, other) < 0);
    }

    public CatalaBool lessEqThan(CatalaPosition pos, CatalaDuration other) {
        return CatalaBool.fromBoolean(this.compareTo(pos, other) <= 0);
    }

    public CatalaBool greaterThan(CatalaPosition pos, CatalaDuration other) {
        return CatalaBool.fromBoolean(this.compareTo(pos, other) > 0);
    }

    public CatalaBool greaterEqThan(CatalaPosition pos, CatalaDuration other) {
        return CatalaBool.fromBoolean(this.compareTo(pos, other) >= 0);
    }

    public CatalaBool equalsTo(CatalaPosition pos, CatalaDuration other) {
        return CatalaBool.fromBoolean(this.compareTo(pos, other) == 0);
    }

    @Override
    public CatalaBool equalsTo(CatalaValue v) {
        if (v instanceof CatalaDuration catalaDuration) {
            return CatalaBool.fromBoolean(this.period.equals(catalaDuration.period));
        } else {
            return CatalaBool.FALSE;
        }
    }

    @Override
    public String toString() {
        int years = this.getYears();
        if (years > 0) {
            return "[" + years + (years == 1 ? " year]" : " years]");
        }
        int months = this.getMonths();
        if (months > 0) {
            return "[" + months + (months == 1 ? " month]" : " months]");
        }
        int days = this.getDays();
        return "[" + days + (days == 1 ? " day]" : " days]");
    }
}
