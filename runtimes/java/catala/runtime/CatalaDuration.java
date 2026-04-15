package catala.runtime;

import catala.dates_calc.Period;
import catala.runtime.exception.CatalaError;

public final class CatalaDuration extends CatalaValue<CatalaDuration> {

    public final Period period;

    public CatalaDuration(Period period) {
        this.period = period;
    }

    public CatalaDuration(int years, int months, int days) {
        this.period = new Period(years, months, days);
    }

    public boolean isZero() {
        return this.period.years == 0
                && this.period.months == 0
                && this.period.days == 0;
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

    public static final CatalaDuration of(int years, int months, int days) {
        return new CatalaDuration(years, months, days);
    }

    public static final CatalaDuration ofYears(int years) {
        return new CatalaDuration(years, 0, 0);
    }

    public static final CatalaDuration ofMonths(int months) {
        return new CatalaDuration(0, months, 0);
    }

    public static final CatalaDuration ofDays(int days) {
        return new CatalaDuration(0, 0, days);
    }

    public final Period asPeriod() {
        return this.period;
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

    @Override
    public int compareTo(CatalaPosition p, CatalaDuration o) {
        if (this.getMonths() == 0 && o.getMonths() == 0 && this.getYears() == 0 && o.getYears() == 0) {
            return ((Integer) (this.getDays())).compareTo(o.getDays());
        } else if (this.getDays() == 0 && o.getDays() == 0) {
            return ((Integer) (this.getYears() * 12 + this.getMonths()))
                    .compareTo(o.getYears() * 12 + o.getMonths());
        } else {
            throw CatalaError.error(CatalaError.Error.DateError, p,
                    "ambiguous date computation with no rounding mode specified");
        }
    }

    @Override
    public CatalaBool equalsTo(CatalaPosition pos, CatalaDuration other) {
        return (CatalaBool.of(this.getYears() == other.getYears()
                && this.getMonths() == other.getMonths()
                && this.getDays() == other.getDays()
                || this.compareTo(pos, other) == 0));
    }

    private static boolean appendDuration(StringBuilder b, String frlbl, String enlbl,
            int value, boolean always, boolean hasPred) {
        if (value > 0 || always) {
            if (hasPred) {
                b.append(", ");
            }
            if (CatalaGlobals.lang == CatalaGlobals.Language.FR) {
                b.append(value).append(" ").append(frlbl);
            } else {
                b.append(value).append(" ").append(enlbl);
            }
            if (value > 1) {
                b.append("s");
            }
            return true;
        } else {
            return false;
        }
    }

    @Override
    public String toString() {
        StringBuilder b = new StringBuilder();
        b.append('[');
        boolean hasPred = false;
        if (this.isZero()) {
            appendDuration(b, "jour", "day", 0, true, hasPred);
        } else {
            hasPred = appendDuration(b, "an", "year", this.period.years, false, hasPred);
            hasPred = appendDuration(b, "mois", "month", this.period.months, false, hasPred);
            appendDuration(b, "jour", "day", this.period.days, false, hasPred);
        }
        return b.toString().trim() + ']';
    }

    @Override
    public String toJSONString() {
        if (this.isZero()) {
            return "{}";
        }
        StringBuilder b = new StringBuilder();
        b.append("{ ");
        boolean pred = false;
        if (this.period.years > 0) {
            b.append("\"years\":").append(this.period.years);
            pred = true;
        }
        if (this.period.months > 0) {
            if (pred) {
                b.append(", ");
            }
            pred = true;
            b.append("\"months\":").append(this.period.months);
        }
        if (this.period.days > 0) {
            if (pred) {
                b.append(", ");
            }
            b.append("\"days\":").append(this.period.days);
        }
        b.append(" }");
        return b.toString();
    }
}
