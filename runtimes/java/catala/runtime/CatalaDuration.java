package catala.runtime;

import catala.dates_calc.Period;
import catala.runtime.exception.CatalaError;

public final class CatalaDuration extends CatalaValue<CatalaDuration> {

    public final Period period;

    public CatalaDuration(Period period) {
        this.period = period;
    }

    public static final CatalaDuration of(int years, int months, int days) {
        return new CatalaDuration(new Period(years, months, days));
    }

    public boolean isZero() {
        return this.period.years == 0
                && this.period.months == 0
                && this.period.days == 0;
    }

    public final int years() {
        return this.period.years;
    }

    public final int months() {
        return this.period.months;
    }

    public final int days() {
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
        if (this.years() != 0 || this.months() != 0
                || other.years() != 0 || other.months() != 0) {
            throw new IllegalArgumentException("Can only divide durations expressed in days");
        }
        if (other.days() == 0) {
            throw new IllegalArgumentException("Duration: divide by zero");
        }
        return new CatalaDecimal(this.days(), other.days());

    }

    @Override
    public int compareTo(CatalaPosition p, CatalaDuration o) {
        if (this.months() == 0 && o.months() == 0 && this.years() == 0 && o.years() == 0) {
            return ((Integer) (this.days())).compareTo(o.days());
        } else if (this.days() == 0 && o.days() == 0) {
            return ((Integer) (this.years() * 12 + this.months()))
                    .compareTo(o.years() * 12 + o.months());
        } else {
            throw CatalaError.error
                (CatalaError.Error.DateError, p,
                 "ambiguous date computation with no rounding mode specified");
        }
    }

    @Override
    public CatalaBool equalsTo(CatalaPosition pos, CatalaDuration other) {
        return (CatalaBool.fromBoolean(this.years() == other.years()
                && this.months() == other.months()
                && this.days() == other.days()
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
