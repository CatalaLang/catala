package catala.runtime;

import java.time.Period;

import catala.runtime.exception.CatalaException;

public final class CatalaDuration implements CatalaValue {

    // 'Period' in java.time maps to catala Durations
    // (java.time.Period is a datetime version)
    private final Period duration;

    public CatalaDuration(Period period) {
        this.duration = period;
    }

    public static final CatalaDuration of(int years, int months, int days) {
        return new CatalaDuration(Period.of(years, months, days));
    }

    public final int getYears() {
        return this.duration.getYears();
    }

    public final int getMonths() {
        return this.duration.getMonths();
    }

    public final int getDays() {
        return this.duration.getDays();
    }

    public final CatalaDuration add(CatalaDuration other) {
        return new CatalaDuration(this.duration.plus(other.duration));
    }

    public final CatalaDuration subtract(CatalaDuration other) {
        return new CatalaDuration(this.duration.minus(other.duration));
    }

    public final CatalaDuration negate() {
        return new CatalaDuration(this.duration.negated());
    }

    // Mult_dur_int
    public final CatalaDuration multiply(CatalaInteger other) {
        // FIXME
        int i = other.asBigInteger().intValue();
        return new CatalaDuration(this.duration.multipliedBy(i));
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
    public int compareTo(SourcePosition pos, CatalaDuration t) {
        if (this.getYears() != 0 || t.getYears() != 0) {
            if (this.getMonths() != 0 || t.getMonths() != 0 && this.getDays() != 0 || t.getDays() != 0) {
                throw new CatalaException("comparing durations in different units (e.g. months vs. days): "
                        + pos);
            }
            return ((Integer) (this.getYears())).compareTo(t.getYears());
        }
        if (this.getMonths() != 0 || t.getMonths() != 0) {
            if (this.getYears() != 0 || t.getYears() != 0 && this.getDays() != 0 || t.getDays() != 0) {
                throw new CatalaException("comparing durations in different units (e.g. months vs. days): "
                        + pos);
            }
            return ((Integer) (this.getMonths())).compareTo(t.getMonths());
        }

        return ((Integer) (this.getDays())).compareTo(t.getDays());
    }

    public CatalaBool lessThan(SourcePosition pos, CatalaDuration other) {
        return CatalaBool.fromBoolean(this.compareTo(pos, other) < 0);
    }

    public CatalaBool lessEqThan(SourcePosition pos, CatalaDuration other) {
        return CatalaBool.fromBoolean(this.compareTo(pos, other) <= 0);
    }

    public CatalaBool greaterThan(SourcePosition pos, CatalaDuration other) {
        return CatalaBool.fromBoolean(this.compareTo(pos, other) > 0);
    }

    public CatalaBool greaterEqThan(SourcePosition pos, CatalaDuration other) {
        return CatalaBool.fromBoolean(this.compareTo(pos, other) >= 0);
    }

    public CatalaBool equalsTo(SourcePosition pos, CatalaDuration other) {
        return CatalaBool.fromBoolean(this.compareTo(pos, other) == 0);
    }

}
