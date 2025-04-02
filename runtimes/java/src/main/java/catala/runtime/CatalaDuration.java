package catala.runtime;

import java.time.Period;

public final class CatalaDuration implements CatalaValue {
    // 'Period' in java.time maps to catala Durations
    // (java.time.Period is a datetime version)
    private final Period duration;

    public CatalaDuration(Period period){
        this.duration = period;
    }

    public static final CatalaDuration of(int years, int months, int days){
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

    public final CatalaDuration plus(CatalaDuration other){
        return new CatalaDuration(this.duration.plus(other.duration));
    }

    public final CatalaDuration minus(CatalaDuration other){
        return new CatalaDuration(this.duration.minus(other.duration));
    }
}