package dates_calc;

import java.util.Objects;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Purposefully not final (same rationale as Date)
 */
public class Period {

    public final int years;
    public final int months;
    public final int days;

    public Period(int years, int months, int days) {
        this.years = years;
        this.months = months;
        this.days = days;
    }

    public final Period negate() {
        return new Period(-this.years, -this.months, -this.days);
    }

    public final Period add(Period p) {
        return new Period(this.years + p.years,
                this.months + p.months,
                this.days + p.days);
    }

    public final Period mul(int m) {
        return new Period(this.years * m, this.months * m, this.days * m);
    }

    public final int toDays(){
        if(this.years != 0 || this.months != 0){
            throw new AmbiguousComputationException("Cannot convert a Period with non-zero years and/or months to days");
        }
        return this.days;
    }

    private static final Pattern PERIOD_PATTERN = Pattern.compile("\\[(-?\\d+) years, (-?\\d+) months, (-?\\d+) days\\]");

    public static Period fromString(String s) {
        Matcher matcher = PERIOD_PATTERN.matcher(s);
        if (matcher.matches()) {
            int years = Integer.parseInt(matcher.group(1));
            int months = Integer.parseInt(matcher.group(2));
            int days = Integer.parseInt(matcher.group(3));
            return new Period(years, months, days);
        } else {
            throw new IllegalArgumentException("Invalid period format: " + s);
        }
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Period period = (Period) o;
        return years == period.years && months == period.months && days == period.days;
    }

    @Override
    public int hashCode() {
        return Objects.hash(years, months, days);
    }

    @Override
    public String toString() {
        return String.format("[%d years, %d months, %d days]", years, months, days);
    }
}
