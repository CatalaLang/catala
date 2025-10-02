package dates_calc;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Note that Date is purposefully non-final (we want to inherit from it in the
 * java catala runtime, in order to add a marker interface)
 */
public class Date implements Comparable<Date> {

    public enum Rounding {
        ROUND_UP, ROUND_DOWN, ABORT_ON_ROUND
    }

    public final int year;
    public final int month;
    public final int day;

    private Date(int year, int month, int day) {
        // We need to be able to privately create
        // invalid dates prior to rounding.
        // The user-facing API (`of`) will throw an error
        // if such a date is created, however

        this.year = year;
        this.month = month;
        this.day = day;
    }

    private static final Pattern DATE_PATTERN = Pattern.compile("(\\d{4})-(\\d{2})-(\\d{2})");

    public static Date fromString(String s) {
        Matcher matcher = DATE_PATTERN.matcher(s);
        if (matcher.matches()) {
            int year = Integer.parseInt(matcher.group(1));
            int month = Integer.parseInt(matcher.group(2));
            int day = Integer.parseInt(matcher.group(3));
            return Date.of(year, month, day);
        } else {
            throw new IllegalArgumentException("Invalid date format: " + s);
        }
    }


    public static final Date of(int year, int month, int day) {
        if (!isValid(year, month, day)) {
            throw new IllegalArgumentException("Invalid date");
        }

        return new Date(year, month, day);
    }

    public final Date firstDayOfMonth() {
        return Date.of(this.year, this.month, 1);
    }

    public final Date lastDayOfMonth() {
        return of(this.year, this.month, daysInMonth(this.month, isLeapYear(this.year)));
    }

    private boolean isValid() {
        return isValid(this.year, this.month, this.day);
    }

    private final Date round(Rounding mode) {
        if (this.isValid()) {
            return this;
        } else {
            switch (mode) {
                case ROUND_UP:
                    return nextValidDate();
                case ROUND_DOWN:
                    return prevValidDate();
                case ABORT_ON_ROUND:
                    throw new AmbiguousComputationException("Ambiguous date computation involving potential rounding");
            }
            throw new AssertionError("Unreachable code reached in Date.round");
        }
    }

    private static boolean isLeapYear(int year) {
        return (year % 400 == 0) || (year % 4 == 0 && year % 100 != 0);
    }

    private static int daysInMonth(int month, boolean isLeapYear) {
        if (month < 1 || month > 12) {
            throw new IllegalArgumentException("Invalid month number");
        }

        switch (month) {
            case 1:
            case 3:
            case 5:
            case 7:
            case 8:
            case 10:
            case 12:
                return 31;
            case 4:
            case 6:
            case 9:
            case 11:
                return 30;
            case 2:
                if (isLeapYear) {
                    return 29;
                } else {
                    return 28;
                }
        }

        throw new RuntimeException("This path should be unreachable, this is a bug");

    }

    private static boolean isValid(int year, int month, int day) {
        return day >= 1 && day <= daysInMonth(month, isLeapYear(year)) && month >= 1 && month <= 12;
    }

    private Date prevValidDate() {
        assert (this.month >= 1 && this.month <= 12);
        assert (this.day >= 1 && this.day <= 31);
        if (this.isValid()) {
            return this;
        } else {
            return of(this.year, this.month, daysInMonth(this.month, isLeapYear(this.year)));
        }

    }

    private int[] addMonthsToFirstOfMonthDate(int year, int month, int plusMonths) {
        assert (month >= 1 && month <= 12);

        int newMonth = month + plusMonths;
        if (newMonth >= 1 && newMonth <= 12) {
            return new int[]{year, newMonth};
        } else if (newMonth > 12) {
            return addMonthsToFirstOfMonthDate(year + 1, month, plusMonths - 12);
        } else {
            // newMonth <= 0
            return addMonthsToFirstOfMonthDate(year - 1, month, plusMonths + 12);
        }
    }

    private Date nextValidDate() {
        assert (this.month >= 1 && this.month <= 12);
        assert (this.day >= 1 && this.day <= 31);
        if (this.isValid()) {
            return this;
        } else {
            int[] yearAndMonth = addMonthsToFirstOfMonthDate(this.year, this.month, 1);
            return of(yearAndMonth[0], yearAndMonth[1], 1);
        }
    }

    /* This function is only ever called from `add_dates` below.
     Hence, any call to `add_dates_years` will be followed by a call
     to `add_dates_month`. We therefore perform a single rounding
     in `add_dates_month`, to avoid introducing additional imprecision here,
     and to ensure that adding n years + m months is always equivalent to
     adding (12n + m) months
     */
    private Date addYears(int years) {
        return new Date(this.year + years, this.month, this.day);
    }

    private Date addMonths(int months, Rounding rounding) {
        int[] newYearAndMonth = addMonthsToFirstOfMonthDate(this.year, this.month, months);
        return new Date(newYearAndMonth[0], newYearAndMonth[1], this.day).round(rounding);
    }

    private Date addDays(int days) {
        int daysInThisMonth = daysInMonth(this.month, isLeapYear(this.year));
        // note that 'days' is algebraic and may be negative
        int newDay = this.day + days;
        if (newDay >= 1 && newDay <= daysInThisMonth) {
            return of(this.year, this.month, newDay);
        } else if (newDay >= daysInThisMonth) {
            int[] newYearAndMonth = addMonthsToFirstOfMonthDate(this.year, this.month, 1);
            /* We warp to the first day of the next month!
            Now we compute how many days we still have left to add. Because we have
            warped to the next month, we already have added the rest of the days in
            the current month: [days_in_d_month - d.day]. But then we switch
            months, and that corresponds to adding another day.
             */
            return of(newYearAndMonth[0], newYearAndMonth[1], 1).addDays(days - (daysInThisMonth - this.day) - 1);
        } else {
            /* We warp to the first day of the next month!
            Now we compute how many days we still have left to add. Because we have
            warped to the next month, we already have added the rest of the days in
            the current month: [days_in_d_month - d.day]. But then we switch
            months, and that corresponds to adding another day.
             */
            int[] newYearAndMonth = addMonthsToFirstOfMonthDate(this.year, this.month, -1);
            return of(newYearAndMonth[0],
                    newYearAndMonth[1],
                    daysInMonth(newYearAndMonth[1], isLeapYear(newYearAndMonth[0]))).addDays(days + this.day);
        }
    }

    public final Date add(Period p, Rounding rounding){
      Date d = this.addYears(p.years);
      d = d.addMonths(p.months, rounding);
      d = d.addDays(p.days);
      return d;
    }

    public final Date add(Period p){
        return this.add(p, Rounding.ABORT_ON_ROUND);
    }

    public final Period sub(Date d){
        if(this.year == d.year && this.month == d.month){
          return new Period(0, 0, this.day - d.day);
        } else {
            if(this.compareTo(d) < 0){
              return d.sub(this).negate();
            }
            //at this point, we know that this >= d
            int[] newYearAndMonthD = addMonthsToFirstOfMonthDate(d.year, d.month, 1);
            Date newD = of(newYearAndMonthD[0], newYearAndMonthD[1], 1);
            return new Period(0, 0, daysInMonth(d.month, isLeapYear(d.year)) - d.day + 1)
              .add(this.sub(newD));

        }
    }

    @Override
    public int compareTo(Date other) {
        assert this.isValid() && other.isValid();

        int yearComparison = Integer.compare(this.year, other.year);
        if (yearComparison != 0) {
            return yearComparison;
        }
        int monthComparison = Integer.compare(this.month, other.month);
        if (monthComparison != 0) {
            return monthComparison;
        }
        return Integer.compare(this.day, other.day);
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (obj == null || getClass() != obj.getClass()) return false;
        Date date = (Date) obj;
        return year == date.year && month == date.month && day == date.day;
    }

    @Override
    public int hashCode() {
        int result = Integer.hashCode(year);
        result = 31 * result + Integer.hashCode(month);
        result = 31 * result + Integer.hashCode(day);
        return result;
    }

    @Override
    public String toString() {
        // Format as ISO 8601 date (YYYY-MM-DD)
        return String.format("%04d-%02d-%02d", this.year, this.month, this.day);
    }
}
