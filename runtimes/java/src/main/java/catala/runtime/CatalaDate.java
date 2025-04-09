package catala.runtime;

import java.time.LocalDate;

import catala.runtime.exception.CatalaException;

public final class CatalaDate implements CatalaValue, Comparable<CatalaDate> {

    private final LocalDate date;

    public CatalaDate(LocalDate date) {
        this.date = date;
    }

    public static CatalaDate of(int year, int month, int day) {
        return new CatalaDate(LocalDate.of(year, month, day));
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
        return new CatalaDate(LocalDate.parse(catalaDateLiteral.subSequence(1, 11)));
    }

    public final CatalaInteger getYear(){
        return new CatalaInteger(this.date.getYear());
    }

    public final CatalaInteger getMonth(){
        return new CatalaInteger(this.date.getMonthValue());
    }

    public final CatalaInteger getDay(){
        return new CatalaInteger(this.date.getDayOfMonth());
    }

    public final CatalaDate firstDayOfMonth() {
        return new CatalaDate(LocalDate.of(this.date.getYear(), this.date.getMonth(), 1));
    }

    public final CatalaDate lastDayOfMonth() {
        return new CatalaDate(LocalDate.of(this.date.getYear(), this.date.getMonth(), this.date.lengthOfMonth()));
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

    public CatalaDate addDurationAbortOnRound(SourcePosition pos, CatalaDuration dur) {
        throw new CatalaException("addDurationAbortOnRound not implemented yet");
    }

    public CatalaDate addDurationRoundUp(SourcePosition pos, CatalaDuration dur){
        throw new CatalaException("addDurationRoundUp not implemented yet");
    }

    public CatalaDuration subtract(CatalaDate dur) {
        throw new CatalaException("subtract not implemented yet");
    }

    @Override
    public CatalaBool equalsTo(CatalaValue v) {
        if (v instanceof CatalaDate catalaDate) {
            return CatalaBool.fromBoolean(this.compareTo(catalaDate) == 0);
        } else {
            return CatalaBool.FALSE;
        }
    }
}
