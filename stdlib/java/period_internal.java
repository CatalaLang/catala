import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;

import catala.dates_calc.Date;
import catala.runtime.CatalaArray;
import catala.runtime.CatalaDate;
import catala.runtime.CatalaDuration;
import catala.runtime.CatalaFunction;
import catala.runtime.CatalaInteger;
import catala.runtime.CatalaTuple;
import catala.runtime.CatalaValue;

class PeriodAssocComparator implements Comparator<CatalaTuple> {
    @Override
    public int compare(CatalaTuple assc1, CatalaTuple assc2) {
        CatalaTuple t1 = CatalaValue.<CatalaTuple>cast(assc1.get(0));
        CatalaTuple t2 = CatalaValue.<CatalaTuple>cast(assc2.get(0));
        CatalaDate d1 = t1.get(0, CatalaDate.class);
        CatalaDate d2 = t2.get(0, CatalaDate.class);
        return d1.compareTo(d2);
    }
}


public class Period_internal {

    public static class Globals {

        public static final CatalaFunction<CatalaArray<CatalaTuple>,CatalaArray<CatalaTuple>> sort =
            p -> {
                CatalaValue[] arr = p.asArray();
                CatalaTuple[] sorted = new CatalaTuple[arr.length];
                System.arraycopy(arr, 0, sorted, 0, arr.length);
                Arrays.sort(sorted, 0, sorted.length, new PeriodAssocComparator());
                return new CatalaArray<>(sorted);
            };

        /**
         * Splits the given period, returning one period per calendar month. 
         * The first and last elements may be non-whole months.
         * Edge-case: if the given period is empty (begin >= end), an empty list
         * is returned.
         */
        public static final CatalaFunction<CatalaTuple,CatalaArray<CatalaTuple>> splitByMonth =
            p -> {
                CatalaDate cur = p.get(0, CatalaDate.class);
                CatalaDate end = p.get(1, CatalaDate.class);

                List<CatalaTuple> res = new ArrayList();

                while(cur.compareTo(end) < 0){
                   CatalaDate fdmNext = cur.getFirstDayOfMonth().addDurationAbortOnRound(null, CatalaDuration.of(0, 1, 0));
                   int cmp = end.compareTo(fdmNext);
                   CatalaDate cut = cmp <= 0 ? end : fdmNext;
                   res.add(new CatalaTuple(cur, cut));
                   cur = cut;
                }

                return new CatalaArray<>(res.toArray(CatalaTuple[]::new));
            };

        /** 
         * Splits the given period, returning one period per year, split on the first
         * of the given month. The first and last elements returned may be non-whole
         * years.
         */
        public static final CatalaFunction<CatalaTuple,CatalaArray<CatalaTuple>> splitByYear =
            tup_arg_11 -> {
              CatalaInteger startMonth = CatalaValue.<CatalaInteger>cast(tup_arg_11.get(0));
              CatalaTuple p = CatalaValue.<CatalaTuple>cast(tup_arg_11.get(1));

              CatalaDate cur = p.get(0, CatalaDate.class);
              CatalaDate end = p.get(1, CatalaDate.class);

              List<CatalaTuple> res = new ArrayList();

              while(cur.compareTo(end) < 0){
                CatalaDate fdyNext = firstDayOfNextRollingYear(startMonth.asBigInteger().intValueExact(), end);
                
                int cmp = end.compareTo(fdyNext);
                CatalaDate cut = cmp <= 0 ? end : fdyNext;
                res.add(new CatalaTuple(cur, cut));
                cur = cut;
              }

              return new CatalaArray<>(res.toArray(CatalaTuple[]::new));
            };
    }

    private static CatalaDate firstDayOfNextRollingYear(int startMonth, CatalaDate date){
      assert((startMonth >= 1) && (startMonth <= 12));
      CatalaInteger yr = date.getMonth().asBigInteger().intValueExact() < startMonth ? date.getYear() : date.getYear().add(new CatalaInteger(1));
      return new CatalaDate(Date.of(yr.asBigInteger().intValueExact(), startMonth, 1));
    }

}
