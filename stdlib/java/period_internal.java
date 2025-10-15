import catala.runtime.*;
import catala.runtime.exception.*;

class PeriodAssocComparator implements Comparator<CatalaTuple> {
    @Override
    public int compare(CatalaTuple assc1, CatalaTuple assc2) {
        CatalaDate d1 = CatalaValue.<CatalaDate>cast(assc1.get(0));
        CatalaDate d2 = CatalaValue.<CatalaDate>cast(assc1.get(1));
        return d1.compareTo(d2);
    }
}


public class Period_internal {

    public static class Globals {

        public static final CatalaFunction<CatalaArray<CatalaTuple>,CatalaArray<CatalaTuple>> sort =
            p -> {
            CatalaArray<CatalaTuple> 
            p.sort(new PeriodAssocComparator())
            final CatalaArray<CatalaTuple> sort__1;
            CatalaPosition pos =
                new CatalaPosition
                 ("stdlib/period_internal.catala_en", 6, 13, 6, 17, new String[]{});
            throw new CatalaError(CatalaError.Error.Impossible, pos); };

        public static final CatalaFunction<CatalaTuple,CatalaArray<CatalaTuple>> splitByMonth =
            p -> {
            final CatalaArray<CatalaTuple> splitByMonth__1;
            CatalaPosition pos =
                new CatalaPosition
                 ("stdlib/period_internal.catala_en", 10, 13, 10, 27, new String[]{});
            throw new CatalaError(CatalaError.Error.Impossible, pos); };

        public static final CatalaFunction<CatalaTuple,CatalaArray<CatalaTuple>> splitByYear =
            tup_arg_11 -> {
            CatalaInteger startMonth = CatalaValue.<CatalaInteger>cast(tup_arg_11.get(0));
            CatalaTuple p = CatalaValue.<CatalaTuple>cast(tup_arg_11.get(1));
            final CatalaArray<CatalaTuple> splitByYear__1;
            CatalaPosition pos =
                new CatalaPosition
                 ("stdlib/period_internal.catala_en", 14, 13, 14, 26, new String[]{});
            throw new CatalaError(CatalaError.Error.Impossible, pos); };
    }

}
