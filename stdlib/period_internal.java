/* This is a template file following the expected interface and declarations to
 * implement the corresponding Catala module.
 *
 * You should replace all `Error.Impossible` place-holders with your
 * implementation and rename it to remove the ".template" suffix. */

import catala.runtime.*;
import catala.runtime.exception.*;

public class Period_internal {
    
    public static class Globals {
        
        public static final CatalaFunction<CatalaArray<CatalaTuple>,CatalaArray<CatalaTuple>> sort =
            p -> {
            final CatalaArray<CatalaTuple> sort__1;
            CatalaPosition pos =
                new CatalaPosition
                 ("stdlib/period_internal.catala_en", 6, 13, 6, 17,
                  new String[]{});
            throw new CatalaError(CatalaError.Error.Impossible, pos); };
        
        public static final CatalaFunction<CatalaTuple,CatalaArray<CatalaTuple>> splitByMonth =
            p -> {
            final CatalaArray<CatalaTuple> splitByMonth__1;
            CatalaPosition pos =
                new CatalaPosition
                 ("stdlib/period_internal.catala_en", 9, 13, 9, 27,
                  new String[]{});
            throw new CatalaError(CatalaError.Error.Impossible, pos); };
        
        public static final CatalaFunction<CatalaTuple,CatalaArray<CatalaTuple>> splitByYear =
            tup_arg_11 -> {
            CatalaInteger startMonth = CatalaValue.<CatalaInteger>cast
               (tup_arg_11.get(0));
            CatalaTuple p = CatalaValue.<CatalaTuple>cast(tup_arg_11.get(1));
            final CatalaArray<CatalaTuple> splitByYear__1;
            CatalaPosition pos =
                new CatalaPosition
                 ("stdlib/period_internal.catala_en", 12, 13, 12, 26,
                  new String[]{});
            throw new CatalaError(CatalaError.Error.Impossible, pos); };
    }
    
}
