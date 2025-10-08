/* This is a template file following the expected interface and declarations to
 * implement the corresponding Catala module.
 *
 * You should replace all `Error.Impossible` place-holders with your
 * implementation and rename it to remove the ".template" suffix. */

import catala.runtime.*;
import catala.runtime.exception.*;

public class List_internal {
    
    public static class Globals {
        
        public static final CatalaFunction<CatalaTuple,CatalaArray<CatalaInteger>> sequence =
            tup_arg_11 -> {
            CatalaInteger begin = CatalaValue.<CatalaInteger>cast
               (tup_arg_11.get(0));
            CatalaInteger end = CatalaValue.<CatalaInteger>cast
               (tup_arg_11.get(1));
            final CatalaArray<CatalaInteger> sequence__1;
            CatalaPosition pos =
                new CatalaPosition
                 ("stdlib/list_internal.catala_en", 4, 13, 4, 21,
                  new String[]{});
            throw new CatalaError(CatalaError.Error.Impossible, pos); };
        
        static final CatalaFunction<CatalaUnit,CatalaFunction<CatalaTuple,CatalaOption<CatalaValue>>> nthElementInit =
            unit -> {
            final CatalaFunction<CatalaTuple,CatalaOption<CatalaValue>>
                nthElement__1;
            CatalaPosition pos =
                new CatalaPosition
                 ("stdlib/list_internal.catala_en", 9, 13, 9, 24,
                  new String[]{});
            throw new CatalaError(CatalaError.Error.Impossible, pos); };
        
        public static final CatalaFunction<CatalaTuple,CatalaOption<CatalaValue>> nthElement =
            nthElementInit.apply(CatalaUnit.INSTANCE);
        
        static final CatalaFunction<CatalaUnit,CatalaFunction<CatalaTuple,CatalaArray<CatalaValue>>> removeNthElementInit =
            unit -> {
            final CatalaFunction<CatalaTuple,CatalaArray<CatalaValue>>
                removeNthElement__1;
            CatalaPosition pos =
                new CatalaPosition
                 ("stdlib/list_internal.catala_en", 14, 13, 14, 31,
                  new String[]{});
            throw new CatalaError(CatalaError.Error.Impossible, pos); };
        
        public static final CatalaFunction<CatalaTuple,CatalaArray<CatalaValue>> removeNthElement =
            removeNthElementInit.apply(CatalaUnit.INSTANCE);
    }
    
}
