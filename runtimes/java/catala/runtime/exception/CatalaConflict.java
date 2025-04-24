package catala.runtime.exception;

import java.util.List;
import java.util.stream.Stream;

import catala.runtime.CatalaArray;
import catala.runtime.CatalaOption;
import catala.runtime.CatalaPosition;
import catala.runtime.CatalaTuple;

public class CatalaConflict {

    @SuppressWarnings("unchecked")
    public static CatalaOption<CatalaTuple> handleExceptions(CatalaArray<CatalaOption<CatalaTuple>> v) {
        List<CatalaOption<CatalaTuple>> active_exns
                = (Stream.of(v.asArray())
                        .filter(exn -> exn.isSome()).toList());
        int len = active_exns.size();
        switch (len) {
            case 0:
                return CatalaOption.NONE;
            case 1:
                return active_exns.get(0);
            default:
                List<CatalaPosition> lpos
                        = active_exns.stream().map(p_opt -> (p_opt.get()).get(1, CatalaPosition.class))
                                .toList();
                throw new CatalaError(CatalaError.Error.Conflict, lpos);
        }
    }
}
