package catala.runtime.exception;

import java.util.List;
import java.util.stream.Stream;

import catala.runtime.CatalaArray;
import catala.runtime.CatalaOption;
import catala.runtime.CatalaTuple;
import catala.runtime.SourcePosition;

public class ConflictException extends CatalaException {

    public ConflictException(String message, Throwable t) {
        super(message, t);
    }

    public ConflictException(String message) {
        super(message);
    }

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
                StringBuilder msg = new StringBuilder("two or more concurring valid computations: ");
                for (CatalaOption<CatalaTuple> p_opt : active_exns) {
                    SourcePosition pos = (SourcePosition) (p_opt.get()).get(1);
                    msg.append("\n");
                    msg.append(pos.toString());
                }
                throw new CatalaException(msg.toString());
        }
    }
}
