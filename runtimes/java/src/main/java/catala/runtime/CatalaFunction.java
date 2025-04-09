package catala.runtime;

import java.util.function.Function;

public interface CatalaFunction<Tin extends CatalaValue, Tout extends CatalaValue>
        extends CatalaValue, Function<Tin, Tout> {

    @Override
    default CatalaBool equalsTo(CatalaValue v) {
        // Functions are not comparable
        return CatalaBool.FALSE;
    }
}
