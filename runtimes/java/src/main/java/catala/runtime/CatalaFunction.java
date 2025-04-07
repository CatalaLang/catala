package catala.runtime;

import java.util.function.Function;

public interface CatalaFunction<Tin extends CatalaValue, Tout extends CatalaValue>
    extends CatalaValue, Function<Tin, Tout> {}
