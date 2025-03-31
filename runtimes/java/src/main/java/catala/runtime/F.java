package catala.runtime;

import java.util.function.Function;

public class F implements Function<F.F_in, F.F_out> {

    public record F_in(Integer x) {

    };

    public record F_out(Integer x) {

    };

    @Override
    public F_out apply(F_in t) {
        return new F_out(t.x);
    }

}
