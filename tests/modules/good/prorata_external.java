import catala.runtime.*;
import java.util.*;

public class Prorata_external {

    public static class Globals {

        private static final CatalaPosition dummy_position = new CatalaPosition("src/periodes.java", 0, 0, 0, 0, new String[]{});

        public static final CatalaFunction<CatalaTuple, CatalaArray<CatalaMoney>> prorata
                = x -> {
                    CatalaMoney amount = x.get(0, CatalaMoney.class);
                    CatalaValue[] ws = x.get(1, CatalaArray.class).asArray();
                    CatalaMoney[] weights = Arrays.copyOf(ws, ws.length, CatalaMoney[].class);
                    CatalaMoney w_total = Arrays.stream(weights)
                            .reduce(CatalaMoney.ofCents(0), (acc, y) -> acc.add(y)
                            );
                    CatalaMoney rem = amount;
                    CatalaMoney[] new_weights = Arrays.copyOf(weights, weights.length);
                    for (int i = 0; i < weights.length; i++) {
                        CatalaMoney w = weights[i];
                        CatalaMoney r = amount.multiply(
                                w.divide(dummy_position, w_total));
                        rem = rem.subtract(r);
                        new_weights[i] = r;
                    }
                    new_weights[weights.length - 1] = new_weights[weights.length - 1].add(rem);
                    return new CatalaArray(new_weights);
                };

        public static final CatalaFunction<CatalaTuple, CatalaArray<CatalaMoney>> prorata2
                = x -> {
                    CatalaMoney amount = x.get(0, CatalaMoney.class);
                    CatalaValue[] ws = x.get(1, CatalaArray.class).asArray();
                    CatalaMoney[] weights = Arrays.copyOf(ws, ws.length, CatalaMoney[].class);
                    CatalaMoney w_total = Arrays.stream(weights)
                            .reduce(CatalaMoney.ofCents(0), (acc, y) -> acc.add(y)
                            );
                    CatalaMoney rem_amount = amount;
                    CatalaMoney rem_weights = w_total;
                    CatalaMoney[] new_weights = Arrays.copyOf(weights, weights.length);
                    for (int i = 0; i < weights.length; i++) {
                        CatalaMoney w = weights[i];
                        CatalaMoney r = rem_amount.multiply(
                                w.divide(dummy_position, rem_weights));
                        rem_amount = rem_amount.subtract(r);
                        rem_weights = rem_weights.subtract(w);
                        new_weights[i] = r;
                    }
                    return new CatalaArray(new_weights);
                };
    }

}
