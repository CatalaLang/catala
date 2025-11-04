import catala.runtime.*;
import catala.runtime.exception.*;

public class List_internal {

    public static class Globals {

        public static final CatalaFunction<CatalaTuple,CatalaArray<CatalaInteger>> sequence =
            tup_arg -> {
            CatalaInteger begin = CatalaValue.<CatalaInteger>cast(tup_arg.get(0));
            CatalaInteger end = CatalaValue.<CatalaInteger>cast(tup_arg.get(1));
            int len = end.subtract(begin).asBigInteger().intValue();
            CatalaInteger[] values = new CatalaInteger[len > 0 ? len : 0];
            for (int i = 0; i < len; i++)
                values[i] = begin.add(new CatalaInteger(i));
            return new CatalaArray<CatalaInteger>(values);
        };

        public static final CatalaFunction<CatalaTuple,CatalaOption<CatalaValue>> nthElement =
            tup_arg -> {
            CatalaArray<CatalaValue> lst =
                CatalaValue.<CatalaArray<CatalaValue>>cast(tup_arg.get(0));
            int n =
                CatalaValue.<CatalaInteger>cast(tup_arg.get(1)).asBigInteger().intValue() - 1;
            if (0 <= n && n < lst.length().asBigInteger().intValue())
                return CatalaOption.some(lst.get(n));
            else
                return CatalaOption.none();
        };

        public static final CatalaFunction<CatalaTuple,CatalaArray<? extends CatalaValue>> removeNthElement =
            tup_arg -> {
            CatalaArray<CatalaValue> lst =
                CatalaValue.<CatalaArray<CatalaValue>>cast(tup_arg.get(0));
            int n =
                CatalaValue.<CatalaInteger>cast(tup_arg.get(1)).asBigInteger().intValue() - 1;
            int len = lst.length().asBigInteger().intValue();
            if (0 <= n && n < len) {
                CatalaValue[] values = new CatalaValue[len - 1];
                for (int i = 0; i < n; i++) values[i] = lst.get(i);
                for (int i = n+1; i < len; i++) values[i-1] = lst.get(i);
                return new CatalaArray<CatalaValue>(values);
            } else {
                return lst;
            }
        };

        public static final CatalaFunction<CatalaArray<? extends CatalaValue>,CatalaArray<CatalaValue>> reverse =
            lst -> {
            CatalaValue[] lst_arr = lst.asArray();
            int len = lst_arr.length;
            if (len == 0)
                return CatalaValue.<CatalaArray<CatalaValue>>cast(lst);
            CatalaValue[] ret = java.util.Arrays.copyOf(lst_arr, len);
            for (int i = 0; i < len; i++){
                ret[i] = lst_arr[len - 1 - i];
            }
            return new CatalaArray<CatalaValue>(ret);
        };
    }
}
