package catala.runtime;

public class Test {

    public static void main(String[] args) {
        F f = new F();
        System.out.println(f.apply(new F.F_in(5)));
    }
}
