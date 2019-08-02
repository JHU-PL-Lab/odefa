package boomerang.example;

import java.util.function.Function;

public class Loop2_1 {

    public static void main(String[] args) {

        Lp1 lp1 = new Lp1();
        Integer ten = new Integer(10);
        Integer zero = new Integer(0);
        Integer result = lp1.apply(ten, zero);

        System.out.println(result);
        queryFor(lp1);
        queryFor(ten);
        queryFor(zero);
        queryFor(result);

    }

    @FunctionalInterface
    interface TriFunction<P1, P2, P3, R> {
        public R apply(P1 param1, P2 param2, P3 param3);
    }

    private static class Lp1 {

        public Integer apply(Integer i, Integer x) {
            if (i == 0) {
                return x;
            }
            else {
                TriFunction<Integer, Function<Integer, Integer>, Integer, Integer>
                        lp2 = new TriFunction<Integer, Function<Integer, Integer>, Integer, Integer>() {
                    public Integer apply(Integer j, Function<Integer, Integer> f, Integer y) {
                        if (j == 0) {
                            return Lp1.this.apply(i-1, y);
                        }
                        {
                            return this.apply(j-1, f, (f.apply(y)));
                        }
                    }
                };
                Integer ten = new Integer(10);
                Function<Integer, Integer> f1 = new Function<Integer, Integer>() {

                    public Integer apply(Integer n) {
                        return new Integer(n+i);
                    }
                };
                return lp2.apply(ten, f1, x);
            }
        }

    }

    private static <T> void queryFor(T query) {

    }
}
