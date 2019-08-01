package boomerang.example;

import java.util.function.Function;

public class Cpstak {

    public static void main(String[] args) {

        CpstakFun cpstakFun = new CpstakFun();
        Tak tak = new Tak();
        tak.tak = tak;
        cpstakFun.tak = tak;
        Integer result = cpstakFun.apply(new Integer(2), new Integer(3), new Integer(4));
        System.out.println(result);

        queryFor(cpstakFun);
        queryFor(tak);
        queryFor(result);

    }

    private static class CpstakFun {
        Tak tak;
        public Integer apply(Integer x, Integer y, Integer z) {
            Function<Integer, Integer> idFun = new Function<Integer, Integer>() {
                public Integer apply(Integer elm) {
                    return elm;
                }
            };
            return tak.apply(x, y, z, idFun);
        }
    }

    private static class Tak {

        public static Tak tak;

        public Integer apply(Integer x, Integer y, Integer z, Function<Integer, Integer> k) {
            if (!(y < x)) {
                return k.apply(z);
            }
            else {
                Function<Integer, Integer> fun1 = new Function<Integer, Integer>() {
                    public Integer apply(Integer v1) {
                        Function<Integer, Integer> fun2 = new Function<Integer, Integer>() {
                            public Integer apply(Integer v2) {
                                Function<Integer, Integer> fun3 = new Function<Integer, Integer>() {
                                    public Integer apply(Integer v3) {
                                        return tak.apply(v1, v2, v3, k);
                                    }
                                };
                                return tak.apply(z - 1, x, y, fun3);
                            }
                        };
                        return tak.apply(y - 1, z, x, fun2);
                    }
                };
                return tak.apply(x-1, y, z, fun1);
            }
        }
    }


    private static <T> void queryFor(T query) {

    }
}
