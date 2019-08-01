package boomerang.example;

public class Ack {

    public static void main(String[] args) {

        Integer three = new Integer(3);
        Integer twelve = new Integer(12);
        AckFun ackFun = new AckFun();
        Integer result = ackFun.apply(three, twelve);

        System.out.println(result);

        queryFor(three);
        queryFor(twelve);
        queryFor(ackFun);
        queryFor(result);
    }

    private static class AckFun {

        public Integer apply(Integer m, Integer n) {
            if (m == 0) {
                return new Integer(n + 1);
            }
            else if (n == 0) {
                return apply(m - 1, 1);
            }
            else {
                return apply(m - 1, apply(m, n - 1));
            }
        }
    }

    private static <T> void queryFor(T query) {

    }
}
