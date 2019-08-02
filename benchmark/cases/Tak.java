package boomerang.example;

public class Tak {

    public static void main(String[] args) {

        TakFun takFun = new TakFun();
        Integer tt = new Integer(32);
        Integer ft = new Integer(15);
        Integer eight = new Integer(8);
        Integer result = takFun.apply(tt, ft, eight);

        System.out.println(result);

        queryFor(takFun);
        queryFor(tt);
        queryFor(ft);
        queryFor(eight);
        queryFor(result);
    }

    private static class TakFun {

        public Integer apply(Integer x, Integer y, Integer z) {
            if (!(y < x)) {
                return z;
            }
            else {
                return this.apply(this.apply(x-1, y, z), this.apply(y-1, z,x), this.apply(z-1, x, y));
            }
        }
    }

    private static <T> void queryFor(T query) {

    }
}
